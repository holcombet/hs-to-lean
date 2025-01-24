{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

{-
Module for processing the ghc-lib-parser AST into the intermediate ADT structure.
-}

module HsToLean.ProcessFile (generateIntermediateAST) where 

import GHC
import GHC.Maybe
import "ghc" GHC.Parser
import "ghc" GHC.Utils.Outputable
import Data.Generics (gshow)
import GHC.Paths
import System.Environment ( getArgs )
import System.FilePath (takeBaseName)
import System.IO (writeFile)
import "ghc" GHC.Driver.Session -- ( defaultFatalMessager, defaultFlushOut )
import Control.Monad.IO.Class
import "ghc" GHC.Driver.Phases (Phase(Cpp))
import "ghc" GHC.Types.SourceFile (HscSource(HsSrcFile))
import "ghc" GHC.Unit.Module
import "ghc" GHC.Plugins ( occNameString, HasOccName(occName), isBoxed, rdrNameOcc)
import Data.String (String)
import Data.List (intercalate, isPrefixOf, tails, findIndex, transpose)
import Data.List.Split (splitOn)
import "ghc" GHC.Types.SourceText
import Data.Void
import GHC.Runtime.Eval (Term(ty))
import "ghc" GHC.Data.FastString (unpackFS)
import Data.Bool (Bool)
import "ghc" GHC.Data.Bag (bagToList, Bag)
import "ghc" GHC.Hs.Binds
import Data.Ratio ((%))
import GHC.Base (String)
import Data.Kind (FUN)
import System.IO

import TestAST



---------------------------------------------------------------------
{- 
sortDeclList, getFirstElement, and prependConstructor are helper functions for 
sorting the HsDecls list after their translation into the intermediate AST. 

    NOTE: HsDecl is a list of Haskell Declarations (TyClD, DerivD, ValD, SigD, etc.).
          We made the intermediate AST because we want to link type TypeSig with FunBind 
          to extract the parameter names. 

sortDeclList sorts the list of HsDecls (or, in our case, Decls) so that 

-}

sortDeclList :: [String] -> [String]
sortDeclList [] = []
sortDeclList [x] = [x]
sortDeclList (x : y : xs) = 
    let l = words x
        isTypeSig = getFirstElement l == "TySig" 
    in if isTypeSig then ("(" ++ x ++ " (" ++ y ++ " ))") : sortDeclList xs else ("(" ++ x ++ ")") : sortDeclList (y: xs)
    

getFirstElement :: [String] -> String
getFirstElement [] = ""
getFirstElement (x : _ ) = x

prependConstructor :: String -> [String] -> [String]
prependConstructor prefix = map (prefix ++) 


---------------------------------------------------------------------

{-
Functions for empty constructors as place holders
-}

emptyPatts :: Patts
emptyPatts = VariPatt "Patts Not Implemented" 

emptyMatchPair :: MatchPair 
emptyMatchPair = MP {bound_var = [], guard_body = EmptyG}


-- emptyAST :: ASt 
-- emptyAST = SignatureD (TyClassD {name = ""})

{-
constructor checkers
-}


---------------------------------------------------------------------

{-
Functions for checking characteristics of objects
-}

isVarPatt :: Patts -> Bool 
isVarPatt = \case 
    VariPatt x -> True 
    _ -> False 

isParPatt :: Patts -> Bool 
isParPatt = \case 
    ParPatt x -> True 
    _ -> False 


isConPattInfix :: Patts -> Bool 
isConPattInfix = \case
    ConPatt _ details -> case details of 
        ConPattInfix x y -> True 
        _ -> False 
    ParPatt p -> isConPattInfix p
    _ -> False




isEmptyLocBinds :: LocBinds -> Bool 
isEmptyLocBinds = \case 
    EmptyLocBinds -> True 
    _ -> False 


getConPattTail :: Patts -> VarPatt 
getConPattTail = \case 
    ConPatt ty det -> case det of 
        ConPattInfix _ x ->  x 
        _ -> "" 
    ParPatt p -> getConPattTail p
    _ -> ""

    
getVarPattFromPatts :: Patts -> VarPatt
getVarPattFromPatts = \case 
    VariPatt x -> x 
    _ -> ""


isFunType :: Types -> Bool 
isFunType = \case 
    FType f -> True 
    _ -> False
---------------------------------------------------------------------
---------------------------------------------------------------------

{- 
Trigger function (makes ghc-lib-parser ast, starts translating to 
                  intermediate ast)
-}

generateIntermediateAST :: ParsedSource -> IO()
generateIntermediateAST ast = do 
    let ps = map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ ast 
        interDecls = map intermediateDecls ps 
        -- sortedDeclList = sortDeclList interDecls 
    liftIO $ writeFile "IntermediateAST.txt" ("[" ++ (intercalate ",\n" $ showIntermediateAST interDecls) ++ "]")

showIntermediateAST :: [AST] -> [String]
showIntermediateAST = map show 

---------------------------------------------------------------------
---------------------------------------------------------------------

{-
Starting translation from ghc-lib-parser AST to intermediate AST
-}

---------------------------------------------------------------------
---------------------------------------------------------------------

{-
HsDecls to AST (intermediateDecls)

Translates HsDecls to AST objects
-}

intermediateDecls :: HsDecl GhcPs -> AST 
intermediateDecls = \case 
    ValD _ decl -> intermediateHsBind decl
    SigD _ decl -> intermediateSig decl
    TyClD _ decl -> intermediateTyClDecl decl 
    _ -> EmptyD

---------------------------------------------------------------------
---------------------------------------------------------------------

{-
Sig to SignatureD (intermediateSig)

translates Sig to SignatureD
-}

{- translating Sig to SignatureD-}
intermediateSig :: Sig GhcPs -> AST 
intermediateSig decl = SignatureD (intermediateSigToSigs decl)             --case decl of 
    -- TypeSig _ names typ -> 
    --     -- let typeName = unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names)
    --     --     sigTypes = intermediateSigType typ
    --     -- in SignatureD (TySig {ty_name = typeName, qual_ty = [], fun_type = sigTypes, fun_bind = EmptyB })
    -- _ -> EmptyD


intermediateSigToSigs :: Sig GhcPs -> Sigs 
intermediateSigToSigs decl = case decl of 
    TypeSig _ names typ -> 
        let typeName = unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names)
            sigTypes = intermediateSigType typ 
        in TySig {ty_name = typeName, qual_ty = [], fun_type = sigTypes, fun_bind = EmptyB}
    _ -> EmptySig

intermediateSigType :: LHsSigWcType GhcPs -> [Types]
intermediateSigType = \case 
    HsWC ext body -> intermediateHsSigType (unXRec @(GhcPass 'Parsed) body)

intermediateHsSigType :: HsSigType GhcPs -> [Types]
intermediateHsSigType = \case 
    HsSig ext bndrs body -> 
        let tys = intermediateTypes (unLoc body)
            ls = removeFunTy tys
            -- l = removeAllFunTy ls
        in ls



removeFunTy :: Types -> [Types]
removeFunTy = \case 
    FType typ -> typ 
    TypeVar t -> [TypeVar t]
    ListTy l -> [ListTy l]
    AppTy f x-> [AppTy f x]
    ExpListTy l -> [ExpListTy l]
    FunVar f -> [FunVar f]


intermediateTypes :: HsType GhcPs -> Types 
intermediateTypes = \case 
    HsFunTy _ _ arg1 arg2 -> 
        let typList = (unXRec @(GhcPass 'Parsed) arg1) : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
        in FType (map intermediateTypes typList)
    HsTyVar _ _ typ -> --TypeVar (occNameString . occName . unLoc $ typ)
        let var = (occNameString . occName . unLoc $ typ)
        in intermediateDecideTypes var
    HsAppTy _ typ1 typ2 -> AppTy (intermediateTypes $ unLoc typ1) (intermediateTypes $ unLoc typ2)
    HsExplicitListTy _ _ ls -> ExpListTy (map intermediateTypes (map unLoc ls))
    HsListTy _ typ -> ListTy (intermediateTypes (unLoc typ))
    HsParTy _ typ -> ParaTy (intermediateTypes (unLoc typ))
    HsTyLit _ t -> TypeVar "TyLit"
    _ -> EmptyT


{-
intermediateDecideTypes

Function that translates Haskell functions/monads to Lean equivalent if names are not the same
(does not inlcude types)
-}
intermediateDecideTypes :: String -> Types 
intermediateDecideTypes x 
    | x == "Rational" = FunVar LRational
    | x == "Either" = FunVar LEither 
    -- | x == "Left" = FunVar LLeft 
    -- | x == "Right" = FunVar LRight 
    | x == "Maybe" = FunVar LMaybe 
    -- | x == "Just" = FunVar LJust 
    -- | x == "Nothing" = FunVar LNothing 
    | x == "a" = FunVar LAlphaA
    | otherwise = TypeVar x 

extractArgs :: HsType GhcPs -> [HsType GhcPs]
extractArgs (HsFunTy _ _ arg1 arg2) = unXRec @(GhcPass 'Parsed) arg1 : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
extractArgs x = [x]
---------------------------------------------------------------------
---------------------------------------------------------------------

{-
HsBind to ValueD (intermediateHsBind)

translates HsBind objects into intermediate ValueD objects
-}
intermediateHsBind :: HsBind GhcPs -> AST 
intermediateHsBind decl = ValueD (intermediateHsBindToBinds decl)
-- intermediateHsBind decl = case decl of 
--     FunBind _ name matches _ -> 
--         let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name 
--             matchPs = map (\(L _ (Match _ _ pats body)) -> 
--                 let boundVar = map intermediatePatts pats       
--                     bodyExpr = intermediateGRHSs body       
--                 in MP {bound_var = boundVar, guard_body = bodyExpr} ) (unLoc $ mg_alts matches)
            
--             matchLPats = map (\(L _ (Match _ _ pats _)) -> pats) (unLoc $ mg_alts matches)

--             boundVar = filterValidPatt $ boundVarToPatt$ transposeBoundVar matchLPats

--         in ValueD (FBind {fun_name = funName, patt_args = boundVar, matches = matchPs})
--     _ -> EmptyD


{-

-}

intermediateHsBindToBinds :: HsBind GhcPs -> Binds 
intermediateHsBindToBinds decl = case decl of 
    FunBind _ name matches _ -> 
        let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name 
            matchPs = map (\(L _ (Match _ _ pats body)) -> 
                let boundVar = map intermediatePatts pats       
                    bodyExpr = intermediateGRHSs body       
                in MP {bound_var = boundVar, guard_body = bodyExpr} ) (unLoc $ mg_alts matches)
            
            matchLPats = map (\(L _ (Match _ _ pats _)) -> pats) (unLoc $ mg_alts matches)

            boundVar = filterValidPatt $ boundVarToPatt$ transposeBoundVar matchLPats

        in FBind {fun_name = funName, patt_args = boundVar, matches = matchPs}
    _ -> EmptyB


{-
transposeBoundVar, boundVarToPatt, filterValidPatt, and chooseVar are
functions for generating the appropriate info for patt_args in FBind
-}
transposeBoundVar :: [[LPat GhcPs]] -> [[LPat GhcPs]]
transposeBoundVar = transpose 

boundVarToPatt :: [[LPat GhcPs]] -> [[Patts]]
boundVarToPatt [] = []
boundVarToPatt (x : xs) = 
    let opt = findValidPattNames (map intermediatePatts x)
    in opt : boundVarToPatt xs

filterValidPatt :: [[Patts]] -> [VarPatt]
filterValidPatt xs = map chooseVar xs 



chooseVar :: [Patts] -> VarPatt 
chooseVar [] = ""
chooseVar (x : xs) 
    | isVarPatt x = getVarPattFromPatts x
    | isConPattInfix x = getConPattTail x 
    | otherwise = chooseVar xs 


findValidPattNames :: [Patts] -> [Patts]
findValidPattNames [] = []
findValidPattNames (x : xs) = 
    let name = if isVarPatt x then x else 
                    if isConPattInfix x then x
                    else EmptyP
    in name : findValidPattNames xs
    
     

---------------------------------------------------------------------
---------------------------------------------------------------------


{-
Guards (GRHSs, GRHS, etc...) for function body (and other things)
-}

intermediateGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> GuardRHSs
intermediateGRHSs (GRHSs _ grhss binds) = 
    let sb = map intermediateGRHS grhss 
        bndr = EmptyLocBinds         -- TODO: implement LocalBinds
    in Guards {guard_exprs = sb, loc_binds = bndr}


intermediateGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> GuardRHS 
intermediateGRHS (L _ (GRHS _ guardStmt body)) = 
    let guardStmts = if null guardStmt then [] else intermediateGuardStmts guardStmt      -- implement Stmts 
        exprStr = intermediateLExpr body 
    in StmtBody {guard_stmt = guardStmts, guard_expr =  exprStr}


intermediateGuardStmts :: [GuardLStmt GhcPs] -> [Stmts]
intermediateGuardStmts stms = map intermediateGuardStmt stms 


intermediateGuardStmt :: GuardLStmt GhcPs -> Stmts 
intermediateGuardStmt (L _ stmt) = case stmt of 
    BodyStmt _ expr _ _ -> BodyStmts (intermediateLExpr expr)
    LetStmt _ binds -> EmptyS 
    BindStmt _ pat expr -> EmptyS 
    _ -> EmptyS


intermediateLocBinds :: HsLocalBinds GhcPs -> LocBinds 
intermediateLocBinds binds = case binds of 
    EmptyLocalBinds _ -> EmptyLocBinds 
    HsValBinds _ (ValBinds _ bindlst sigs) -> intermediateValBinds bindlst sigs
    _ -> EmptyLocBinds 

intermediateValBinds :: LHsBindsLR GhcPs GhcPs -> [LSig GhcPs] -> LocBinds 
intermediateValBinds binds sigs = 
    let bindList = bagToList binds 
        prettyBinds = map intermediateLHsBindLR bindList 
        prettySigs = map (intermediateSigToSigs . unLoc) sigs 
    in ValsBinds prettyBinds prettySigs

{-
let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name 
            matchPs = map (\(L _ (Match _ _ pats body)) -> 
                let boundVar = map intermediatePatts pats       
                    bodyExpr = intermediateGRHSs body       
                in MP {bound_var = boundVar, guard_body = bodyExpr} ) (unLoc $ mg_alts matches)
            
            matchLPats = map (\(L _ (Match _ _ pats _)) -> pats) (unLoc $ mg_alts matches)

            boundVar = filterValidPatt $ boundVarToPatt$ transposeBoundVar matchLPats
-}
intermediateLHsBindLR :: LHsBindLR GhcPs GhcPs -> Binds
intermediateLHsBindLR (L _ bind) = case bind of 
    FunBind _ id matches _ -> intermediateHsBindToBinds bind
    _ -> EmptyB
    --     let name = (occNameString . occName . unXRec @(GhcPass 'Parsed)) id 
    --         m = map(\(L _ (Match _ c pats body)) ->
    --             let bodyExpr = intermediateGRHSs body 
    --             in MP {bound_var = [], guard_body = bodyExpr}) (unLoc $ mg_alts matches)
    --     in FBinding name m 
    -- _ -> EmptyBinding 
        
---------------------------------------------------------------------
---------------------------------------------------------------------

{-
TyClassDecls Functions (TypeSyn and DataDecl)
-}

-- SynDecls {syn_name = "Name", qualTy_var = [], syn_body = TypeVar "String"}

intermediateTyClDecl :: TyClDecl GhcPs -> AST 
intermediateTyClDecl decl = case decl of 
    SynDecl _ name tyVar fix rhs ->
        let synName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name 
            tyVarS = intermediateQTyVars tyVar      -- TODO: implement QualTyVar
            rhsString = intermediateTypes (unLoc rhs)
        in TyClassD (SynDecls {syn_name = synName, qualTy_var = tyVarS, syn_body = rhsString})
    DataDecl _ name tyVar fix dataDef ->
        let dataName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name 
            tyVarStr = intermediateQTyVars tyVar 
            dataDeriv = getDataDerivClause dataDef
            dataDefnCons = getDataCons dataDef
            typeData = getDataDefnNewOrData dataDef 
        in TyClassD (DataDecls {defn_type = typeData, data_name = dataName, qualTy_var = tyVarStr, dataDefn_cons = dataDefnCons, deriv_clause = dataDeriv})
    _ -> EmptyD


--

intermediateQTyVars :: LHsQTyVars GhcPs -> QTyVar
intermediateQTyVars (HsQTvs _ tyVars) = map intermediateQTyVar tyVars 


intermediateQTyVar :: LHsTyVarBndr () GhcPs -> String 
intermediateQTyVar (L _ (UserTyVar _ _ (L _ name))) =  occNameString (occName name)

--
--

getDataDefnNewOrData :: HsDataDefn GhcPs -> NoD 
getDataDefnNewOrData (HsDataDefn _ nod _ _ _ _ _) = case nod of 
    NewType -> NewTy 
    DataType -> DataTy

getDataDerivClause :: HsDataDefn GhcPs -> [Types] 
getDataDerivClause (HsDataDefn _ nod contxt _ kind cons derv) =
    if not (null derv) then [] else [] 

--
--

getDataCons :: HsDataDefn GhcPs -> [DefnConsDetails]
getDataCons (HsDataDefn _ nod contxt _ kind cons derv) = map getConDecl cons 


getConDecl :: LConDecl GhcPs -> DefnConsDetails 
getConDecl (L _ conDecl) = case conDecl of 
    ConDeclH98 _ name _ _ _ detail _ -> 
        let n = (occNameString (occName ( unLoc name)))
            consDets = getConDetails (getConDeclH98Details detail)
        in DefnConsDetail n consDets

getConDetails :: HsConDetails Void (HsScaled GhcPs (LHsType GhcPs)) (Located [LConDeclField GhcPs]) -> ConsDetails 
getConDetails details = case details of 
    PrefixCon tyArgs arg -> map intermediateTypes (map unLoc (getLHsTypes arg))
    _ -> []


getLHsTypes :: [HsScaled GhcPs (LHsType GhcPs)] -> [LHsType GhcPs]
getLHsTypes = map getLHsType 

getLHsType :: HsScaled GhcPs (LHsType GhcPs) -> LHsType GhcPs 
getLHsType (HsScaled _ ty) = ty

getConDeclH98Details :: HsConDeclH98Details GhcPs -> HsConDetails Void (HsScaled GhcPs (LHsType GhcPs)) (Located [LConDeclField GhcPs])
getConDeclH98Details details = case details of 
    PrefixCon _ args -> PrefixCon [] args 
    InfixCon arg1 arg2 -> InfixCon arg1 arg2 
    RecCon (L _ fields) -> RecCon (L noSrcSpan fields)

---------------------------------------------------------------------
---------------------------------------------------------------------

{-
Exprs
-}

intermediateLExpr :: LHsExpr GhcPs -> Exprs 
intermediateLExpr expr = intermediateExpr (unXRec @(GhcPass 'Parsed) expr)

intermediateExpr :: HsExpr GhcPs -> Exprs 
intermediateExpr = \case 
    HsVar _ name -> -- Var (occNameString . occName . unLoc $ name)
        let n = (occNameString . occName . unLoc $ name)
        in intermediateDecideExpr n
    HsApp _ expr1 expr2 -> App (intermediateExpr (unLoc expr1)) (intermediateExpr (unLoc expr2))
    OpApp _ expr1 op expr2 -> OperApp (intermediateExpr (unLoc expr1)) (intermediateExpr (unLoc op)) (intermediateExpr (unLoc expr2))
    HsLet _ _ binds _ exp -> 
        let b = intermediateLocBinds binds 
            e = intermediateExpr (unLoc exp)
        in LetExpr b e
    ExplicitList _ exprs -> ExpList (map intermediateLExpr exprs)
    HsOverLit _ lit -> Litr (intermediateOverLits lit)
    HsLit _ lit -> Litr (intermediateLits lit)
    _ -> Other


intermediateDecideExpr :: String -> Exprs 
intermediateDecideExpr x
    | x == "putStrLn" = SpecialVar VPutStrLn
    | x == "putStr" = SpecialVar VPutStr
    | x == "Just" = SpecialVar VJust 
    | x == "Nothing" = SpecialVar VNothing
    | x == "Left" = SpecialVar VLeft
    | x == "Right" = SpecialVar VRight
    | otherwise = Var x



---------------------------------------------------------------------
---------------------------------------------------------------------

{-
Patts
-}

intermediatePatts :: LPat GhcPs -> Patts
intermediatePatts (L _ pat) = case pat of 
    VarPat _ typ -> VariPatt (occNameString . occName . unLoc $ typ)
    LitPat _ pat -> LitPatt (intermediateLits pat)
    NPat _ lit _ _ -> LitPatt (intermediateOverLits (unXRec @(GhcPass 'Parsed) lit))
    ListPat _ pats -> ListPatt (map intermediatePatts pats)
    ConPat _ (L _ name) details -> 
        let conName = occNameString . occName $ name 
            patDetails = intermediateConPatDetails details 
        in case details of 
            PrefixCon _ _ -> ConPatt {con_type = conName, patt_details = patDetails}
            InfixCon _ _ -> ConPatt {con_type = conName, patt_details = patDetails}
    ParPat _ tokLeft pat tokRight -> ParPatt (intermediatePatts pat)
    _ -> VariPatt "Not Implemented"


intermediateConPatDetails :: HsConPatDetails GhcPs -> ConPDetails
intermediateConPatDetails details = case details of 
    PrefixCon tyarg arg -> ConPattPrefix (map intermediatePatts arg)
    InfixCon arg1 arg2 -> ConPattInfix (getVarPatt arg1) (getVarPatt arg2)


getVarPatt :: LPat GhcPs -> VarPatt 
getVarPatt (L _ pat) = case pat of 
    VarPat _ typ -> occNameString . occName . unLoc $ typ 
    _ -> ""


---------------------------------------------------------------------
---------------------------------------------------------------------

{-
Lits
-}

intermediateLits :: HsLit GhcPs -> Lits 
intermediateLits = \case 
    HsChar _ chr -> Chars chr 
    HsString _ str -> Strings (unpackFS str)
    HsInteger _ i _ -> Ints i


intermediateOverLits :: HsOverLit GhcPs -> Lits 
intermediateOverLits (OverLit _ val) = case val of 
    HsIntegral (IL _ _ i) -> Ints i 
    HsFractional (FL _ neg signi exp expBase) ->
        let sign = if neg then -1 else 1 
            factor = if exp < 0 then 1 / (10 ^ abs exp) else 10 ^ exp 
            final = (fromRational (sign * (signi * factor)))
        in Fractionals final 
    HsIsString _ s -> Strings (unpackFS s)

---------------------------------------------------------------------
---------------------------------------------------------------------