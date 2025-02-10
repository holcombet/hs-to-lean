{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

{-
Module for processing the ghc-lib-parser AST into the intermediate ADT structure.
-}

module HsToLean.ProcessFile (generateIntermediateAST, getIntermediateAST, showIntermediateAST) where 

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

import AST
import HsToLean.ASTToLean



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


isConPattPrefix :: Patts -> Bool
isConPattPrefix = \case
    ConPatt _ details -> case details of 
        ConPattPrefix x -> True 
        _ -> False 
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

getConType :: Patts -> String 
getConType = \case 
    ConPatt t d -> t 
    _ -> ""

getVarPattFromPatts :: Patts -> VarPatt
getVarPattFromPatts = \case 
    VariPatt x -> x 
    -- ConPatt t d -> if t == "[]" else
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
    liftIO $ writeFile "IntermediateAST.txt" ("[" ++ (intercalate ",\n" $ showIntermediateAST interDecls) ++ "]")


getIntermediateAST :: ParsedSource -> [AST]
getIntermediateAST ast = do 
    let ps = map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ ast 
        interDecls = map intermediateDecls ps 
    
    interDecls

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
    InstD _ decl -> intermediateInstDecl decl 
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


{-
intermediateSigToSigs
    Translates ghc-lib-parser's Sig object to intermediate AST Sigs object
-}
intermediateSigToSigs :: Sig GhcPs -> Sigs 
intermediateSigToSigs decl = case decl of 
    TypeSig _ names typ -> 
        let typeName = unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names)
            sigTypes = intermediateSigType typ 
        in TySig {ty_name = typeName, qual_ty = [], fun_type = sigTypes, fun_bind = EmptyB}
    ClassOpSig _ isDefault n typ -> 
        let name = map (occNameString . occName . unLoc) n
            typeS = intermediateHsSigType (unLoc typ )
        in ClassSigs {is_default = isDefault, names = name, sig_ty = typeS}
    _ -> EmptySig

intermediateSigType :: LHsSigWcType GhcPs -> [Types]
intermediateSigType = \case 
    HsWC ext body -> intermediateHsSigType (unXRec @(GhcPass 'Parsed) body)

-- for Type Signature to remove FunType from type list
intermediateHsSigType :: HsSigType GhcPs -> [Types]
intermediateHsSigType = \case 
    HsSig ext bndrs body -> 
        let tys = intermediateTypes (unLoc body)
            ls = removeFunTy tys
            -- l = removeAllFunTy ls
        in ls


-- for non-type signature related calls (e.g. getting derived types)
intermediateHsSigTypes :: HsSigType GhcPs -> Types
intermediateHsSigTypes = \case 
    HsSig ext bndrs body -> intermediateTypes (unLoc body)


removeFunTy :: Types -> [Types]
removeFunTy = \case 
    FType typ -> typ 
    TypeVar t -> [TypeVar t]
    ListTy l -> [ListTy l]
    AppTy f x-> [AppTy f x]
    ExpListTy l -> [ExpListTy l]
    FunVar f -> [FunVar f]
    ParaTy p -> [ParaTy p]
    EmptyT -> []


intermediateTypes :: HsType GhcPs -> Types 
intermediateTypes = \case 
    HsFunTy _ _ arg1 arg2 -> 
        let typList = (unXRec @(GhcPass 'Parsed) arg1) : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
        in FType (map intermediateTypes typList)
    HsTyVar _ _ typ -> 
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
    | x == "Maybe" = FunVar LMaybe 
    | x == "a" = FunVar LAlphaA
    | x == "Show" = FunVar LShow
    | x == "Eq" = FunVar LEq
    | x == "Enum" = FunVar LEmpty 
    | x == "Bounded" = FunVar LEmpty 
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


{-
intermediateHsBindsToBinds 
    function that translates ghc-lib-parser AST to intermediate AST
    returns: Intermediate AST's Binds object 
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
            numPlaceholderVar = countNumTempVar boundVar 

        -- in FBind {fun_name = funName, patt_args = boundVar, matches = matchPs}
        in FBind {fun_name = funName, patt_args = if numPlaceholderVar == 0 then boundVar else replaceTempVar numPlaceholderVar boundVar, matches = matchPs}
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
    | isConPattPrefix x = "temporaryVar"
    | otherwise = chooseVar xs 


findValidPattNames :: [Patts] -> [Patts]
findValidPattNames [] = []
findValidPattNames (x : xs) = 
    let name = if isVarPatt x then x else 
                    if isConPattInfix x then x  else 
                        if isConPattPrefix x && not (getConType x == "[]" ) then x
                            else EmptyP
    in name : findValidPattNames xs
    
{-
replaceTempVar, replaceVar
    replace temp var with created var
-}
     
replaceTempVar :: Int -> [VarPatt] -> [VarPatt]
replaceTempVar i v = 
    let newVar = generateVarNamesExcluding i v 
    in replaceVar v newVar 

replaceVar :: [VarPatt] -> [String] -> [VarPatt]
replaceVar [] _ = [] 
replaceVar (x : xs) (y : ys) 
    | x == "temporaryVar" = y : replaceVar xs ys 
    | otherwise = x : replaceVar xs (y : ys )
replaceVar (x : xs) [] = x : xs

{-
countNumTempVar
-}
countNumTempVar :: [VarPatt] -> Int 
countNumTempVar [] = 0 
countNumTempVar (x : xs) = if x == "temporaryVar" then 1 + countNumTempVar xs 
                            else countNumTempVar xs
---------------------------------------------------------------------
---------------------------------------------------------------------


{-
Guards (GRHSs, GRHS, etc...) for function body (and other things)
-}

intermediateGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> GuardRHSs
intermediateGRHSs (GRHSs _ grhss binds) = 
    let sb = map intermediateGRHS grhss 
        bndr = intermediateLocBinds binds 
    in Guards {guard_exprs = sb, loc_binds = bndr}


intermediateGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> GuardRHS 
intermediateGRHS (L _ (GRHS _ guardStmt body)) = 
    let guardStmts = if null guardStmt then [] else intermediateGuardStmts guardStmt      
        exprStr = intermediateLExpr body 
    in StmtBody {guard_stmt = guardStmts, guard_expr =  exprStr}


intermediateGuardStmts :: [GuardLStmt GhcPs] -> [Stmts]
intermediateGuardStmts stms = map intermediateGuardStmt stms 


intermediateGuardStmt :: GuardLStmt GhcPs -> Stmts 
intermediateGuardStmt (L _ stmt) = case stmt of 
    BodyStmt _ expr _ _ -> BodyStmts (intermediateLExpr expr)
    BindStmt _ pat expr -> EmptyS 
    _ -> EmptyS


intermediateLocBinds :: HsLocalBinds GhcPs -> LocBinds 
intermediateLocBinds binds = case binds of 
    EmptyLocalBinds _ -> EmptyLocBinds 
    HsValBinds _ (ValBinds _ bindlst sigs) -> intermediateValBinds bindlst sigs

intermediateValBinds :: LHsBindsLR GhcPs GhcPs -> [LSig GhcPs] -> LocBinds 
intermediateValBinds binds sigs = 
    let bindList = bagToList binds 
        prettyBinds = map intermediateLHsBindLR bindList 
        prettySigs = map (intermediateSigToSigs . unLoc) sigs 
    in ValsBinds prettyBinds prettySigs

{-
intermediateLHsBindLR 
    function for function binding in function body
-}
intermediateLHsBindLR :: LHsBindLR GhcPs GhcPs -> Binds
intermediateLHsBindLR (L _ bind) = case bind of 
    FunBind _ id matches _ -> intermediateHsBindToBinds bind
    _ -> EmptyB


{- making a MatchPair -}

intermediateMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> [MatchPair]
intermediateMatchGroup = \case
    MG ext (L _ matches) _ -> map intermediateLMatch matches 

intermediateLMatch :: LMatch GhcPs (LHsExpr GhcPs) -> MatchPair
intermediateLMatch (L _ match) = case match of 
    Match _ _ pats body -> MP (map intermediatePatts pats) (intermediateGRHSs body)
        

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
    ClassDecl _ cntxt tyCon tyVar fix _ metSig mets _ _ _ -> 
        let className = (occNameString . occName . unXRec @(GhcPass 'Parsed)) tyCon 
            tyVarStr = intermediateQTyVars tyVar 
            methodS = map intermediateSigToSigs (map unLoc metSig)
            defaultMet = map intermediateLHsBindLR (intermediateConvertLHsBindsToLHsBindLR mets)
        in TyClassD (ClassDecls {class_name = className, qualTy_var = tyVarStr, method_sigs = methodS, default_method = defaultMet})
    _ -> EmptyD


intermediateConvertLHsBindsToLHsBindLR :: LHsBinds GhcPs -> [LHsBindLR GhcPs GhcPs]
intermediateConvertLHsBindsToLHsBindLR b = bagToList b

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
    if not (null derv) then getHsDeriving derv else [] 


getHsDeriving :: [LHsDerivingClause GhcPs] -> [Types]
getHsDeriving clauses =  getHsDerivingClause  $ head clauses 

getHsDerivingClause :: LHsDerivingClause GhcPs -> [Types] 
getHsDerivingClause (L _ (HsDerivingClause _ strat ((L _ typ)))) = 
    getDerivClauseTys typ 

getDerivClauseTys :: DerivClauseTys GhcPs -> [Types]
getDerivClauseTys tys = case tys of 
    DctSingle _ ty -> intermediateHsSigType (unLoc ty)
    DctMulti _ typs ->  (map intermediateHsSigTypes (map unLoc typs))

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
Instance Declarations
-}

intermediateInstDecl :: InstDecl GhcPs -> AST 
intermediateInstDecl decl = case decl of 
    ClsInstD _ ci -> case ci of 
        ClsInstDecl _ sigTys bind sigs _ _ _ -> 
            let st = intermediateHsSigType (unLoc sigTys)
                b = map intermediateLHsBindLR (intermediateConvertLHsBindsToLHsBindLR bind)
                ss = map (intermediateSigToSigs . (unXRec @(GhcPass 'Parsed))) sigs
            in  InstancesD  (ClassInst {inst_ty = st, binds = b, exp_sigs = ss})
    _ -> InstancesD (EmptyID)





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
    HsPar _ _ e _ -> ParaExpr (intermediateExpr (unLoc e))
    HsLet _ _ binds _ exp -> 
        let b = intermediateLocBinds binds 
            e = intermediateExpr (unLoc exp)
        in LetExpr b e
    HsDo _ _ es -> 
        let e = unXRec @(GhcPass 'Parsed) es
            s = map unLoc e 
        in DoExprs (map intermediateStmts s)
    HsCase _ exp matches -> CaseExpr (intermediateExpr (unLoc exp)) (intermediateMatchGroup matches)
    HsIf _ exp1 exp2 exp3 -> IfExpr (intermediateExpr (unLoc exp1)) (intermediateExpr (unLoc exp2)) (intermediateExpr (unLoc exp3))
    ExplicitList _ exprs -> ExpList (map intermediateLExpr exprs)
    HsOverLit _ lit -> Litr (intermediateOverLits lit)
    HsLit _ lit -> Litr (intermediateLits lit)
    _ -> Other

intermediateStmts :: StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs)) -> Stmts 
intermediateStmts s = case s of 
    BodyStmt _ expr _ _ -> BodyStmts (intermediateLExpr expr) 
    BindStmt _ pat body -> BindStmts (intermediatePatts  pat) (intermediateLExpr body)
    _ -> EmptyS


{-
intermediateDecideExpr
    function for identifying keywords/operations that have different name in Lean (or other ITPs)
    to improve translation efforts
-}
intermediateDecideExpr :: String -> Exprs 
intermediateDecideExpr x
    | x == "putStrLn" = SpecialVar VPutStrLn
    | x == "putStr" = SpecialVar VPutStr
    | x == "print" = SpecialVar VPrint
    | x == "Just" = SpecialVar VJust 
    | x == "Nothing" = SpecialVar VNothing
    | x == "Left" = SpecialVar VLeft
    | x == "Right" = SpecialVar VRight
    | x == "pi" = SpecialVar VPi
    | x == "show" = SpecialVar VShow
    | x == "True" = SpecialVar VTrue 
    | x == "False" = SpecialVar VFalse
    | x == "foldr" = SpecialVar VFoldr
    | otherwise = Var x


{-
function for reordering lists built with the ":" operator
-}

reorderConsList :: HsExpr GhcPs -> [Exprs]
reorderConsList x = case x of
    OpApp _ expr1 op expr2 -> 
        let o = intermediateExpr (unLoc op)
            e2 = reorderConsList (unLoc expr2)
        in intermediateExpr (unLoc expr1) : o : e2
    _ -> [intermediateExpr x]


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
    WildPat _ -> WildPatt
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


{-
generateVarNamesExcluding
    Helper function
-}
generateVarNamesExcluding :: Int -> [String] -> [String]
generateVarNamesExcluding n exclude = take n $ filter (`notElem` exclude) $ map toName [0..]
  where
    toName :: Int -> String
    toName x
      | x < 26    = [alphabet !! x]
      | otherwise = toName (x `div` 26 - 1) ++ [alphabet !! (x `mod` 26)]
    alphabet = ['a'..'z']