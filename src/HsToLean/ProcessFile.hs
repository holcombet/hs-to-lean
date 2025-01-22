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
    _ -> EmptyD

---------------------------------------------------------------------
---------------------------------------------------------------------

{-
Sig to SignatureD (intermediateSig)

translates Sig to SignatureD
-}

{- translating Sig to SignatureD-}
intermediateSig :: Sig GhcPs -> AST 
intermediateSig decl = case decl of 
    TypeSig _ names typ -> 
        let typeName = unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names)
            sigTypes = intermediateSigType typ
        in SignatureD (TySig {ty_name = typeName, qual_ty = [], fun_type = sigTypes, fun_bind = EmptyB })
    _ -> EmptyD


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


-- removeAllFunTy :: [Types] -> [Types]
-- removeAllFunTy [] = []
-- removeAllFunTy (x : xs) =
--     let result = if isFunType x then removeFunTy x else x 
--     in result : removeAllFunTy xs


-- getFunSig :: HsType GhcPs -> [Types]
-- getFunSig = \case 
--     HsFunTy _ _ arg1 arg2 ->
--         let typList = (unXRec @(GhcPass 'Parsed) arg1) : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
--         in (map intermediateTypes typList)
--     _ -> []

intermediateTypes :: HsType GhcPs -> Types 
intermediateTypes = \case 
    HsFunTy _ _ arg1 arg2 -> 
        let typList = (unXRec @(GhcPass 'Parsed) arg1) : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
        in FType (map intermediateTypes typList)
    HsTyVar _ _ typ -> TypeVar (occNameString . occName . unLoc $ typ)
    HsTyLit _ t -> TypeVar "TyLit"
    _ -> EmptyT


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
intermediateHsBind decl = case decl of 
    FunBind _ name matches _ -> 
        let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name 
            matchPs = map (\(L _ (Match _ _ pats body)) -> 
                let boundVar = map intermediatePatts pats       
                    bodyExpr = intermediateGRHSs body       
                in MP {bound_var = boundVar, guard_body = bodyExpr} ) (unLoc $ mg_alts matches)
            
            matchLPats = map (\(L _ (Match _ _ pats _)) -> pats) (unLoc $ mg_alts matches)

            boundVar = filterValidPatt $ boundVarToPatt$ transposeBoundVar matchLPats

        in ValueD (FBind {fun_name = funName, patt_args = boundVar, matches = matchPs})
    _ -> EmptyD


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
    let guardStmts = if null guardStmt then [] else []      -- implement Stmts 
        exprStr = intermediateLExpr body 
    in StmtBody {guard_stmt = guardStmts, guard_expr =  exprStr}

---------------------------------------------------------------------
---------------------------------------------------------------------

{-
Exprs
-}

intermediateLExpr :: LHsExpr GhcPs -> Exprs 
intermediateLExpr expr = intermediateExpr (unXRec @(GhcPass 'Parsed) expr)

intermediateExpr :: HsExpr GhcPs -> Exprs 
intermediateExpr = \case 
    HsVar _ name -> Var (occNameString . occName . unLoc $ name)
    HsApp _ expr1 expr2 -> App (intermediateExpr (unLoc expr1)) (intermediateExpr (unLoc expr2))
    OpApp _ expr1 op expr2 -> OperApp (intermediateExpr (unLoc expr1)) (intermediateExpr (unLoc op)) (intermediateExpr (unLoc expr2))
    ExplicitList _ exprs -> ExpList (map intermediateLExpr exprs)
    HsOverLit _ lit -> Litr (intermediateOverLits lit)
    HsLit _ lit -> Litr (intermediateLits lit)
    _ -> Other


---------------------------------------------------------------
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