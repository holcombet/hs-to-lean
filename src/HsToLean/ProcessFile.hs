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

{-
Reference:

    let tySig3 = TySig {ty_name = "add", qual_ty = [], fun_type = [TypeVar "Int", TypeVar "Int", TypeVar "Int"], fun_bind = FBind {fun_name = "add", pat_args = [VarPatt "a", VarPatt "b"], matches = [MP {bound_var = [VarPatt "a", VarPatt "b"], guard_body = ""}]}}
    putStrLn $ sigToLean tySig3 
    putStrLn "\n"
-}


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


-- PATTS

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


---------------------------------------------------------------------

{- 
Function that generates the intermediate AST in its entirety
-}

generateIntermediateAST :: ParsedSource -> IO()
generateIntermediateAST ast = do 
    let ps = map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ ast 
        interDecls = map intermediateDecls ps 
        -- sortedDeclList = sortDeclList interDecls 
    liftIO $ writeFile "IntermediateAST.txt" (unlines $ showIntermediateAST interDecls)


showIntermediateAST :: [AST] -> [String]
showIntermediateAST = map show 

---------------------------------------------------------------------

{-
Starting translation from ghc-lib-parser AST to intermediate AST
-}

intermediateDecls :: HsDecl GhcPs -> AST 
intermediateDecls = \case 
    ValD _ decl -> intermediateHsBind decl
    _ -> EmptyD

intermediateHsBind :: HsBind GhcPs -> AST 
intermediateHsBind decl = case decl of 
    FunBind _ name matches _ -> 
        let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name 
            matchPs = map (\(L _ (Match _ _ pats body)) -> 
                let boundVar = map intermediatePatts pats       -- Patts not implemented
                    bodyExpr = intermediateGRHSs body       -- GRHSs not implemented
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
-- filterValidPatt [] = []
-- filterValidPatt (x : xs) =
--     if null x then filterValidPatt xs else
--         chooseVar x : filterValidPatt xs 


chooseVar :: [Patts] -> VarPatt 
chooseVar [] = ""
chooseVar (x : xs) -- = if isVarPatt x || isConPattInfix x then x else chooseVar xs 
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
    
     
    -- if isVarPatt x || isParPatt x || isConPattInfix x then 
    --     x : findValidPattNames xs 
    -- else findValidPattNames xs

----


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

---

intermediateLExpr :: LHsExpr GhcPs -> Exprs 
intermediateLExpr expr = intermediateExpr (unXRec @(GhcPass 'Parsed) expr)

intermediateExpr :: HsExpr GhcPs -> Exprs 
intermediateExpr = \case 
    HsVar _ name -> Var (occNameString . occName . unLoc $ name)
    HsApp _ expr1 expr2 -> App (intermediateExpr (unLoc expr1)) (intermediateExpr (unLoc expr2))
    OpApp _ expr1 op expr2 -> OperApp (intermediateExpr (unLoc expr1)) (intermediateExpr (unLoc op)) (intermediateExpr (unLoc expr2))
    ExplicitList _ exprs -> ExpList (map intermediateLExpr exprs)
    _ -> Other



intermediatePatts :: LPat GhcPs -> Patts
intermediatePatts (L _ pat) = case pat of 
    VarPat _ typ -> VariPatt (occNameString . occName . unLoc $ typ)
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