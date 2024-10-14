{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns #-}

module TranslateHaskell (translateToLean) where

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
import "ghc" GHC.Plugins ( occNameString, HasOccName(occName) )
import Data.String (String)
import Data.List (intercalate)
import "ghc" GHC.Types.SourceText


translateToLean :: ParsedSource -> IO()
translateToLean ast = do
    let parsedAst =  map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ ast

    liftIO $ writeFile "src/result.txt" (unlines $ map prettyLeanDecl parsedAst)

-- translateToLean ast = 

prettyLeanDecl :: HsDecl GhcPs -> String
prettyLeanDecl = \case
    TyClD _ decl -> case decl of
        SynDecl _ name tyVar fix rhs -> "Not implemented"
        _ -> "Not implemented"
    ValD _ decl -> case decl of
        FunBind _ name matches _ -> --"Not implemented\n"   -- requires Pat and GRHSs 
                                                          -- NOTE: remove newline 
            let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name
                matchStrings = map (\(L _ (Match _ c pats body)) ->
                    let argStrings = "\t| " ++  intercalate ", " (map prettyLeanPat pats)
                        bodyString = prettyLeanGRHSs body -- prettyLeanGRHSs body
                    in argStrings ++ " => " ++ bodyString) (unLoc $ mg_alts matches)
            in unlines matchStrings
        PatBind _ pat rhs _ -> prettyLeanPat pat ++ " = " ++ prettyLeanGRHSs rhs    -- requires Pat
        VarBind _ var rhs -> gshow var ++ " = " ++ prettyLLeanExpr rhs      -- requires LHsExpr
        _ -> "Not implemented"
    SigD _ decl -> case decl of
        TypeSig _ names typ -> "def " ++ unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names) ++ " : " ++ prettyLLeanSigWcType typ    -- requires LHsSigWcType
        PatSynSig _ names typ -> "def " ++ unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names) ++ " : " ++ "Not implemented"  -- requires LHsSigType
        _ -> "Not implemented"
    _ -> "Not implemented"


prettyLLeanSigWcType :: LHsSigWcType GhcPs -> String
prettyLLeanSigWcType = \case
    HsWC ext body -> prettyLLeanSigType body

prettyLLeanSigType :: LHsSigType GhcPs -> String
prettyLLeanSigType arg = prettyLeanSigType (unXRec @(GhcPass 'Parsed) arg)


prettyLeanSigType :: HsSigType GhcPs -> String
prettyLeanSigType = \case
    HsSig ext bndrs body -> prettyLLeanType body        -- requires HsType

prettyLLeanType :: LHsType GhcPs -> String
prettyLLeanType arg = prettyLeanType (unXRec @(GhcPass 'Parsed) arg)


prettyLeanType :: HsType GhcPs -> String
prettyLeanType = \case
    HsFunTy _ _ arg1 arg2 -> prettyLLeanType arg1 ++ " -> " ++ prettyLLeanType arg2     -- for SigType
    HsTyVar _ _ typ  
        | typVar == "Int" -> "Nat"
        | otherwise -> "Not implemented"
        where typVar = occNameString . occName . unLoc $ typ      -- getting type
    HsListTy _ typ -> "List " ++ prettyLLeanType typ
    HsAppTy _ typ1 typ2 -> "Not implemented"     -- 
    HsParTy _ typ -> "Not implemented"
    HsOpTy _ _ typ1 id typ2 -> "Not implemented"
    _ -> "Not implemented"


prettyLeanMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> String
prettyLeanMatchGroup = \case
    MG ext (L _ matches) _ -> unlines $ map prettyLLeanMatch matches

prettyLLeanMatch :: LMatch GhcPs (LHsExpr GhcPs) -> String
prettyLLeanMatch (L _ match) = case match of
    Match _ _ pats body -> unwords (map prettyLeanPat pats) ++ " -> " ++ prettyLeanGRHSs body

prettyLeanPat :: LPat GhcPs -> String
prettyLeanPat (L _ pat) = case pat of
    VarPat _ typ -> occNameString . occName . unLoc $ typ   -- might have to implement a renamer down the line...insert
    LitPat _ pat -> "Not implemented"       -- requires HsLit
    ListPat _ pats -> "[" ++ intercalate "," (map prettyLeanPat pats) ++ "]"        -- still have literal lists... 
    NPat _ lit _ _ -> prettyLeanOverLit (unXRec @(GhcPass 'Parsed) lit)     -- OverLit
    ParPat _ tokLeft pat tokRight -> "(" ++ prettyLeanPat pat ++ ")"
    ConPat _ (L _ name) details ->
        let conName = occNameString . occName $ name
            patDetails = prettyLeanConPatDetails details 
        in case details of
            PrefixCon _ _ -> conName ++ " " ++ unwords patDetails       -- not sure what this does yet, no change
            InfixCon _ _ -> intercalate (" " ++ "::" ++ " ") patDetails     -- for colon lists (::) but not quite right

    _ -> "Not implemented"


prettyLeanConPatDetails :: HsConPatDetails GhcPs -> [String]
prettyLeanConPatDetails details = case details of
    PrefixCon tyarg arg -> map prettyLeanPat arg
    InfixCon arg1 arg2 -> [prettyLeanPat arg1, prettyLeanPat arg2]
    

prettyLeanGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> String
prettyLeanGRHSs (GRHSs _ grhss _) = unwords $ map prettyLeanGRHS grhss

prettyLeanGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> String
prettyLeanGRHS (L _ (GRHS _ _ body)) = prettyLLeanExpr body --prettyLHsExpr body


-- did not change from original prettyOverLit function
-- have not found issues yet
prettyLeanOverLit :: HsOverLit GhcPs -> String
prettyLeanOverLit (OverLit _ val) = case val of
    HsIntegral (IL _ _ i) -> gshow i
    HsFractional f -> gshow f
    HsIsString _ s -> gshow s


prettyLLeanExpr :: LHsExpr GhcPs -> String
prettyLLeanExpr expr = prettyLeanExpr (unXRec @(GhcPass 'Parsed) expr)

prettyLeanExpr :: HsExpr GhcPs -> String
prettyLeanExpr = \case
    HsVar _ name -> occNameString . occName . unLoc $ name      -- variable names
    HsLit _ lit -> "Not implemented"    -- requires HsLit
    HsApp _ exp1 exp2 -> prettyLeanExpr (unLoc exp1) ++ " " ++ prettyLeanExpr (unLoc exp2)  -- function application to arguments
    OpApp _ exp1 op exp2 ->  prettyLeanExpr (unLoc exp1) ++ " " ++ prettyLeanExpr (unLoc op) ++ " " ++ prettyLeanExpr (unLoc exp2)   -- operators 
        -- TODO: account for : denoted lists to use :: for lean lists
    HsPar _ tok1 exp tok2 -> "( " ++ prettyLLeanExpr exp ++ " )"
    HsOverLit _ lit -> prettyLeanOverLit lit
    ExplicitList _ exprs -> "[" ++ intercalate ", " (map prettyLLeanExpr exprs) ++ "]"
    HsIf _ exp1 exp2 exp3 -> "if " ++ prettyLLeanExpr exp1 ++ " then " ++ prettyLLeanExpr exp2 ++ " else " ++ prettyLLeanExpr exp3
    _ -> "Not implemented"


main = putStrLn "Done."