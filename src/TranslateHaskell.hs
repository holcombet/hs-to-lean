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
                    let argStrings = "\t| " ++ unwords (map prettyLeanPat pats)
                        bodyString = "Not implemented" -- prettyLeanGRHSs body
                    in argStrings ++ " => " ++ bodyString) (unLoc $ mg_alts matches)
            in unlines matchStrings
        PatBind _ pat rhs _ -> "Not implemented"    -- requires Pat
        VarBind _ var rhs -> "Not implemented"      -- requires LHsExpr
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
    NPat _ lit _ _ -> "Not implemented"     -- literals
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
prettyLeanGRHS (L _ (GRHS _ _ body)) = "Not implemented" --prettyLHsExpr body


main = putStrLn "Done."