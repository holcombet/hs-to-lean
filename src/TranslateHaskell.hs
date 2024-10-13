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
        FunBind _ name matches _ -> "Not implemented\n"   -- requires Pat and GRHSs 
                                                          -- NOTE: remove newline 
            -- let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) names
            --     matchStrings = map (\L (L _ (Match _ c pats body)) ->
            --         let argStrings = "\t | " ++ unwords (map prettyLeanPat pats)
            --             bodyString = prettyLeanGRHSs body
            --         in argStrings ++ " = " ++ bodyString) (unLoc $ mg_alts matches)
            -- in unlines matchStrings
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

main = putStrLn "Done."