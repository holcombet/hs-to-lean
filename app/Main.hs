{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase #-}


module Main where

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
import "ghc" GHC.Plugins
import Data.String (String)
import GHC (UnXRec(unXRec))

prettyPrint :: String -> String
prettyPrint = unlines . snd . foldl processChar (0, []) where
    processChar (indent, lines) char
        | char == '{' = (indent + 1, lines ++ [replicate indent ' ' ++ [char]])
        | char == '}' = (indent - 1, lines ++ [replicate (indent - 1) ' ' ++ [char]])
        | otherwise = if null lines
                      then (indent, [[char]])
                      else (indent, init lines ++ [last lines ++ [char]])

-- runs but doesn't parse --
main :: IO ()
main = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget "src/Test.hs" Nothing (Just (Cpp HsSrcFile))
      setTargets [target]
      load LoadAllTargets
      let moduleName = takeBaseName "src/Test.hs"
      modSum <- getModSummary $ mkModuleName moduleName
      parsedModule <- GHC.parseModule modSum
      -- let ast = pm_parsed_source parsedModule
      let ast = map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ pm_parsed_source parsedModule
      -- let prettyAst = prettyPrint $ gshow ast
      let prettyAst = gshow ast -- write your own gshow
      liftIO $ mapM_ (putStrLn . prettyHsDecl) ast

prettyHsDecl :: HsDecl GhcPs -> String
prettyHsDecl = \case
  -- ValD _ decl -> prettyPrint $ gshow decl 
  ValD _ decl -> case decl of 
    -- FunBind takes (XFunBind GhcPs GhcPs) (LIdp GhcPs) (MatchGroup GhcPs (LHsExpr GhcPs)) ([CoreTickish])
    FunBind _ fun_id matches  _ -> gshow (( occNameString . occName . unXRec @(GhcPass 'Parsed) ) fun_id) ++ " = " ++  prettyMatchGroup matches
    _ -> prettyPrint $ gshow decl
  SigD _ decl -> case decl of
    TypeSig _ names typ -> gshow (map ( occNameString . occName . unXRec @(GhcPass 'Parsed)) names)  ++ " :: " ++ prettyLHsSigWcType typ
    _ -> "Not implemented"
  _ -> "Not implemented"

prettyLHsSigWcType :: LHsSigWcType GhcPs -> String
prettyLHsSigWcType = \case
  HsWC ext body -> prettyLHsSigType body

prettyLHsSigType :: LHsSigType GhcPs -> String
prettyLHsSigType arg = prettyHsSigType (unXRec @(GhcPass 'Parsed) arg)

prettyHsSigType :: HsSigType GhcPs -> String
prettyHsSigType = \case
  HsSig ext bndrs body -> prettyLHsType body

prettyLHsType :: LHsType GhcPs -> String
prettyLHsType arg = prettyHsType (unXRec @(GhcPass 'Parsed) arg)

prettyHsType :: HsType GhcPs -> String
prettyHsType = \case
  HsFunTy _ _ arg1 arg2 -> prettyLHsType arg1 ++ " -> " ++ prettyLHsType arg2
  HsTyVar _ _ typ -> occNameString . occName . unLoc $ typ
  _ -> "Not implemented"

prettyMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> String
prettyMatchGroup = \case
  MG ext (L _ matches) _ -> unlines $ map prettyLMatch matches

prettyLMatch :: LMatch GhcPs (LHsExpr GhcPs) -> String
prettyLMatch (L _ match) = case match of 
  -- pats :: [LPat p]
  -- body :: GRHSs p body
  Match _ _ pats body -> unwords (map prettyPat pats) ++ " -> " ++ prettyGRHSs body
  -- _ -> "Not implemented"

prettyPat :: LPat GhcPs -> String
prettyPat (L _ pat) = case pat of
  VarPat _ typ -> occNameString . occName . unLoc $ typ
  _ -> "Not implemented"

prettyGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> String
prettyGRHSs (GRHSs _ grhss _) = unlines $ map prettyGRHS grhss

prettyGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> String
prettyGRHS (L _ (GRHS _ _ body)) = prettyLHsExpr body

prettyLHsExpr :: LHsExpr GhcPs -> String
prettyLHsExpr expr = prettyHsExpr (unXRec @(GhcPass 'Parsed) expr)
-- prettyLHsExpr (L _ body) = 

prettyHsExpr :: HsExpr GhcPs -> String 
prettyHsExpr = \case
  HsVar _ name -> occNameString . occName . unLoc $ name
  _ -> "Not implemented"


-- prettyLMatch :: XRec GhcPs [LMatch GhcPs (LHsExpr GhcPs)] -> String
-- prettyLMatch = \case
--   LM xrec lm -> 