{-# LANGUAGE PackageImports, CPP #-}


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

prettyPrint :: String -> String
prettyPrint = unlines . snd . foldl processChar (0, []) where
    processChar (indent, lines) char
        | char == '{' = (indent + 1, lines ++ [replicate indent ' ' ++ [char]])
        | char == '}' = (indent - 1, lines ++ [replicate (indent - 1) ' ' ++ [char]])
        | otherwise = (indent, init lines ++ [last lines ++ [char]])
        
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
      let ast = pm_parsed_source parsedModule
      let prettyAst = prettyPrint $ gshow ast
      liftIO $ putStrLn prettyAst

