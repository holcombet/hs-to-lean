{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}


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
import "ghc" GHC.Plugins ( occNameString, HasOccName(occName) )
import Data.String (String)
import Data.List (intercalate)
import "ghc" GHC.Types.SourceText
import Data.Void
import GHC.Runtime.Eval (Term(ty))
import "ghc" GHC.Data.FastString (unpackFS)
import Data.Bool (Bool)
import "ghc" GHC.Data.Bag (bagToList, Bag)
import "ghc" GHC.Hs.Binds
import Data.Ratio ((%))


import HsToLean.TranslateHaskell (translateToLean)



prettyPrint :: String -> String
prettyPrint = unlines . snd . foldl processChar (0, []) where
    processChar (indent, lines) char
        | char == '{' = (indent + 1, lines ++ [replicate indent ' ' ++ [char]])
        | char == '}' = (indent - 1, lines ++ [replicate (indent - 1) ' ' ++ [char]])
        | otherwise = if null lines
                      then (indent, [[char]])
                      else (indent, init lines ++ [last lines ++ [char]])



main :: IO ()
main = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget "src/HeapSort.hs" Nothing (Just (Cpp HsSrcFile))
      setTargets [target]
      load LoadAllTargets
      let moduleName = takeBaseName "src/HeapSort.hs"
      modSum <- getModSummary $ mkModuleName moduleName
      parsedModule <- GHC.parseModule modSum

      -- make ParsedSource object to pass to translateToLean, generateSimpleAST, and translateHaskellToHaskell
      let astForLean = pm_parsed_source parsedModule

      -- calls the module that translates AST into Lean "code"
      liftIO $ translateToLean astForLean


