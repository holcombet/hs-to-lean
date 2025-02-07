{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}


module Main where

import GHC
import GHC.Maybe
import "ghc" GHC.Parser
import "ghc" GHC.Utils.Outputable
import Data.Generics (gshow)
import GHC.Paths

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

import System.IO
import System.Environment ( getArgs, getExecutablePath )
import System.FilePath (takeBaseName, takeDirectory)
import System.Directory (doesFileExist)



import HsToLean.ProcessFile (getIntermediateAST, generateIntermediateAST, showIntermediateAST)
import HsToLean.ASTToLean (astListToLean, astToLean, findASTPairs)



main :: IO ()
main = do

  args <- getArgs

  let defaultTargetFile = "examples/HeapSort.hs"     

  let userTargetFile = if not (null args) && (length args) == 1 then head args else defaultTargetFile 
  fileExists <- doesFileExist userTargetFile 

  if fileExists 
    then liftIO $ putStrLn "Translating file..."
    else liftIO $ putStrLn "Invalid file or file path. Translating default file: examples/HeapSort.hs"
      


  

  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget (if fileExists then userTargetFile else defaultTargetFile) Nothing (Just (Cpp HsSrcFile))
      setTargets [target]
      load LoadAllTargets
      let moduleName = takeBaseName (if fileExists then userTargetFile else defaultTargetFile)
      modSum <- getModSummary $ mkModuleName moduleName
      parsedModule <- GHC.parseModule modSum

      let astForLean = pm_parsed_source parsedModule      -- makes ParsedSource object, used for all translation modules
      
      ----



      let interAST = findASTPairs (getIntermediateAST astForLean)   -- make (intermediate) AST object 
      
      liftIO $ putStrLn "\n\n"
      let fileName = "LeanOutputs/" ++ (getModuleName $ hsmodName $ unLoc astForLean) ++ ".lean"
      liftIO $ writeFile fileName (intercalate "\n\n" (astListToLean interAST))




getModuleName :: Maybe (LocatedA ModuleName) -> String
getModuleName Nothing = ""
getModuleName (Just (L _ moduleName)) = moduleNameString moduleName