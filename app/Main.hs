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



import HsToLean.TranslateHaskell (translateToLean)
import HsToLean.SimpleAST (generateSimpleAST)
import HsToLean.ProcessFile (getIntermediateAST, generateIntermediateAST, showIntermediateAST)
import HsToLean.ASTToLean (astListToLean, astToLean, findASTPairs)

-- import TestAST 

import StructureAst (structAst)
import HaskellToHaskell (translateHaskellToHaskell)




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

  args <- getArgs

  let defaultTargetFile = "examples/TestFunctions.hs"     

  let userTargetFile = if not (null args) && (length args) == 1 then head args else defaultTargetFile 
  fileExists <- doesFileExist userTargetFile 

  if fileExists 
    then liftIO $ putStrLn "Translating file..."
    else liftIO $ putStrLn "Invalid file or file path. Translating default file: examples/TestFunctions.hs"
      


  

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
      
      liftIO $ generateIntermediateAST astForLean         -- generate intermediate AST and write to file (reference file)

      let interAST = findASTPairs (getIntermediateAST astForLean)   -- make (intermediate) AST object 
      
      liftIO $ putStrLn "\n\n"
      liftIO $ writeFile "LeanResult.lean" (intercalate "\n\n" (astListToLean interAST))    -- translate AST to lean and write to file

      {-
      Following lines are for generating resources for testing and debugging
      -}
      liftIO $ writeFile "AST.txt" (gshow astForLean)                 -- printing ghc-lib-parser ast to file
      liftIO $ structAst "AST.txt"                                    -- generate & write structured ast to file

      -- liftIO $ putStrLn $ unlines $ showIntermediateAST interAST   -- show intermediate AST structure
      -- liftIO $  translateHaskellToHaskell astForLean               -- show HaskellToHaskell translation


