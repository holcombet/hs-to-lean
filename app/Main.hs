{-# LANGUAGE PackageImports, CPP #-}


module Main where

import GHC
import GHC.Maybe
import "ghc" GHC.Parser
import "ghc" GHC.Utils.Outputable
import GHC.Paths
import System.Environment ( getArgs )
import System.FilePath (takeBaseName)
import System.IO (writeFile)
import "ghc" GHC.Driver.Session -- ( defaultFatalMessager, defaultFlushOut )
import Control.Monad.IO.Class
import "ghc" GHC.Driver.Phases (Phase(Cpp))  
import "ghc" GHC.Types.SourceFile (HscSource(HsSrcFile))
import "ghc" GHC.Unit.Module 


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
      let ast = ppr $ pm_parsed_source parsedModule
      liftIO $ putStrLn $ showSDocUnsafe ast      


-- doesnt work --

-- main :: IO ()
-- main = do
--   defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
--     runGhc (Just libdir) $ do
--       dflags <- getSessionDynFlags
--       setSessionDynFlags dflags
--       target <- guessTarget "/Users/tallyholcombe/Documents/Thesis/hs-to-lean/src/Test.hs" Nothing (Just (Cpp HsSrcFile))
--       setTargets [target]
--       load LoadAllTargets
--       let moduleName = takeBaseName "/Users/tallyholcombe/Documents/Thesis/hs-to-lean/src/Test.hs"
--       modSum <- getModSummary $ mkModuleName moduleName
--       parsedModule <- GHC.parseModule modSum
      

--       tcheck <- GHC.typecheckModule p
--       dsugar <- GHC.desugarModule tcheck
--       loadM <- GHC.loadModule dsugar
--       n <- GHC.getNamesInScope
--       c <- return $ GHC.coreModule dsugar 

--       g <- GHC.getModuleGraph 
--       mapM showModule g 
--       putStrLn $ (parsedSource dsugar, "/n-----/n",  typecheckedSource dsugar )









-- main :: IO ()
-- main = do
--    res <- example
--    str <- runGhc (Just libdir) $ do
--       dflags <- getSessionDynFlags
--       return $ showSDoc dflags $ ppr res
--    putStrLn str
-- example = 
--   defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
--     runGhc (Just libdir) $ do
--         dflags <- getSessionDynFlags
--         let dflags' = foldl xopt_set dflags
--                             [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
--         setSessionDynFlags dflags'
--         target <- guessTarget "/Users/tallyholcombe/Documents/Thesis/hs-to-lean/src/Test.hs" Nothing
--         setTargets [target]
--         load LoadAllTargets
--         modSum <- getModSummary $ mkModuleName "Test"
--         p <- parseModule modSum
--         t <- typecheckModule p
--         d <- desugarModule t
--         l <- loadModule d
--         n <- getNamesInScope
--         c <- return $ coreModule d
 
--         g <- getModuleGraph
--         mapM showModule g     
--         return $ (parsedSource d,"/n-----/n",  typecheckedSource d)

