{-# LANGUAGE PackageImports #-}


module Main where

-- import qualified Test (recursiveAdd)
import Test (recursiveAdd)
-- import ASTGen

-- main :: IO ()
-- main = do
--   print (recursiveAdd 5 3)
import GHC
import GHC.Paths ( libdir )
import DynFlags 
import System.Environment ( getArgs )
import Control.Monad.IO.Class

main :: IO ()
main = do
  targetFile <- head <$> getArgs
  runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = xopt_set dflags Opt_PackageImports
      setSessionDynFlags dflags'
      setSessionDynFlags dflags
      target <- guessTarget targetFile Nothing
      setTargets [target]
      load LoadAllTargets
      modSum <- getModSummary $ mkModuleName "InsertionSort"
      p <- parseModule modSum
      t <- typecheckModule p
      d <- desugarModule t
      l <- loadModule d
      n <- getNamesInScope
      c <- return $ coreModule d

      g <- getModuleGraph
      mapM_ (liftIO . print . ms_mod . ms_location) g
      liftIO $ print $ parsedSource d