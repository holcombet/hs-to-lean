{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns #-}


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
-- import GHC (UnXRec(unXRec), ParsedModule (pm_parsed_source))
-- import Data.Maybe (Maybe(Nothing))

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
  -- get targetFile from cmd line
  


  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget "src/TestInsertionSort.hs" Nothing (Just (Cpp HsSrcFile))
      setTargets [target]
      load LoadAllTargets
      let moduleName = takeBaseName "src/TestInsertionSort.hs"
      modSum <- getModSummary $ mkModuleName moduleName
      parsedModule <- GHC.parseModule modSum
      -- let ast = pm_parsed_source parsedModule
      let astMod = hsmodName $ unLoc $ pm_parsed_source parsedModule
      
      let ast = map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ pm_parsed_source parsedModule
      let prettyAstMod = gshow astMod
      let prettyAst = gshow ast -- write your own gshow
      liftIO $ (putStrLn . prettyHsModuleName) astMod
      liftIO $ mapM_ (putStrLn . prettyHsDecl) ast


prettyHsModuleName :: Maybe (LocatedA ModuleName) -> String
prettyHsModuleName Nothing = ""
prettyHsModuleName (Just (L _ moduleName)) =  "module " ++ moduleNameString moduleName ++ " where"


prettyHsDecl :: HsDecl GhcPs -> String
prettyHsDecl = \case
  TyClD _ decl -> case decl of
    SynDecl _ name tyVar fix rhs -> "type " ++ (occNameString . occName . unXRec @(GhcPass 'Parsed)) name  ++ " = " ++ prettyLHsType rhs
    -- missing:
    -- DataDecl
    -- ClassDecl
    _ -> "Not implemented"
  ValD _ decl -> case decl of 
    -- something's wrong here... function name not binding if function more than 1 line
    FunBind _ name matches _ -> 
      let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name -- no gshow = no "" around function name
          matchStrings = map (\(L _ (Match _ c pats body)) ->
            let argStrings = funName ++ " " ++ unwords (map prettyPat pats)
                bodyString = prettyGRHSs body
            in argStrings ++ " = " ++ bodyString) (unLoc $ mg_alts matches)
      -- in funName ++ " " ++ unlines matchStrings
      in  unlines matchStrings
    PatBind _ pat rhs _ -> prettyPat pat ++ " = " ++ prettyGRHSs rhs
    VarBind _ var_id rhs -> gshow var_id ++ " = " ++ prettyLHsExpr rhs
    -- PatSynBind _ patSyn -> prettyPatSynBind patSyn
    _ -> prettyPrint $ gshow decl
  SigD _ decl -> case decl of
    TypeSig _ names typ -> unwords (map ( occNameString . occName . unXRec @(GhcPass 'Parsed)) names)  ++ " :: " ++ prettyLHsSigWcType typ
    PatSynSig _ names typ -> unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names) ++ " :: " ++ prettyLHsSigType typ
    _ -> "Not implemented"
  _ -> "Not implemented"

-- prettyHsMatchContext :: HsMatchContext GhcPs -> String
-- prettyHsMatchContext context = case context of
--   FunRhs {mc_fun = name}

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

-- prettyPatSynBind :: PatSynBind id id -> String
-- prettyPatSynBind (PSB _ pId details def dir) _ =
--   "pattern " ++ gshow ((occNameString . occName. unXRec @(GhcPass 'Parsed)) pId) ++ prettyPatSynDetails details ++ prettyPat def ++ prettyPatSynDir dir

-- prettyPatSynDetails :: HsPatSynDetails (Located idR) -> String
-- prettyPatSynDetails details = case details of -- currently working on this (from PrettyPatSynBind above)
--   -- PrefixCon _ args -> unwords (map gshow args)
--   InfixCon arg1 arg2 -> gshow ((occNameString . occName . unXRec @(GhcPass 'Parsed)) arg1) ++ " " ++ prettyPat arg2



-- prettyLHsToken :: XRec GhcPs (HsToken p) -> [Char]
-- prettyLHsToken = p


{-
HsType:
  HsForAllTy:   _ _ hstBody
  HsQualTy:     _ hsCtxt hsbody
  HsTyVar:      _ _ LIdP
  HsAppTy:      _ LHsType LHsType
  HsAppKindTy:  _ LHsType LHsKind
  HsFunTy:      _ HsArrow LHsType LHsType
  HsTupleTy:    _ HsTupleSort [LHsType]
  HsSumTy:      _ LHsType
  HsOpTy:       _ _ LHsType LIdp LHsType
  HsParTy:      _ LHsType
  HsIParamTy:   _ _ LHsType
  HsStarTy:     _ Bool
  HsKindSig:    _ LHsType LHsKind
  HsSpliceTy:   _ HsUntypedSplice
  HsDocTy:      _ LHsType LHsDoc
  HsBangTy:     _ HsSrcBang LHsType
-}
prettyHsType :: HsType GhcPs -> String
prettyHsType = \case
  HsFunTy _ _ arg1 arg2 -> prettyLHsType arg1 ++ " -> " ++ prettyLHsType arg2   -- for SigTyp (function signature) I think
  HsTyVar _ _ typ -> occNameString . occName . unLoc $ typ    -- Type Variables (Int -> Int)
  HsAppTy _ typ1 typ2 -> prettyLHsType typ1 ++ " " ++ prettyLHsType typ2
  HsParTy _ typ -> prettyLHsType typ
  HsOpTy _ _ typ1 id typ2 -> prettyLHsType typ1 ++ (occNameString . occName . unLoc $ id) ++ prettyLHsType typ2
  HsListTy _ typ -> "[" ++ prettyLHsType typ ++ "]"   -- list type
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
  LitPat _ pat -> prettyHsLit pat
  WildPat _ -> "_"    -- for wildcards
  ListPat _ pats -> "[" ++ intercalate "," (map prettyPat pats) ++ "]"    -- for list patterns
  NPat _ lit _ _ ->  prettyOverLit (unXRec @(GhcPass 'Parsed) lit)    -- for n (0)
  ConPat _ con details -> (occNameString . occName . unLoc $ con) ++ " " ++ unwords (map prettyPat (prettyHsConPatDetails details)) 
  _ -> "Not implemented"

prettyGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> String
prettyGRHSs (GRHSs _ grhss _) = unwords $ map prettyGRHS grhss

prettyGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> String
prettyGRHS (L _ (GRHS _ _ body)) = prettyLHsExpr body

prettyLHsExpr :: LHsExpr GhcPs -> String
prettyLHsExpr expr = prettyHsExpr (unXRec @(GhcPass 'Parsed) expr)
-- prettyLHsExpr (L _ body) = 

prettyOverLit :: HsOverLit GhcPs -> String
prettyOverLit (OverLit _ val) = case val of
  HsIntegral (IL _ _ i) -> gshow i
  HsFractional f -> gshow f
  HsIsString _ s -> gshow s


prettyHsConPatDetails :: HsConPatDetails GhcPs -> [LPat GhcPs]
prettyHsConPatDetails details = case details of
  PrefixCon tyarg arg -> arg
  InfixCon arg1 arg2 -> [arg1, arg2]
  -- RecCon (HsRecFields fields _)  -> 




{-
HsVar:    _ LIdP
HsLit:    _ HsLit
-}
prettyHsExpr :: HsExpr GhcPs -> String 
prettyHsExpr = \case
  HsVar _ name -> occNameString . occName . unLoc $ name   -- variable names
  HsLit _ lit -> prettyHsLit lit  -- for literals in code (ints, strings, etc.)
  HsApp _ expr1 expr2 -> prettyHsExpr (unLoc expr1) ++ " " ++ prettyHsExpr (unLoc expr2) -- apply a function to an argument (i.e. f x) -- recursiveAdd n (...)
  OpApp _ expr1 op expr2 -> prettyHsExpr (unLoc expr1) ++ " " ++ prettyHsExpr (unLoc op) ++ " " ++ prettyHsExpr (unLoc expr2) -- for operaor expr (i.e. n + m)
  HsPar _ tok1 exp tok2 -> "( " ++ prettyLHsExpr exp ++ " )"  -- parenthesized expressions (i.e. m - 1)
  HsOverLit _ lit -> prettyOverLit lit    -- adds the literals in function body (1 + recursiveAdd n (m - 1))
  ExplicitList _ exprs -> "[" ++ intercalate  ", " (map prettyLHsExpr exprs) ++ "]"
  -- HsCase _ expr matches -> 
  _ -> "Not implemented"


prettyHsLit :: HsLit GhcPs -> String
prettyHsLit = \case
  HsChar _ char -> gshow char
  HsString _ str -> gshow str
  HsInt _ (IL _ _ int) -> gshow int   -- might be wrong
  HsInteger _ int _ -> gshow int
  -- there are more, but this is it for now
  _ -> "Not implemented"  -- not the missing piece ( the 1 )

