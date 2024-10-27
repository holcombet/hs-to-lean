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
import Data.Void
import GHC.Runtime.Eval (Term(ty))
import "ghc" GHC.Data.FastString (unpackFS)
import Data.Bool (Bool)
import "ghc" GHC.Data.Bag (bagToList, Bag)
import "ghc" GHC.Hs.Binds
import Data.Ratio ((%))


import HsToLean.TranslateHaskell (translateToLean)
import StructureAst (structAst)



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
  -- TODO: get target file from command line
  


  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget "src/Calendar.hs" Nothing (Just (Cpp HsSrcFile))
      setTargets [target]
      load LoadAllTargets
      let moduleName = takeBaseName "src/Calendar.hs"
      modSum <- getModSummary $ mkModuleName moduleName
      parsedModule <- GHC.parseModule modSum

      let astForLean = pm_parsed_source parsedModule

      -- following 3 lines for generating AST in .txt and structuring
      liftIO $ translateToLean astForLean
      liftIO $ writeFile "AST.txt" (gshow astForLean)
      liftIO $ structAst "AST.txt"


      -- Haskell ast to Haskell
      let astMod = hsmodName $ unLoc $ pm_parsed_source parsedModule
      let ast = map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ pm_parsed_source parsedModule
      let prettyAstMod = gshow astMod
      let prettyAst = gshow ast -- write your own gshow
      liftIO $ (putStrLn . prettyHsModuleName) astMod
      liftIO $ mapM_ (putStrLn . prettyHsDecl) ast


prettyHsModuleName :: Maybe (LocatedA ModuleName) -> String
prettyHsModuleName Nothing = ""
prettyHsModuleName (Just (L _ moduleName)) =  "module " ++ moduleNameString moduleName ++ " where\n"


prettyHsDecl :: HsDecl GhcPs -> String
prettyHsDecl = \case
  TyClD _ decl -> case decl of
    SynDecl _ name tyVar fix rhs -> "type " ++ (occNameString . occName . unXRec @(GhcPass 'Parsed)) name  ++ " = " ++ prettyLHsType rhs
    -- TODO: ClassDecl
    DataDecl _ name tyVar fix dataDef ->
      let dataName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name
          tyVarStr = prettyLHsQTyVars tyVar
          dataDefStr = prettyHsDataDefn dataDef
      in "data " ++ dataName ++ " " ++ tyVarStr ++ " = " ++ dataDefStr ++ "\n"
    _ -> "Not implemented"
  -- TODO: InstD, DerivD
  ValD _ decl -> prettyHsBind decl--case decl of 
    -- FunBind _ name matches _ -> 
    --   let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name -- no gshow = no "" around function name
    --       matchStrings = map (\(L _ (Match _ c pats body)) ->
    --         let argStrings = funName ++ " " ++ unwords (map prettyPat pats)
    --             bodyString = prettyGRHSs body
    --             indent = if hasGuards body then replicate (length argStrings) ' ' else ""
    --         in argStrings ++  bodyString) (unLoc $ mg_alts matches)
    --       -- indent = if any hasGuards (map (\(L _ (Match _ c pats body)) -> body ) (unLoc $ mg_alts matches)) then replicate (length funName) ' ' else ""
    --         -- in argStrings ++ " = " ++ bodyString) (unLoc $ mg_alts matches)
    --   in  unlines matchStrings
    -- PatBind _ pat rhs _ -> prettyPat pat ++ " = " ++ prettyGRHSs rhs
    -- VarBind _ var_id rhs -> gshow var_id ++ " = " ++ prettyLHsExpr rhs
    -- TODO: PatSynBind
    -- _ -> prettyPrint $ gshow decl
  SigD _ decl -> prettySig decl --case decl of
    -- TypeSig _ names typ -> unwords (map ( occNameString . occName . unXRec @(GhcPass 'Parsed)) names)  ++ " :: " ++ prettyLHsSigWcType typ
    -- PatSynSig _ names typ -> unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names) ++ " :: " ++ prettyLHsSigType typ
    -- ClassOpSig _ isDefault names typ ->
    --   let defaultStr = if isDefault then "default " else ""
    --       nameStr = unwords $ map (occNameString . occName . unLoc) names
    --       typeStr = prettyLHsSigType typ
    --   in defaultStr ++ nameStr ++ " :: " ++ typeStr
    -- _ -> "Not implemented"
  _ -> "Not implemented"


prettyHsBind :: HsBind GhcPs -> String
prettyHsBind decl = case decl of
  FunBind _ name matches _ -> 
    let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) name -- no gshow = no "" around function name
        matchStrings = map (\(L _ (Match _ c pats body)) ->
          let argStrings = funName ++ " " ++ unwords (map prettyPat pats)
              bodyString = prettyGRHSs body
              indent = if hasGuards body then replicate (length argStrings) ' ' else ""
          in argStrings ++  bodyString) (unLoc $ mg_alts matches)
        -- indent = if any hasGuards (map (\(L _ (Match _ c pats body)) -> body ) (unLoc $ mg_alts matches)) then replicate (length funName) ' ' else ""
          -- in argStrings ++ " = " ++ bodyString) (unLoc $ mg_alts matches)
    in  unlines matchStrings
  PatBind _ pat rhs _ -> prettyPat pat ++ " = " ++ prettyGRHSs rhs
  VarBind _ var_id rhs -> gshow var_id ++ " = " ++ prettyLHsExpr rhs
  _ -> prettyPrint $ gshow decl

prettySig :: Sig GhcPs -> String
prettySig decl = case decl of
  TypeSig _ names typ -> unwords (map ( occNameString . occName . unXRec @(GhcPass 'Parsed)) names)  ++ " :: " ++ prettyLHsSigWcType typ
  PatSynSig _ names typ -> unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names) ++ " :: " ++ prettyLHsSigType typ
  ClassOpSig _ isDefault names typ ->
    let defaultStr = if isDefault then "default " else ""
        nameStr = unwords $ map (occNameString . occName . unLoc) names
        typeStr = prettyLHsSigType typ
    in defaultStr ++ nameStr ++ " :: " ++ typeStr
  _ -> "Not implemented"


--------------------------
-- Functions for DataDecl
--------------------------

prettyLHsQTyVars :: LHsQTyVars GhcPs -> String
prettyLHsQTyVars (HsQTvs _ tyVars) = unwords $ map prettyLHsTyVar tyVars

prettyLHsTyVar :: LHsTyVarBndr () GhcPs -> String
prettyLHsTyVar (L _ (UserTyVar _ _ (L _ name))) = occNameString $ occName name
prettyLHsTyVar _ = "unknown"


prettyHsDataDefn :: HsDataDefn GhcPs -> String
prettyHsDataDefn (HsDataDefn _ _ _ _ kind cons derv) = 
  -- intercalate " | " (map prettyLConDecl cons) ++ "\n" ++ prettyHsDeriving derv
  intercalate " | " (map prettyLConDecl cons) ++ "\n" ++ deriv 
    where deriv = if not (null derv) then prettyHsDeriving derv else ""

prettyHsDeriving :: [LHsDerivingClause GhcPs] -> String
prettyHsDeriving clauses = "\tderiving (" ++ intercalate ", " (map prettyHsDerivingClause clauses) ++ ")"


prettyHsDerivingClause :: LHsDerivingClause GhcPs -> String
prettyHsDerivingClause (L _ (HsDerivingClause _ strat ((L _ typ)))) =
  prettyDerivStrategy strat  ++ prettyDerivClauseTys typ

prettyDerivClauseTys :: DerivClauseTys GhcPs -> String
prettyDerivClauseTys tys = case tys of
  DctSingle _ ty -> prettyLHsSigType ty
  DctMulti _ typs -> intercalate ", " (map prettyLHsSigType typs)

prettyDerivStrategy :: Maybe (LDerivStrategy GhcPs) -> String
prettyDerivStrategy Nothing = ""
prettyDerivStrategy (Just (L _ strategy)) = case strategy of
  NewtypeStrategy _ -> "newtype "
  _ -> "not implemented"

 
prettyLConDecl :: LConDecl GhcPs -> String
prettyLConDecl (L _ (ConDeclH98 _ name _ _ _ details _)) = 
  occNameString (occName (unLoc name))++ " " ++ prettyHsConDetails (prettyHsConDeclH98Details details)


prettyHsConDeclH98Details :: HsConDeclH98Details GhcPs -> HsConDetails Void (HsScaled GhcPs (LHsType GhcPs)) (Located [LConDeclField GhcPs])
prettyHsConDeclH98Details details = case details of
  PrefixCon _ args -> PrefixCon [] args
  InfixCon arg1 arg2 -> InfixCon arg1 arg2
  -- RecCon fields -> RecCon 


prettyHsConDetails :: HsConDetails Void (HsScaled GhcPs (LHsType GhcPs)) (Located [LConDeclField GhcPs]) -> String
prettyHsConDetails details = case details of
  PrefixCon tyArgs arg -> unwords $ map prettyLHsType $ getLHsTypes arg
  InfixCon (HsScaled _ arg1) (HsScaled _ arg2) -> prettyLHsType arg1 ++ " " ++ prettyLHsType arg2
  _ -> "Not implemented"



-------------
-- HsTypes
-------------

getLHsTypes :: [HsScaled GhcPs (LHsType GhcPs)] -> [LHsType GhcPs]
getLHsTypes = map getLHsType

getLHsType :: HsScaled GhcPs (LHsType GhcPs) -> LHsType GhcPs
getLHsType (HsScaled _ ty) = ty



prettyLHsSigWcType :: LHsSigWcType GhcPs -> String
prettyLHsSigWcType = \case
  HsWC ext body -> prettyLHsSigType body

prettyLHsSigType :: LHsSigType GhcPs -> String
prettyLHsSigType arg = prettyHsSigType (unXRec @(GhcPass 'Parsed) arg)

prettyHsSigType :: HsSigType GhcPs -> String
prettyHsSigType = \case 
  HsSig ext bndrs body -> prettyHsOuterSigTyVarBndrs bndrs ++ prettyLHsType body
-- prettyHsSigType = \case
--   HsSig ext bndrs body -> prettyLHsType body

prettyHsOuterSigTyVarBndrs :: HsOuterSigTyVarBndrs GhcPs -> String
prettyHsOuterSigTyVarBndrs thing = case thing of
  HsOuterImplicit _ -> ""
  HsOuterExplicit _ bndrs -> unwords $ map processLTyVarBndr bndrs

processLTyVarBndr ::LHsTyVarBndr flag GhcPs -> String
processLTyVarBndr (L _ bndr) = processTyVarBndr bndr

processTyVarBndr :: HsTyVarBndr flag GhcPs -> String
processTyVarBndr (UserTyVar _ _ (L _ name)) = occNameString $ occName name
processTyVarBndr (KindedTyVar _ _ (L _ name) _) = occNameString $ occName name



prettyLHsType :: LHsType GhcPs -> String
prettyLHsType arg = prettyHsType (unXRec @(GhcPass 'Parsed) arg)


prettyHsType :: HsType GhcPs -> String
prettyHsType = \case
  HsFunTy _ _ arg1 arg2 -> prettyLHsType arg1 ++ " -> " ++ prettyLHsType arg2   -- for SigTyp (function signature) I think
  HsTyVar _ _ typ -> occNameString . occName . unLoc $ typ    -- Type Variables (Int -> Int)
  HsAppTy _ typ1 typ2 -> prettyLHsType typ1 ++ " " ++ prettyLHsType typ2
  HsParTy _ typ -> prettyLHsType typ
  HsOpTy _ _ typ1 id typ2 -> prettyLHsType typ1 ++ (occNameString . occName . unLoc $ id) ++ prettyLHsType typ2
  HsListTy _ typ -> "[" ++ prettyLHsType typ ++ "]"   -- list type
  HsQualTy _ context typ -> prettyLHsContext context ++ " => " ++ prettyLHsType typ
  -- TODO: HsForAllTy, HsQualTy, HsAppKindTy, HsTupleTy, HsSumTy, HsIParamTy, HsStarTy, HsKindSig
  --       HsSpliceTy, HsDocTy, HsBangTy, HsRecTy, HsExplicitListTy, HsExplicitTupleTy, HsTyLit, 
  --       HsWildCardTy
  _ -> "Not implemented" 



prettyLHsContext :: LHsContext GhcPs -> String
prettyLHsContext (L _ context) = "(" ++ intercalate ", " (map prettyLHsType context) ++ ")"


-------------------------
-- Matches (for FunBind)
-------------------------

prettyMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> String
prettyMatchGroup = \case
  MG ext (L _ matches) _ -> unlines $ map prettyLMatch matches

prettyLMatch :: LMatch GhcPs (LHsExpr GhcPs) -> String
prettyLMatch (L _ match) = case match of 
  Match _ _ pats body -> unwords (map prettyPat pats) ++ " -> " ++ prettyGRHSs body


------------------------------
-- Pat (for Pattern Matching)
------------------------------

prettyPat :: LPat GhcPs -> String
prettyPat (L _ pat) = case pat of
  VarPat _ typ -> occNameString . occName . unLoc $ typ
  LitPat _ pat -> prettyHsLit pat
  WildPat _ -> "_"    -- for wildcards
  ListPat _ pats -> "[" ++ intercalate "," (map prettyPat pats) ++ "]"    -- for list patterns, will have to change to : pattern for lean
  NPat _ lit _ _ ->  prettyOverLit (unXRec @(GhcPass 'Parsed) lit)    -- for n (0)
  -- ParPat for (x : xs)
  ParPat _ tokLeft pat tokRight -> "(" ++ prettyPat pat ++ ")"
  ConPat _ (L _ name) details ->
    let conName = occNameString . occName $ name
        patDetails = prettyHsConPatDetails details
    in case details of
      PrefixCon _ _ -> conName ++ " " ++ unwords patDetails 
      InfixCon _ _ -> intercalate (" " ++ conName ++ " ") patDetails    -- for : lists (i.e. (x : xs))
      -- TODO: RecCon
  _ -> "Not implemented"


prettyHsConPatDetails :: HsConPatDetails GhcPs -> [String]
prettyHsConPatDetails details = case details of
  PrefixCon tyarg arg -> map prettyPat arg
  InfixCon arg1 arg2 -> [prettyPat arg1, prettyPat arg2]
  -- TODO: RecCon
  -- RecCon (HsRecFields fields _)  ->  map (\(L _ (HsRecFieldCon (L _ name) pat _ _)) -> (occNameString . occName $ name) ++ " = " ++ prettyPat pat) fields



------------------------
-- GRHS and HsExpr
------------------------

prettyGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> String
prettyGRHSs (GRHSs _ grhss binds) = unwords ( map prettyGRHS grhss)  ++ bindr -- prettyLocalBinds binds
  where
    -- formating, don't know if it will work for all things, but works for month function
    bindr = if prettyLocalBinds binds == "" then "" else "\n\twhere\n" ++ "\t\t" ++ intercalate "\n\t\t" (lines $ prettyLocalBinds binds)

prettyGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> String
prettyGRHS (L _ (GRHS _ guardStmt body)) =  -- prettyLHsExpr body
  let guardStr = if null guardStmt then " = " else " | " ++ prettyGuardStmt guardStmt ++ " = "
      exprStr = if null guardStmt then prettyLHsExpr body else prettyLHsExpr body ++ "\n"
  in guardStr ++ exprStr
  -- in guardStr  ++ prettyLHsExpr body 
    
-- hasGuards checks for guards in GRHSs constructor
-- called by FunBind in HsDecl to ensure proper indentation
hasGuards :: GRHSs GhcPs (LHsExpr GhcPs) -> Bool
hasGuards (GRHSs _ grhss _ ) = not (all ( null . grhssGuards) grhss)
  where
    grhssGuards (L _ (GRHS _ guards _)) = guards


-- Not sure what these two functions are for if where and let are in LocalBinds
prettyGuardStmt :: [GuardLStmt GhcPs] -> String                                                                                                                                                  
prettyGuardStmt stmts = intercalate ", "  (map prettyStmt stmts)                                                                                                                        
                                                                                                                                                                                                  
prettyStmt :: GuardLStmt GhcPs -> String                                                                                                                                                         
prettyStmt (L _ stmt) = case stmt of                                                                                                                                                             
  BodyStmt _ expr _ _ -> prettyLHsExpr expr                                                                                                                                                     
  LetStmt _ binds -> "local binds not implemented" --prettyLocalBinds binds                                                                                                                                                      
  BindStmt _ pat expr -> "bind stmt not implemented" -- prettyPat pat ++ " | " ++ prettyLHsExpr expr                                                                                                                            
  _ -> "Not implemented"      

----------------

-- LocalBinds for let and where
prettyLocalBinds :: HsLocalBinds GhcPs -> String
prettyLocalBinds binds = case binds of
  EmptyLocalBinds _ -> ""
  HsValBinds _ (ValBinds _ bindlst sigs) ->  prettyHsValBinds  bindlst sigs
    -- let bindList = bagToList binds
  HsIPBinds _ ipBinds -> "Implicit parameter binds not implemented"


-- TODO: too many newlines, figure out which to get rid of
prettyHsValBinds :: LHsBindsLR GhcPs GhcPs -> [LSig GhcPs] -> String
prettyHsValBinds binds sigs =
  let bindList = bagToList binds
      prettyBinds = map prettyLHsBindLR bindList
      -- prettySigs = intercalate "\n" . map (prettySig . unLoc) $ sigs
      prettySigs = map (prettySig . unLoc) $ sigs
  -- in intercalate "\n" prettyBinds  ++ prettySigs
  in intercalate "" prettyBinds ++ unlines prettySigs


prettyLHsBindLR :: LHsBindLR GhcPs GhcPs -> String
prettyLHsBindLR (L _ bind) = case bind of
  FunBind _ id matches _ -> 
    let funName = (occNameString . occName . unXRec @(GhcPass 'Parsed)) id -- no gshow = no "" around function name
        matchStrings = map (\(L _ (Match _ c pats body)) ->
          let argStrings = funName ++ " " ++ unwords (map prettyPat pats)
              bodyString = prettyGRHSs body
              -- indent = if hasGuards body then replicate (length argStrings) ' ' else ""
          in argStrings ++ bodyString) (unLoc $ mg_alts matches)
        
    in  unlines matchStrings
  PatBind {} -> "patBind not implemented"
  VarBind {} -> "VarBind not implemented"
  PatSynBind {} -> "PatSynBind not implemented"
  -- _ -> "not implemented"



prettyLHsExpr :: LHsExpr GhcPs -> String
prettyLHsExpr expr = prettyHsExpr (unXRec @(GhcPass 'Parsed) expr)


prettyHsExpr :: HsExpr GhcPs -> String 
prettyHsExpr = \case
  HsVar _ name -> occNameString . occName . unLoc $ name   -- variable names
  HsLit _ lit -> prettyHsLit lit  -- for literals in code (ints, strings, etc.)
  HsApp _ expr1 expr2 -> prettyHsExpr (unLoc expr1) ++ " " ++ prettyHsExpr (unLoc expr2) -- apply a function to an argument (i.e. f x) -- recursiveAdd n (...)
  OpApp _ expr1 op expr2 -> prettyHsExpr (unLoc expr1) ++ " " ++ prettyHsExpr (unLoc op) ++ " " ++ prettyHsExpr (unLoc expr2) -- for operaor expr (i.e. n + m)
  HsPar _ tok1 exp tok2 -> "( " ++ prettyLHsExpr exp ++ " )"  -- parenthesized expressions (i.e. m - 1)
  HsOverLit _ lit -> prettyOverLit lit    -- adds the literals in function body (1 + recursiveAdd n (m - 1))
  ExplicitList _ exprs -> "[" ++ intercalate  ", " (map prettyLHsExpr exprs) ++ "]"
  HsIf _ exp1 exp2 exp3 -> "if " ++ prettyLHsExpr exp1 ++
                           " then " ++ prettyLHsExpr exp2 ++
                           " else " ++ prettyLHsExpr exp3
  HsCase _ exp matches -> "case " ++ prettyLHsExpr exp ++ " of \n" ++ indent (prettyMatchGroup matches)
  HsLet _ tok1 binds tok2 exp -> "\n\tlet\n" ++ indent(indent (prettyLocalBinds binds)) ++ "\tin " ++ prettyHsExpr (unLoc exp)
  -- TODO: HsLam, NegApp, ExplicitTuple, ExplicitSum, HsMultiIf, HsDo, ExplicitList
  -- TODO: others tbd
  _ -> "Not implemented"

indent :: String -> String
indent = unlines . map ("\t" ++) . lines

---------------------------
-- Lit (OverLit, HsLit)
---------------------------

prettyOverLit :: HsOverLit GhcPs -> String
prettyOverLit (OverLit _ val) = case val of
  HsIntegral (IL _ _ i) -> gshow i
  -- TODO: account for negative fractionals
  HsFractional (FL _ neg signi exp expBase) -> --show (fromRational (negate * (signi * (10 ^  exp))))
    -- where negate = if neg then -1 else 1
    let sign = if neg then -1 else 1
        factor = if exp < 0 then 1 / (10 ^ abs exp) else 10 ^ exp   -- to prevent panic! (from neg exponenets)
    in show (fromRational (sign * (signi * factor)))
  HsIsString _ s -> gshow s


prettyHsLit :: HsLit GhcPs -> String
prettyHsLit = \case
  HsChar _ char -> gshow char
  HsString _ str -> gshow(unpackFS str)
  HsInt _ (IL _ _ int) -> gshow int   -- might be wrong
  HsInteger _ int _ -> gshow int
  -- TODO: HsRat
  _ -> "Not implemented"  -- not the missing piece ( the 1 )

