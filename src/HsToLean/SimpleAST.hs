{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

module HsToLean.SimpleAST(generateSimpleAST) where
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
import Data.List (intercalate, isPrefixOf, tails, findIndex)
import Data.List.Split (splitOn)
import "ghc" GHC.Types.SourceText
import Data.Void
import GHC.Runtime.Eval (Term(ty))
import "ghc" GHC.Data.FastString (unpackFS)
import Data.Bool (Bool)
import "ghc" GHC.Data.Bag (bagToList, Bag)
import "ghc" GHC.Hs.Binds
import Data.Ratio ((%))



generateSimpleAST :: ParsedSource -> IO()
generateSimpleAST ast = do
    let ps = map (unXRec @(GhcPass 'Parsed)) $ hsmodDecls $ unLoc $ ast
        parsedDecls = map astDecl ps
        sortedDeclList = sortDeclList parsedDecls
    liftIO $ writeFile "SimplifiedAST.txt" (unlines sortedDeclList)


sortDeclList :: [String] -> [String]
sortDeclList [] = []
sortDeclList [x] = [x]
sortDeclList (x : y : xs) = 
    let l = words x
        isTypeSig = getFirstElement l == "TySig" 
    in if isTypeSig then ("(" ++ x ++ " (" ++ y ++ " ))") : sortDeclList xs else ("(" ++ x ++ ")") : sortDeclList (y: xs)
    

getFirstElement :: [String] -> String
getFirstElement [] = ""
getFirstElement (x : _ ) = x


-- Takes simple/custom constructor name and list of values 
-- used by astHsSigType to add "TypeVar" before each type variable string
prependConstructor :: String -> [String] -> [String]
prependConstructor prefix = map (prefix ++) 

astDecl :: HsDecl GhcPs -> String
astDecl = \case
    TyClD _ decl -> astTyClDecl decl
    ValD _ decl -> astHsBind decl
    SigD _ decl -> astSig decl
    _ -> "HsDecl Not Implemented"
            
    
--------------------------------------
-- Functions for HsDecl Constructors
--------------------------------------
astTyClDecl :: TyClDecl GhcPs -> String
astTyClDecl decl = case decl of
    SynDecl _ name tyVar fix rhs -> "SynDecl " ++ "Not Implemented"
    DataDecl _ name tyVar fix dataDef -> "DataDecl " ++ "Not Implemented"


astHsBind :: HsBind GhcPs -> String
astHsBind decl = case decl of
    FunBind _ name matches _ -> 
        let funName = "(FunName " ++ (occNameString . occName . unXRec @(GhcPass 'Parsed)) name ++ ")"
            matchStrings = map (\(L _ (Match _ _ pats body)) ->
                let boundVar = "[" ++ intercalate ", " (map astPat pats) ++ "] "
                    bodyExpr = "(GRHSs " ++ "Not Implemented" ++ ") "
                in boundVar ++ bodyExpr ) (unLoc $ mg_alts matches) -- do I need a matches constructor?

            -- matchPats = map (\(L _ (Match _ _ pats body)) -> (map unLoc pats)) (unLoc $ mg_alts matches)
            -- matchGRHSs = map (\(L _ (Match _ _ _ body)) -> "GRHSs Not Implemented") (unLoc $ mg_alts matches)
            -- boundVar = findBoundVar matchPats
        in "FBind " ++ funName ++ " " ++ "[" ++ intercalate ", " matchStrings ++ "] "-- FunBind constructor turned to FBind to avoid name conflicts
    PatBind _ pat rhs _ -> "PatBind " ++ "Not Implemented"
    VarBind _ var rhs -> "VarBind " ++ "Not Implemented"
    _ -> "HsBind Not Implemented"


findBoundVar :: [[Pat GhcPs]] -> [Pat GhcPs]
findBoundVar [] = []
findBoundVar (x : xs) = []



-----------------------------------
-- Function(s) for Sig Constructors
-----------------------------------

astSig :: Sig GhcPs -> String
astSig decl = case decl of
    TypeSig _ names typ -> 
        let typeName = "(TypeName " ++ unwords (map (occNameString . occName . unXRec @(GhcPass 'Parsed)) names) ++ ")"
            -- sigTypes =  intercalate ") " (map ("("++) (astSigType typ))
            sigTypes = unwords (astSigType typ)
        in "TySig  " ++  typeName ++ " " ++ sigTypes -- ++ ")"
    PatSynSig _ names typ -> "PatSynSig " ++ "Not Implemented"
    ClassOpSig _ isDefault names typ -> "ClassOpSig " ++ "Not Implemented"
    _ -> "Sig Not Implemented"


--------------------------------------------------
-- Functions for SigType and HsType Constructors
--------------------------------------------------

-- calls astHsSigType to unwind sigTypes
astSigType :: LHsSigWcType GhcPs -> [String]
astSigType = \case
    HsWC ext body -> astHsSigType (unXRec @(GhcPass 'Parsed) body)


astHsSigType :: HsSigType GhcPs -> [String]
astHsSigType = \case
    HsSig ext bndrs body -> --["(OuterTyVarBndrs " ++ "Not Implemented" ++ ") " , astHsType (unLoc body) ]--prependConstructor "TypeVar " (words (astHsType (unLoc body)))
        let astTyps = astHsType (unLoc body)
            hasContext = (getHeadStringList (words astTyps)) == "(QualifiedTy"
        in ["(OuterTyVarBndrs " ++ "Not Implemented" ++ ") ", (if hasContext then astTyps else ("(QualifiedTy Null) " ++ astTyps))]


astHsType :: HsType GhcPs -> String
astHsType = \case
    HsFunTy _ _ arg1 arg2 ->  -- "FunType (" ++ astHsType (unXRec @(GhcPass 'Parsed) arg1) ++ ") " ++ "(" ++ astHsType (unXRec @(GhcPass 'Parsed) arg2) ++ ") "-- for function types (e.g. Int -> Int). Only
        let typList = (unXRec @(GhcPass 'Parsed) arg1) : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
        in "(FunType " ++ "[" ++ intercalate ", " (map astHsType typList) ++ "] " ++ ") "
    HsTyVar _ _ typ -> "(TypeVar \"" ++ (occNameString . occName . unLoc $ typ) ++ "\") "
    HsAppTy _ typ1 typ2 -> "(AppTy " ++ astHsType (unXRec @(GhcPass 'Parsed) typ1) ++ astHsType (unXRec @(GhcPass 'Parsed) typ2) ++ ") "
    HsQualTy _ context typ -> "(QualifiedTy " ++  astContext context ++") " ++ astHsType (unXRec @(GhcPass 'Parsed) typ) 
    HsParTy _ typ -> "(ParaTy " ++ astHsType (unXRec @(GhcPass 'Parsed) typ) ++ ")"
    HsOpTy _ _ typ1 id typ2 -> "(OperatorTy " ++ "(Operator \"" ++ (occNameString . occName . unLoc $ id) ++ "\") " ++ astHsType (unXRec @(GhcPass 'Parsed) typ1) ++ astHsType (unXRec @(GhcPass 'Parsed) typ2)
    _ -> "HsType Not Implemented"


extractArgs :: HsType GhcPs -> [HsType GhcPs]
extractArgs (HsFunTy _ _ arg1 arg2) = unXRec @(GhcPass 'Parsed) arg1 : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
extractArgs x = [x]


astContext :: LHsContext GhcPs -> String
astContext (L _ context) = "(Context [" ++ intercalate ", " (map astHsType (map (unXRec @(GhcPass 'Parsed)) context)) ++ "]) "


-----------------------------------------
-- Functions for Pat (Pattern Matching)
-----------------------------------------

astPat :: LPat GhcPs -> String
astPat (L _ pat) = case pat of
    VarPat _ typ -> "(VarPatt \"" ++ (occNameString . occName . unLoc $ typ) ++ "\") "
    LitPat _ pat -> "(LitPatt " ++ "Litrl Not Implemented" ++ ") "
    ListPat _ pats -> "(ListPatt " ++ "[" ++ intercalate "," (map astPat pats) ++ "]) "  
    ConPat _ (L _ name) details ->
        let conName = occNameString . occName $ name
            patDetails = astConPatDetails details 
        in case details of
            PrefixCon _ _ -> "(ConPatt " ++ conName ++ " " ++ unwords patDetails ++ ") "
            InfixCon _ _ -> "(ConPatt " ++ intercalate (" " ++ conName ++ " ") patDetails ++ ") "
    ParPat _ _ pat _ -> "(ParPatt " ++ astPat pat ++ ") "
    -- NPat _ lit _ _ ->
    _ -> "Patt Not Implemented"


astConPatDetails :: HsConPatDetails GhcPs -> [String]
astConPatDetails details = case details of
    PrefixCon tyarg arg -> map astPat arg
    InfixCon arg1 arg2 -> [astPat arg1, astPat arg2]



---------------------------
-- Helper Functions
---------------------------

-- get the head of a list of strings
getHeadStringList :: [String] -> String
getHeadStringList [] = ""
getHeadStringList (x : _) = x

main = do
    putStrLn "Hello World"