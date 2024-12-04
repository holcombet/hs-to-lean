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
import "ghc" GHC.Plugins ( occNameString, HasOccName(occName), isBoxed, rdrNameOcc)
import Data.String (String)
import Data.List (intercalate, isPrefixOf, tails, findIndex, transpose)
import Data.List.Split (splitOn)
import "ghc" GHC.Types.SourceText
import Data.Void
import GHC.Runtime.Eval (Term(ty))
import "ghc" GHC.Data.FastString (unpackFS)
import Data.Bool (Bool)
import "ghc" GHC.Data.Bag (bagToList, Bag)
import "ghc" GHC.Hs.Binds
import Data.Ratio ((%))
import GHC.Base (String)
import Data.Kind (FUN)



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


---------------------------------------------------------------------
-- Function that Triggers translations depending on Decl Constructor
---------------------------------------------------------------------

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
    SynDecl _ name tyVar fix rhs -> --"SynDecl " ++ "Not Implemented"
        let synName = "(SynName " ++ (occNameString . occName . unXRec @(GhcPass 'Parsed)) name ++ ") "
            tyVarString = "(QualTyVar " ++ astLHsQTyVars tyVar ++ ") "
            rhsStr = "(SynBody " ++  astHsType (unLoc rhs) ++ ") "
        in "SynDecls " ++ synName ++ " " ++ tyVarString ++ rhsStr 

    DataDecl _ name tyVar fix dataDef -> 
        let dataName = "(DataName " ++ (occNameString . occName . unXRec @(GhcPass 'Parsed)) name ++ ") "
            tyVarStr = "(QualTyVar " ++ astLHsQTyVars tyVar ++ ") "
            dataDefStr = astHsDataDefn dataDef 
            typedata = astGetHsDataDefnNewOrData dataDef 
        in "DataDecls " ++ typedata ++ dataName ++ tyVarStr ++ dataDefStr 



astHsBindName :: HsBind GhcPs -> String 
astHsBindName decl = case decl of
    FunBind _ name matches _ ->  ((occNameString . occName . unXRec @(GhcPass 'Parsed)) name)
    _ -> "HsBindName Not Implemented"


astHsBind :: HsBind GhcPs -> String
astHsBind decl = case decl of
    FunBind _ name matches _ -> 
        let funName = "(FunName " ++ (occNameString . occName . unXRec @(GhcPass 'Parsed)) name ++ ")"
            matchStrings = map (\(L _ (Match _ _ pats body)) ->
                let boundVar = "(Patts [" ++ intercalate ", " (map astPat pats) ++ "]) "
                    bodyExpr = "(GRHSs " ++ astGRHSs body ++ ") "
                in boundVar ++ bodyExpr ) (unLoc $ mg_alts matches) 

           
            matchLPats = map (\(L _ (Match _ _ pats _)) -> pats) (unLoc $ mg_alts matches)

            boundVar = filterValidPatt $ boundVarToString $ transposeBoundVar matchLPats

        in "FBind " ++ funName ++ " " ++ ("(PatArgs [" ++ intercalate ", " boundVar ++ "]) ") ++ "(Matches " ++ "[ " ++ intercalate ", " matchStrings ++ "]) "  
    PatBind _ pat rhs _ -> "PatBind " ++ "Not Implemented"
    VarBind _ var rhs -> "VarBind " ++ "Not Implemented"
    _ -> "HsBind Not Implemented"



--------------------------------------------------------------
-- Functions for GRHSs (for FunBind)

astGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> String 
astGRHSs (GRHSs _ grhss binds) = "(GuardRHSs [" ++ intercalate ", " (map astGRHS grhss) ++ "]) " ++ "(LBinds " ++ bndr ++ ") " 
    where
        bndr = if astLocalBinds binds == "" then "EmptyLocBinds " else "LocalBinds Not Implemented"



astGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> String 
astGRHS (L _ (GRHS _ guardStmt body)) =
    let guardStmts = if null guardStmt then " [] " else ("(Stmts " ++ astGuardStmt guardStmt ++ ") ")
        exprStr = "(GuardBody " ++ astLHsExpr body ++ ") "
    in guardStmts ++ exprStr



astGuardStmt :: [GuardLStmt GhcPs] -> String 
astGuardStmt stmts = "GuardStmts Not Implemented"


astLocalBinds :: HsLocalBinds GhcPs -> String 
astLocalBinds binds = case binds of 
    EmptyLocalBinds _ -> ""
    HsValBinds _ (ValBinds _ bindlst sigs) -> "ValBinds Not Implemented"
    HsIPBinds _ ipBinds -> "Impliciat Parameter Binds Not Implemented"
---------------------------------------------------
-- Functions to make list of variables for TypeSig
-- mostly related to FunBind


-- transpose nested list to group naming options from each pattern matching line in function body
transposeBoundVar :: [[LPat GhcPs]] -> [[LPat GhcPs]]
transposeBoundVar = transpose


-- translate it all to strings
boundVarToString :: [[LPat GhcPs]] -> [[String]]
boundVarToString [] = []
boundVarToString (x : xs) =
    let opt = findValidPattNames (map astPat x)
    in opt : boundVarToString xs


-- find all the valid variables in each inner list (will likely be VarPatt)
-- TODO: think of cases where it would not be VarPatt(?)
filterValidPatt :: [[String]] -> [String]
filterValidPatt xs = map chooseVar xs


-- returns the (first) instance of VarPatt
-- if there is none in the list, returns ""
chooseVar :: [String] -> String
chooseVar [] = ""
chooseVar (x : xs) =
    let pattTy = words x
        retVar 
            | head pattTy == "(VarPatt" = x
            -- | head pattTy == "(ConPattInfix" = x 
            | otherwise =  chooseVar xs
    in retVar

-----------------------------------------------------------------------------------
-- Functions that get the valid (i.e. non PrefixCon) strings and returns as a list

findValidPattNames :: [String] -> [String]
findValidPattNames [] = []
findValidPattNames (x:xs) =
    let terms = words x
        name  
            | head terms == "(VarPatt" =  x 
            | head terms == "(ParPatt" && head (tail terms) == "(ConPattInfix"= getConPattVal (unwords (tail terms))
            | head terms == "(ConPattInfix" = getConPattVal x 
            | otherwise =  "NA"

    in name : findValidPattNames xs


-- gets the last "item" of ConPattInfix (the list name)
-- returns the type (usually VarPatt) and the name (such as xs)
getConPattVal :: String -> String
getConPattVal "" = ""
getConPattVal x = 
    let terms = words x
        varName = getNthElement 6 terms ++ " " ++  getNthElement 7 terms
    in varName



-- helper function to get an element at a specific position of a list 
-- for getting PatArgs
getNthElement :: Int -> [String] -> String 
getNthElement a [] = "NA"
getNthElement a (x : xs)    | a <= 0 = "NA"
                            | a == 1 = x
                            | a > 1 = getNthElement (a-1) xs





--------------------------------------------------------------
--------------------------------------------------------------

    
------------------------------------------------------------
-- DataDecl Helper Functions
------------------------------------------------------------

-- function for determining if data decl is data or newtype
astGetHsDataDefnNewOrData :: HsDataDefn GhcPs -> String 
astGetHsDataDefnNewOrData (HsDataDefn _ nod _ _ _ _ _) = case nod of 
    NewType -> "(DefnType \"newtype\") " 
    DataType -> "(DefnType \"data\") "

-- function for the body of data declaration/definition
astHsDataDefn :: HsDataDefn GhcPs -> String
astHsDataDefn (HsDataDefn _ nod contxt _ kind cons derv) =
    let dataCons = intercalate ", " (map astLConDecl cons)
        deriv = "(DerivClause " ++ (if not (null derv) then astHsDeriving derv else "[]") ++ ") "
    in "(DataDefnCons " ++ "[" ++ dataCons ++ "]) " ++ deriv


-- for the constructor declarations of data definition
astLConDecl :: LConDecl GhcPs -> String
astLConDecl (L _ conDecl) = case conDecl of 
    ConDeclH98 _ name _ _ _ detail _ ->
        "(ConsName " ++ occNameString (occName (unLoc name)) ++ ") " ++ " " ++ astHsConDetails (astHsConDeclH98Details detail)
    ConDeclGADT _ names _ _ _ typ _ ->
        let conNames = intercalate ", " (map (occNameString . occName . unLoc) names)
            conType = astHsType (unLoc typ)
        in "(ConsName " ++ conNames ++ ") " ++ "(" ++ conType ++ ") "

---------
-- helper functions for astLConDecl
astHsConDeclH98Details :: HsConDeclH98Details GhcPs -> HsConDetails Void (HsScaled GhcPs (LHsType GhcPs)) (Located [LConDeclField GhcPs])
astHsConDeclH98Details details = case details of 
    PrefixCon _ args -> PrefixCon [] args 
    InfixCon arg1 arg2 -> InfixCon arg1 arg2 
    RecCon (L _ fields) -> RecCon (L noSrcSpan fields)
    -- TODO: RecCon 

astHsConDetails :: HsConDetails Void (HsScaled GhcPs (LHsType GhcPs)) (Located [LConDeclField GhcPs]) -> String 
astHsConDetails details = case details of 
    PrefixCon tyArgs arg -> "(ConDetails [" ++ (intercalate ", " $ map astHsType (map unLoc (astGetLHsTypes arg))) ++ "]) "
    InfixCon (HsScaled _ arg1) (HsScaled _ arg2) -> "InfixCon Not Implemented"
    RecCon (L _ fields) -> "RecCon Not Implemented"


astGetLHsTypes :: [HsScaled GhcPs (LHsType GhcPs)] -> [LHsType GhcPs]
astGetLHsTypes = map astGetLHsType


astGetLHsType :: HsScaled GhcPs (LHsType GhcPs) -> LHsType GhcPs 
astGetLHsType (HsScaled _ ty) = ty
--------

-----------
-- helper functions for astHsDeriving

-- for getting the functions being derived (will be more complicated later because Lean doesn't have same deriving constructors)
astHsDeriving :: [LHsDerivingClause GhcPs] -> String 
astHsDeriving clauses = "[" ++ intercalate ", " (map astHsDerivingClause clauses) ++ "] "

astHsDerivingClause :: LHsDerivingClause GhcPs -> String 
astHsDerivingClause (L _ (HsDerivingClause _ strat ((L _ typ)))) =
    intercalate ", " (astDerivClauseTys typ)  

astDerivStrategy :: Maybe (LDerivStrategy GhcPs) -> String 
astDerivStrategy Nothing = ""
astDerivStrategy (Just (L _ strategy)) = ""     -- Lean doesn't have any deriving strategies, I don't believe


astDerivClauseTys :: DerivClauseTys GhcPs -> [String]
astDerivClauseTys tys = case tys of 
    DctSingle _ ty -> [astDerivSig (unLoc ty)]
    DctMulti _ typs -> map astDerivSig (map unLoc typs)


astDerivSig :: HsSigType GhcPs -> String
astDerivSig = \case 
    HsSig _ _ body -> astHsType (unLoc body )
-------------

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


-- function that makes all types in FunTy a list of types (for HsFunTy)
extractArgs :: HsType GhcPs -> [HsType GhcPs]
extractArgs (HsFunTy _ _ arg1 arg2) = unXRec @(GhcPass 'Parsed) arg1 : extractArgs (unXRec @(GhcPass 'Parsed) arg2)
extractArgs x = [x]

-- gets context for HsQualTy
astContext :: LHsContext GhcPs -> String
astContext (L _ context) = "(Context [" ++ intercalate ", " (map astHsType (map (unXRec @(GhcPass 'Parsed)) context)) ++ "]) "


-- gets types for SynDecl
astLHsQTyVars :: LHsQTyVars GhcPs -> String
astLHsQTyVars (HsQTvs _ tyVars) = "[" ++ intercalate ", " (map astLHsTyVar tyVars) ++ "] "

astLHsTyVar :: LHsTyVarBndr () GhcPs -> String 
astLHsTyVar (L _ (UserTyVar _ _ (L _ name))) = "(TyVar " ++ occNameString (occName name) ++ ") "

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
            PrefixCon _ _ -> "(ConPattPrefix " ++ ("(ConN \"" ++ conName ++ "\") ") ++ " " ++ "[" ++ intercalate ", "  patDetails ++ "] " ++ ") "
            InfixCon _ _ -> "(ConPattInfix " ++ intercalate (" " ++ ("(ConN \"" ++ conName ++ "\") ")++ " ") patDetails ++ ") "
    ParPat _ _ pat _ -> "(ParPatt " ++ astPat pat ++ ") "
    -- NPat _ lit _ _ ->
    _ -> "Patt Not Implemented"


astConPatDetails :: HsConPatDetails GhcPs -> [String]
astConPatDetails details = case details of
    PrefixCon tyarg arg -> map astPat arg
    InfixCon arg1 arg2 -> [astPat arg1, astPat arg2]



-----------------------------------------
-- Functions for HsExpr
-----------------------------------------

astLHsExpr :: LHsExpr GhcPs -> String 
astLHsExpr expr = astHsExpr (unXRec @(GhcPass 'Parsed) expr)

astHsExpr :: HsExpr GhcPs -> String 
astHsExpr = \case 
    HsVar _ name -> "(Var " ++ (occNameString . occName . unLoc $ name) ++ ") "
    OpApp _ expr1 op exp2 -> 
        let opStr = astLHsExpr op 
        in "(OperatorApp " ++ opStr ++ " " ++ astLHsExpr expr1 ++ astLHsExpr exp2 ++ ") "
    _ -> "HsExpr Not Implemented"





---------------------------
-- Helper Functions
---------------------------

-- get the head of a list of strings
getHeadStringList :: [String] -> String
getHeadStringList [] = ""
getHeadStringList (x : _) = x

main = do
    putStrLn "Hello World"