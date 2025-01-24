
{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

module TestAST where

import Data.List (intercalate)
import System.IO


data AST 
    = SignatureD Sigs
    | ValueD Binds
    | TyClassD TyClassDecls
    | EmptyD
    deriving (Eq, Show)
    


data Sigs 
    =   TySig 
            {
                ty_name :: String,
                -- outer_tyvar_bndrs :: OuterTypeVarBndr, -- TODO: figure out OuterTypeVarBndr
                qual_ty ::  Context,
                fun_type :: FunType,
                fun_bind :: Binds
            }
    | EmptySig
    deriving (Eq, Show)


type Context = [Types]
type FunType = [Types]


data Binds 
    = FBind 
        {
            fun_name :: String,
            patt_args :: [VarPatt],
            matches :: [MatchPair]   -- not a string, but temp
        }
    | EmptyB
    deriving (Eq, Show)


-- data MatchPair = MP {bound_var :: [Patts], guard_body :: GuardRHSs}
--     deriving (Eq, Show)


-- temp for testing
data MatchPair = MP {bound_var :: [Patts], guard_body :: GuardRHSs}
    deriving (Eq, Show)

-- data GuardRHSs = GuardRHSs {guard_stmt :: [Stmts], guard_expr :: Exprs, loc_binds :: LocBinds}
--                 | EmptyG
--     deriving (Eq, Show)

data GuardRHSs = Guards {guard_exprs :: [GuardRHS], loc_binds :: LocBinds}
                | EmptyG 
    deriving (Eq, Show)


data GuardRHS = StmtBody {guard_stmt :: [Stmts], guard_expr :: Exprs}
    deriving (Eq, Show)


data Stmts 
    = BodyStmts Exprs 
    | LetStmts LocBinds 
    | BindStmts Patts Exprs 
    | EmptyS
    deriving (Eq, Show)

data Exprs 
    = Var String 
    | SpecialVar SVar       -- special expressions that need translation b/w Haskell and Lean
    | App Exprs Exprs 
    | OperApp Exprs Exprs Exprs     -- left expr, operator, right expr
    | LetExpr LocBinds Exprs
    | ExpList [Exprs]
    | Litr Lits
    | Other
    deriving (Eq, Show)

data SVar 
    = VPutStrLn
    | VPutStr
    | VNothing 
    | VJust
    | VLeft
    | VRight
    | EmptyV 
    deriving (Eq, Show)
data LocBinds 
    = EmptyLocBinds 
    | ValsBinds [Binds] [Sigs]
    deriving (Eq, Show)


data Bindings 
    = FBinding 
        {
            f_name :: String,
            f_args :: [VarPatt],
            f_matches :: [MatchPair]
        }
    | EmptyBinding
    deriving (Eq, Show)

data TyClassDecls 
    = SynDecls
        {
            syn_name    :: String,
            qualTy_var  :: QTyVar,
            syn_body    :: Types
        }
    | DataDecls 
        {
            defn_type       :: NoD,
            data_name       :: String,
            qualTy_var      :: QTyVar,
            dataDefn_cons   :: [DefnConsDetails],
            deriv_clause    :: [Types]
        }
    | EmptyTC
    deriving (Eq, Show)


data NoD = NewTy | DataTy
    deriving (Eq, Show)

data DefnConsDetails = DefnConsDetail String ConsDetails
    deriving (Eq, Show)

type QTyVar = [String]

type ConsName = String 

type ConsDetails = [Types]

data Types 
    = TypeVar String
    | FunVar FVar               -- built-in function/monad that has diff name/operation
    | FType FunType
    | AppTy Types Types
    | ExpListTy [Types]
    | ListTy Types
    | ParaTy Types
    | EmptyT        -- placeholder
    deriving (Eq, Show)

data FVar 
    = LRational     
    | LEither
    -- | LLeft 
    -- | LRight
    | LMaybe
    | LAlphaA
    -- | LJust
    -- | LNothing
    | LEmpty   -- placeholder
    deriving (Eq, Show)
data Patts 
    = EmptyP
    | VariPatt VarPatt
    | LitPatt Lits
    | ParPatt Patts
    | ListPatt [Patts]
    | ConPatt 
        {
            con_type :: String,
            patt_details :: ConPattDetails
        }
    deriving (Eq, Show)

type ConPattDetails = ConPDetails 

type VarPatt = String
data ConPDetails 
    = ConPattPrefix 
        {
            p_arg :: [Patts]
        }
    | ConPattInfix
        {
            p_arg1 :: VarPatt,
            p_arg2 :: VarPatt
        }
    deriving (Eq, Show)


data Lits 
    = Chars Char 
    | Strings String 
    | Ints Integer
    | Fractionals Float
    deriving (Eq, Show)


-- Testing Functions -- 
astListToLean :: [AST] -> [String]
astListToLean = map astToLean 

astToLean :: AST -> String 
astToLean = \case 
    SignatureD s -> sigToLean s 
    ValueD v -> implicitBindsToLean v 
    TyClassD t -> tyClToLean t 
    _ -> ""


-- tester function to convert Sigs type to Lean code
sigToLean :: Sigs -> String
sigToLean = \case
    TySig tyName qualTy funTy funBind -> 
        let boundVar = getBoundVar funTy funBind
            funTyList = processFunType funTy
            retTy = last funTyList
            -- isValBind = head(processFunType funTy) == last(processFunType funTy)
            isValBind = length funTyList == 1
        -- in "def " ++ tyName ++ " " ++ boundVar ++ " : " ++ retTy ++ " := \n" ++ bindsToLean funBind
        -- in "def " ++ tyName ++ " " ++ boundVar ++ " : " ++ retTy ++ " := \n" ++ (if isValBind then valBindsToLean (getFirstTy (head funTy)) funBind else bindsToLean funBind)
        in "def " ++ tyName ++ " " ++ boundVar ++ " : " ++ retTy ++ " := \n" ++ (if isValBind then valBindsToLean tyName funBind else bindsToLean funBind)
    -- _ -> "Not Implemented"


-- functions for value binding functions
getFirstTy :: Types -> String
getFirstTy = \case 
    TypeVar v -> v 
    AppTy typ1 typ2 -> processType typ1

valBindsToLean :: String -> Binds -> String 
valBindsToLean ty bind = case bind of 
    -- FBind name args match -> ty ++ if not (null args) then " " ++ intercalate ", " ( args) else ""  ++ " = " ++ unlines (map matchpairToLean match)  -- intercalate ", " match  -- body not finished... but at least it's something
    FBind name args match -> unlines (map getConstantFunBody match)  -- intercalate ", " match  -- body not finished... but at least it's something
    _ -> ""


-------

implicitBindsToLean :: Binds -> String 
implicitBindsToLean = \case 
    FBind name args match -> 
        -- if length match == 1  then unlines ( map singleMatchPairToLean match)
        "def " ++ name ++ " " ++ unwords args ++ " := " ++ unlines (map singleMatchPairToLean match)
    _ -> ""

-- tester function to convert Binds type to Lean code
bindsToLean :: Binds -> String
bindsToLean = \case
    FBind name args match -> 
        
        -- if (null args || length match == 1)  then unlines ( map matchpairToLean match) -- intercalate ", "  match 
        if length match == 1  then unlines ( map singleMatchPairToLean match)
        else 
            let matchStmt = "\tmatch " ++ intercalate ", " ( args) ++ " "  ++ "with\n"
                matches = (map ("| " ++) (map matchpairToLean match))
            in matchStmt ++ "\t" ++ intercalate "\t" matches
    _ -> ""
       
{-
For let statements
-}
bindingsToLean :: Binds -> String 
bindingsToLean = \case 
    FBind name a e -> name ++" := " ++ unwords (map singleMatchPairToLean e)
    _ -> "Bindings Not Implemented" 

{-
getConstantFunBody, getFunBody, and getConst

functions for getting the a constant function body
-}
getConstantFunBody :: MatchPair -> String 
getConstantFunBody (MP pvar bod) = getFunBody bod

getFunBody :: GuardRHSs -> String
getFunBody = \case 
    EmptyG -> "GuardNotImplemented"
    Guards gs loc -> unlines (map getConst gs)

getConst :: GuardRHS -> String 
getConst (StmtBody sm expr) = processExprs expr
        

{-
singleMatchpairToLean, singleGuardRHSsToLean, and singleGuardRHSToLean 

functions for getting guard body for constant functions
-}

singleMatchPairToLean :: MatchPair -> String 
singleMatchPairToLean (MP pvar bod) =
    let bodyString = singleGuardRHSsToLean bod 
    in bodyString 

singleGuardRHSsToLean :: GuardRHSs -> String 
singleGuardRHSsToLean = \case 
    EmptyG -> "Guard Not Implemented"
    Guards gs loc ->
            let guards = map singleGuardRHSToLean gs
                bndr = if loc == EmptyLocBinds then "" else "local binds no implemented"
                -- trimGuardsExpr = words (unlines guards)
            -- in (if tail (unwords trimGuardsExpr) == "else" then init (unwords trimGuardsExpr) else unlines guards) ++ if bndr == "" then "" else "\n"
            in unlines guards ++ if bndr == "" then "" else "\n"

singleGuardRHSToLean :: GuardRHS -> String 
singleGuardRHSToLean (StmtBody sm epr) = 
    let guardStmts = if null sm then "" else 
            let stm = guardStmtsToLean sm 
            in if stm == "" then "" else stm ++ " then "
        exprStr = if null sm then processExprs epr else 
            let ex = processExprs epr 
            in if guardStmts == "" then "\t" ++ ex else ex ++ " else"

            -- if tail (words exprStr) == "else" then unwords (init )
    -- in guardStmts ++ if unwords (tail trimExprStr) == " else" then unwords (init trimExprStr) else exprStr
    in guardStmts ++ exprStr


{-
matchpairToLean, guardRHSsToLean, and guardRHSToLean 

functions for getting guard body for non-constant functions
-}

matchpairToLean :: MatchPair -> String
matchpairToLean (MP pvar bod) = 
    let args = intercalate ", " (processPattArgs pvar)
        bodystring = guardRHSsToLean bod
    in args ++ bodystring




{- works but adds an unwanted "else" if no otherwise-}
guardRHSsToLean :: GuardRHSs -> String 
guardRHSsToLean = \case 
    EmptyG -> "Guard Not Implemented"
    Guards gs loc -> 
        let guards = map guardRHSToLean gs 
            guardEnd = words  (last guards)
            bndr = if loc == EmptyLocBinds then "" else "local binds not implemented"
        in " => " ++ if last guardEnd == "else" then unlines (init guards) ++ "\t\t\t" ++ unwords (init guardEnd) else unlines guards ++ if bndr == "" then "" else "\n"
        -- in " => " ++ unlines guards ++ if bndr == "" then "" else "\n"
    
guardRHSToLean :: GuardRHS -> String 
guardRHSToLean (StmtBody sm epr) = 
    let guardStmts = if null sm then "" else --if null sm then " => " else --"Lean Guards not Implemented"
            let stm = guardStmtsToLean sm 
            in "\n\t\t" ++ if null stm ||  stm == "" then "" else stm ++ " then "
        exprStr = if null sm then processExprs epr else --"Lean Guards Not Implemented"
            let ex = processExprs epr 
            in if guardStmts == "" then "\t" ++ ex else ex ++ " else"
    in guardStmts ++ exprStr
{--}


{-
guardStmtsToLean

Converting guard stmts to lean (if statements)
-}

guardStmtsToLean :: [Stmts] -> String
guardStmtsToLean sts = intercalate ", " (map guardStmtToLean sts)

guardStmtToLean :: Stmts -> String 
guardStmtToLean = \case 
    BodyStmts e -> -- "if " ++ processExprs e 
        let exp = processExprs e 
        in if exp == "otherwise" then "" else "\tif " ++ exp
    LetStmts l -> "Let Stmt not implemented"
    BindStmts b e -> "Bind Stmt not implemented"
    EmptyS -> "Stmt not implemented"


{-
locBindsToLean

Converting local binds to Lean binding stmts/exprs
-}

processLocBindsToLean :: LocBinds -> String
processLocBindsToLean bind = case bind of 
    EmptyLocBinds -> ""
    ValsBinds bndlst sig -> 
        let bindlst =  map bindingsToLean bndlst
            sigs = map sigToLean sig 
            -- s = if null sigs then 
        in unlines bindlst ++ unlines sigs
        -- let bindlst = map bindingsToLean bndlst
        --     sigs = map sigToLean sig
        -- -- in  (intercalate "" bindlst) ++ unlines sigs
        -- in [unwords bindlst, unlines sigs]
    

---------------------------------------------------------------
---------------------------------------------------------------


-- tester function to convert TyClDecls to Lean code
tyClToLean :: TyClassDecls -> String
tyClToLean = \case 
    SynDecls name tyVar body ->
        let qtv = if not (null tyVar) then unwords tyVar else ""
            sb = processType body 
        in "abbrev " ++ name ++ " " ++ qtv ++ " := " ++ sb 
    DataDecls defTy name tyVar defnCons deriv -> case defTy of 
        NewTy -> 
            let consList = ""
            in ""
        DataTy -> 
            let consList = map getConsDetails defnCons -- list of details of each constructor
                consNames = map getConsNames defnCons
                -- recursEnd = " : " ++ name ++ " " ++ (if not (null tyVar) then unwords tyVar ++ " " else "")
                -- consLines = map (\s ->  s ++ recursEnd ) (map (getDefnCons tyVar) defnCons)
                consLines = map (getDefnCons tyVar) defnCons
            in "inductive " ++ name ++ (if not (null tyVar) then " " ++ unwords (formatParameterizedQTyVar tyVar) else "") ++ " where\n" ++ intercalate "\n" (map ("| "++) consLines)
            -- UNFINISHED
    _ -> "TyClassDecls Not Implemented"

-- returns just the constructor details
getConsDetails :: DefnConsDetails -> [Types]
getConsDetails = \case 
    DefnConsDetail name typs -> typs

-- returns just the constructor name
getConsNames :: DefnConsDetails -> String
getConsNames = \case
    DefnConsDetail name _ -> name


-- helper functions that get the parameterized type to be of the format (a : Type) etc. 
formatParameterizedQTyVar :: QTyVar -> [String]
formatParameterizedQTyVar = map formatParameterizedTyp

-- helper functions that get the parameterized type to be of the format (a : Type) etc. 
formatParameterizedTyp :: String -> String
formatParameterizedTyp x = "(" ++  x ++ " : Type)"


-- returns a string containing construction line (constructor + types)
getDefnCons :: QTyVar -> DefnConsDetails -> String 
getDefnCons tyVar det = case det of 
    DefnConsDetail name typs -> 
        let translatedTypes = map processType typs 
            tempVar = generateVarNamesExcluding (length typs) tyVar
            formatTypes = formatConTy tempVar translatedTypes
        in name ++ " " ++ unwords formatTypes


-- formats constructor types into lean format e.g. (a : Type)
formatConTy :: [String] -> [String] -> [String]
formatConTy tempVar conVar = zipWith (\s1 s2 -> "(" ++ s1 ++ " : " ++ s2 ++ ")") tempVar conVar








-- function that takes the funtypes and funbinds and returns the lean code of
-- the function arguments (e.g. (a : Int))
getBoundVar :: FunType -> Binds -> String
getBoundVar funTy funBind = thing
    where 
        listTys = init (processFunType funTy)
        listBinds = case funBind of
            FBind name args match ->  args 
            _ -> []
    
        -- returnTy = last listTys
        -- argty = init listTys
        pairedLists = zip listBinds listTys 
        thing = unwords (unzipBindTyList pairedLists)


-- helper functions for getBoundVar (unzipBindTyList, processType, processFunType,
                                --   processPatt, processPattArgs)
unzipBindTyList :: [(String, String)] -> [String]
unzipBindTyList = map (\(x, y) -> "(" ++ x ++ " : " ++ y ++ ")")


---------------------------------------------------------------
---------------------------------------------------------------

{- Functions for Types -}

processType :: Types -> String
processType = \case
    TypeVar str ->  str
    FunVar f -> case f of 
        LRational -> "Float"
        LEither -> "Except"
        -- LLeft -> "Except.error"
        -- LRight -> "Except.ok"
        LMaybe -> "Option"
        -- LJust -> "some"
        -- LNothing -> "none"
        LAlphaA -> "a"--"α"
        _ -> "FunType not "
    FType typs -> intercalate " -> " (map processType typs)     -- for actual function applications (arrows)
    AppTy ty1 ty2 -> processType ty1 ++ " " ++ processType ty2
        -- let p1 = processType1
    ExpListTy l -> "[" ++ intercalate ", " (map processType l) ++ "]"
    ListTy t -> "List " ++ processType t
    ParaTy t -> "(" ++ processType t ++ ")"
    -- _ -> "Type not implemented"
    EmptyT -> "Type Not Implemented"



-- for type signatures (no arrows)
processFunType :: FunType -> [String]
processFunType = getFunTypeList

getFunTypeList :: [Types] -> [String]
getFunTypeList =  map processType





---------------------------------------------------------------
---------------------------------------------------------------

{- Functions for Patts -}


processPatt :: Patts -> String
processPatt = \case 
    VariPatt p -> p 
    ParPatt par -> "(" ++ processPatt par ++ ")"
    ListPatt p1 -> "[" ++ intercalate ", " (map processPatt p1) ++ "]"
    ConPatt ty det -> 
        let patDetails = getConPattDetails det 
            con_ty = if ty == ":" then " :: " else ty
        in case det of 
            ConPattPrefix _ -> con_ty               -- TODO: something might be missing here...
            ConPattInfix _ _ -> intercalate con_ty patDetails
    LitPatt l -> processLits l 

processPattArgs :: [Patts] -> [String]
processPattArgs = map processPatt


getConPattDetails :: ConPattDetails -> [String]
getConPattDetails x = case x of 
    ConPattPrefix p -> map processPatt p 
    ConPattInfix a1 a2 -> [ a1,  a2]

---------------------------------------------------------------
---------------------------------------------------------------

{- Functions for Exprs -}

{-
unzipBindTyList :: [(String, String)] -> [String]
unzipBindTyList = map (\(x, y) -> "(" ++ x ++ " : " ++ y ++ ")")
-}

processExprs :: Exprs -> String 
processExprs = \case 
    Var x -> x 
    SpecialVar x -> case x of 
        VPutStrLn -> "IO.println"
        VPutStr -> "IO.print"
        VJust -> "some"
        VNothing -> "none"
        _ -> "Special Expr Not Implemented"
    App e1 e2 -> processExprs e1 ++ " " ++ processExprs e2 
    OperApp e1 o e2 -> processExprs e1 ++ " " ++ processExprs o ++ " " ++ processExprs e2 
    LetExpr bind exp -> 
        -- let bndrs = unzipLocBindsToBinds (processLocBindsToLean bind)
        let bndrs = removeEmptyStrFromList $ lines (processLocBindsToLean bind)
            exps = processExprs exp
        in unlines ( map ("\tlet "++)  bndrs )++ "\t(" ++ exps ++ ")"
        -- in show bndrs ++ " " ++ "\n\t(" ++ exps ++ ")"
    ExpList es -> "[" ++ intercalate ", " (map processExprs es) ++ "]"
    Litr l -> processLits l
    -- If 
    -- Par 
    -- Explicit Tuple
    _ -> "Expr not implemented"


{-
Removes empty strings from list to prevent extra newlines
-}
removeEmptyStrFromList :: [String] -> [String]
removeEmptyStrFromList [] = []
removeEmptyStrFromList (x : xs)
    | x == "" = removeEmptyStrFromList xs 
    | otherwise = x : removeEmptyStrFromList xs



---------------------------------------------------------------
---------------------------------------------------------------

{- Functions for Lits -}

processLits :: Lits -> String 
processLits = \case 
    Chars c -> show c 
    Strings s -> "\"" ++ s ++ "\"" 
    Ints i -> show i 
    Fractionals f -> show f

---------------------------------------------------------------
---------------------------------------------------------------

{-  Helper Functions  -}

{-
generateVarNames
-- parameterized variable name generation function
-- returns x number of alphabetized strings, starting from a, as a list
-- Function to generate variable names
-}
generateVarNames :: Int -> [String]
generateVarNames n = take n $ map toName [0..]
  where
    -- Convert a number to a variable name
    toName :: Int -> String
    toName x
      | x < 26    = [alphabet !! x]
      | otherwise = toName (x `div` 26 - 1) ++ [alphabet !! (x `mod` 26)]
    alphabet = ['a'..'z']


{-
generateVarNamesExcluding
-- another generation function but excludes conflicting strings
-- pass in an int and a list of strings that cannot be generated by the function
-- (attempts to) avoid naming conflicts in a single data/type declaration
-}
generateVarNamesExcluding :: Int -> [String] -> [String]
generateVarNamesExcluding n exclude = take n $ filter (`notElem` exclude) $ map toName [0..]
  where
    -- Convert a number to a variable name
    toName :: Int -> String
    toName x
      | x < 26    = [alphabet !! x]
      | otherwise = toName (x `div` 26 - 1) ++ [alphabet !! (x `mod` 26)]
    alphabet = ['a'..'z']



---------------------------------------------------------------

{-
Functions fo iterating over list of AST decls
    * functions: findASTPairs, isTySig, toSig, getSigName, isFBind, toBinds, getBindName
-}



{-
Goes through list of AST objects and links FBinds associated with their TySig
(because FBind can be independent or a constructor of TySig)
-}
findASTPairs :: [AST] -> [AST]
findASTPairs [] = []
findASTPairs [x] = [x]
findASTPairs (x : xs : ys) = 
    if isTySig x && isFBind xs then 
        let ty = toSig x 
            f = toBinds xs
            result =
                if getSigName ty == getBindName f then 
                    let combined = ty {fun_bind = f}
                    in SignatureD combined : findASTPairs ys
                else (x : findASTPairs (xs :ys))
        in result
    else (x : findASTPairs (xs : ys) )


{-
isTySig
- Checks if AST object is TySig
-}
isTySig :: AST -> Bool 
isTySig = \case 
    SignatureD s -> case s of 
        TySig ty_name qual_ty fun_type fun_bind -> True 
    _ -> False


{-
toSig
- Returns specifically the Sigs type, given the AST object is Sigs
- TODO: should probably do some error handling...
-}
toSig :: AST -> Sigs
toSig (SignatureD x) = x

{-
getSigName
- Returns name associated with Sigs 
- TODO: include other sigs 
-}
getSigName :: Sigs -> String
getSigName = \case 
    TySig name _ _ _ -> name 
    
{-
isFBind
- Checks if AST object is type FBind
-}
isFBind :: AST -> Bool 
isFBind = \case 
    ValueD v -> case v of 
        FBind fun_name patt_args matches -> True 
    _ -> False

{-
toBinds
- returns Binds object given AST object is Binds
- TODO: error handling
-}
toBinds :: AST -> Binds
toBinds (ValueD x) = x

{-
getBindName
- gets name of Bind object
- TODO: include other Binds types
-}
getBindName :: Binds -> String 
getBindName = \case 
    FBind name _ _ -> name


---------------------------------------------------------------


main = do
    hSetEncoding stdout utf8
    ------------------------------------
    --  SIGS AND BINDS TESTS
    ------------------------------------

    let emptyBinds = FBind {fun_name = "", patt_args = [], matches = []}

    
    let funBind2 =  (FBind {fun_name = "random", patt_args = ["x","ys"], matches = [MP {bound_var = [VariPatt "x",ConPatt {con_type = "[]", patt_details = ConPattPrefix {p_arg = []}}], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Var "x"}], loc_binds = EmptyLocBinds}},MP {bound_var = [VariPatt "x",ParPatt (ConPatt {con_type = ":", patt_details = ConPattInfix {p_arg1 = "y", p_arg2 = "ys"}})], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "x") (Var "-") (Litr (Ints 1))}], loc_binds = EmptyLocBinds}}]})
    let tyBind2 = TySig {ty_name = "random", qual_ty = [], fun_type = [TypeVar "Int", TypeVar "List Int", TypeVar "Int" ], fun_bind = funBind2}
    putStrLn $ sigToLean  tyBind2
    putStrLn "\n"

    let funBind3 =  (FBind {fun_name = "abc", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Ints 13)}], loc_binds = EmptyLocBinds}}]})
    let tySig3 = SignatureD (TySig {ty_name = "abc", qual_ty = [], fun_type = [TypeVar "Int"], fun_bind = funBind3})

    let funBind4 = ValueD (FBind {fun_name = "frac", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Litr (Ints 1)) (Var "/") (Litr (Ints 2))}], loc_binds = EmptyLocBinds}}]})

    let funBind5 = ValueD (FBind {fun_name = "add", patt_args = ["a","b"], matches = [MP {bound_var = [VariPatt "a",VariPatt "b"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "a") (Var "+") (Var "b")}], loc_binds = EmptyLocBinds}}]})
    let funBind6 = ValueD (FBind {fun_name = "minus", patt_args = ["a","b"], matches = [MP {bound_var = [VariPatt "a",VariPatt "b"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "a") (Var "-") (Var "b")}], loc_binds = EmptyLocBinds}}]})

    let funBind7 = ValueD (FBind {fun_name = "categorizeNumber", patt_args = ["x"], matches = [MP {bound_var = [VariPatt "x"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Negative")},StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Zero")},StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Small")},StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Large")}], loc_binds = EmptyLocBinds}}]})

    let testerTypeSyn2 = SynDecls {syn_name = "ResultFunction", qualTy_var = ["a", "b"], syn_body = FType [TypeVar "a", AppTy (AppTy (TypeVar "Either") (TypeVar "String")) (TypeVar "b")]}
    putStrLn $ tyClToLean testerTypeSyn2
    putStrLn "\n"

    let bindList = findASTPairs [SignatureD (TySig {ty_name = "frac", qual_ty = [], fun_type = [FunVar LRational], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "frac", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Litr (Ints 1)) (Var "/") (Litr (Ints 2))}], loc_binds = EmptyLocBinds}}]}),
                                TyClassD (SynDecls {syn_name = "Name", qualTy_var = [], syn_body = TypeVar "String"}),
                                TyClassD (SynDecls {syn_name = "ResultFunction", qualTy_var = ["a","b"], syn_body = FType [FunVar LAlphaA,AppTy (AppTy (FunVar LEither) (TypeVar "String")) (TypeVar "b")]}),
                                TyClassD (DataDecls {defn_type = DataTy, data_name = "Something", qualTy_var = ["a","b"], dataDefn_cons = [DefnConsDetail "Blah" [FunVar LAlphaA],DefnConsDetail "Bleh" [TypeVar "b"]], deriv_clause = []}),
                                TyClassD (DataDecls {defn_type = DataTy, data_name = "Color", qualTy_var = [], dataDefn_cons = [DefnConsDetail "Red" [],DefnConsDetail "Green" [],DefnConsDetail "Blue" []], deriv_clause = []}),
                                TyClassD (DataDecls {defn_type = DataTy, data_name = "Tree", qualTy_var = ["a"], dataDefn_cons = [DefnConsDetail "Empty" [],DefnConsDetail "Node" [FunVar LAlphaA,ParaTy (AppTy (TypeVar "Tree") (FunVar LAlphaA)),ParaTy (AppTy (TypeVar "Tree") (FunVar LAlphaA))]], deriv_clause = []}),
                                TyClassD (DataDecls {defn_type = DataTy, data_name = "Tree2", qualTy_var = [], dataDefn_cons = [DefnConsDetail "Nil" [],DefnConsDetail "Nod" [TypeVar "Int",TypeVar "Tree2",TypeVar "Tree2"]], deriv_clause = []}),
                                SignatureD (TySig {ty_name = "add", qual_ty = [], fun_type = [TypeVar "Int",TypeVar "Int",TypeVar "Int"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "add", patt_args = ["a","b"], matches = [MP {bound_var = [VariPatt "a",VariPatt "b"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "a") (Var "+") (Var "b")}], loc_binds = EmptyLocBinds}}]}),
                                ValueD (FBind {fun_name = "minus", patt_args = ["a","b"], matches = [MP {bound_var = [VariPatt "a",VariPatt "b"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "a") (Var "-") (Var "b")}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "abc", qual_ty = [], fun_type = [TypeVar "Int"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "abc", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Ints 13)}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "random", qual_ty = [], fun_type = [TypeVar "Int",ListTy (TypeVar "Int"),TypeVar "Int"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "random", patt_args = ["x","ys"], matches = [MP {bound_var = [VariPatt "x",ConPatt {con_type = "[]", patt_details = ConPattPrefix {p_arg = []}}], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Var "x"}], loc_binds = EmptyLocBinds}},MP {bound_var = [VariPatt "x",ParPatt (ConPatt {con_type = ":", patt_details = ConPattInfix {p_arg1 = "y", p_arg2 = "ys"}})], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "x") (Var "-") (Litr (Ints 1))}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "categorizeNumber", qual_ty = [], fun_type = [TypeVar "Int",TypeVar "String"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "categorizeNumber", patt_args = ["x"], matches = [MP {bound_var = [VariPatt "x"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [BodyStmts (OperApp (Var "x") (Var "<") (Litr (Ints 0)))], guard_expr = Litr (Strings "Negative")},StmtBody {guard_stmt = [BodyStmts (OperApp (Var "x") (Var "==") (Litr (Ints 0)))], guard_expr = Litr (Strings "Zero")},StmtBody {guard_stmt = [BodyStmts (OperApp (Var "x") (Var "<") (Litr (Ints 10)))], guard_expr = Litr (Strings "Small")},StmtBody {guard_stmt = [BodyStmts (Var "otherwise")], guard_expr = Litr (Strings "Large")}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "calculateArea", qual_ty = [], fun_type = [TypeVar "Float",TypeVar "Float"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "calculateArea", patt_args = ["r"], matches = [MP {bound_var = [VariPatt "r"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = LetExpr (ValsBinds [FBind {fun_name = "pi", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Fractionals 3.14)}], loc_binds = EmptyLocBinds}}]}] []) (OperApp (OperApp (Var "pi") (Var "*") (Var "r")) (Var "*") (Var "r"))}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "calculateRandom", qual_ty = [], fun_type = [TypeVar "Int",TypeVar "Int"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "calculateRandom", patt_args = ["x"], matches = [MP {bound_var = [VariPatt "x"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = LetExpr (ValsBinds [FBind {fun_name = "y", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Ints 10)}], loc_binds = EmptyLocBinds}}]},FBind {fun_name = "z", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Ints 2)}], loc_binds = EmptyLocBinds}}]}] []) (OperApp (OperApp (Var "x") (Var "+") (Var "y")) (Var "+") (Var "z"))}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "nthElement", qual_ty = [], fun_type = [ListTy (FunVar LAlphaA),TypeVar "Int",AppTy (FunVar LMaybe) (FunVar LAlphaA)], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "nthElement", patt_args = ["xs","a"], matches = [MP {bound_var = [ConPatt {con_type = "[]", patt_details = ConPattPrefix {p_arg = []}},VariPatt "a"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = SpecialVar VNothing}], loc_binds = EmptyLocBinds}},MP {bound_var = [ParPatt (ConPatt {con_type = ":", patt_details = ConPattInfix {p_arg1 = "x", p_arg2 = "xs"}}),VariPatt "a"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [BodyStmts (OperApp (Var "a") (Var "<=") (Litr (Ints 0)))], guard_expr = SpecialVar VNothing},StmtBody {guard_stmt = [BodyStmts (OperApp (Var "a") (Var "==") (Litr (Ints 1)))], guard_expr = App (SpecialVar VJust) (Var "x")},StmtBody {guard_stmt = [BodyStmts (OperApp (Var "a") (Var ">") (Litr (Ints 1)))], guard_expr = App (App (Var "nthElement") (Var "xs")) Other}], loc_binds = EmptyLocBinds}}]}),
                                ValueD (FBind {fun_name = "main", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = App (SpecialVar VPutStr) (Litr (Strings "Hello World"))}], loc_binds = EmptyLocBinds}}]})]
    

    -- putStrLn $ show bindList

    putStrLn "\n\n\n"
    
    putStrLn $ intercalate "\n\n" (astListToLean bindList)

    putStrLn "\n\n"

    
    
    -- Print Unicode characters
    -- putStrLn "This is a Unicode character: α"
   









    

