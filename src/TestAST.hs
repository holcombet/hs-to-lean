
{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

module TestAST where

import Data.List (intercalate)

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
    deriving (Eq, Show)

data Exprs 
    = Var String 
    | App Exprs Exprs 
    | OperApp Exprs Exprs Exprs     -- left expr, operator, right expr
    | ExpList [Exprs]
    | Litr Lits
    | Other 
    deriving (Eq, Show)
data LocBinds 
    = EmptyLocBinds 
    | ValsBinds [Binds] [Sigs]
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
    | FType FunType
    | AppTy Types Types
    | EmptyT        -- placeholder
    deriving (Eq, Show)

type TVar = String
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
    | Fractionals Double
    deriving (Eq, Show)


-- Testing Functions -- 
astListToLean :: [AST] -> [String]
astListToLean = map astToLean 

astToLean :: AST -> String 
astToLean = \case 
    SignatureD s -> sigToLean s 
    ValueD v -> bindsToLean v 
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



-- tester function to convert Binds type to Lean code
bindsToLean :: Binds -> String
bindsToLean = \case
    FBind name args match -> 
        -- let matchStmt = "\tmatch " ++ intercalate ", " (processPattArgs args) ++ " " ++ "with\n"
        --     matches = match         -- understanding and development of Match under construction
        -- in matchStmt ++ "\t\t" ++ matches   -- TODO: figure out indentation
        -- if (null args && length match == 1) || (null args) then unlines ( map matchpairToLean match) -- intercalate ", "  match 
        if (null args || length match == 1)  then unlines ( map matchpairToLean match) -- intercalate ", "  match 
        else 
            let matchStmt = "\tmatch " ++ intercalate ", " ( args) ++ " "  ++ "with\n"
                matches = (map ("| " ++) (map matchpairToLean match))
            in matchStmt ++ "\t" ++ intercalate "\t" matches
    _ -> ""
       

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
matchpairToLean, guardRHSsToLean, and guardRHSToLean 

functions for getting guard body for non-constant functions
-}

matchpairToLean :: MatchPair -> String
matchpairToLean (MP pvar bod) = 
    let args = intercalate ", " (processPattArgs pvar)
        bodystring = guardRHSsToLean bod
    in args ++ bodystring


guardRHSsToLean :: GuardRHSs -> String 
guardRHSsToLean = \case 
    EmptyG -> "Guard Not Implemented"
    Guards gs loc -> 
        let guards = map guardRHSToLean gs 
            bndr = if loc == EmptyLocBinds then "" else "local binds not implemented"
        -- in unlines guards ++ "\n" ++ bndr 
        in unlines guards ++ if bndr == "" then "" else "\n"
    
guardRHSToLean :: GuardRHS -> String 
guardRHSToLean (StmtBody sm epr) = 
    let guardStmts = if null sm then " => " else "Lean Guards not Implemented"
        exprStr = if null sm then processExprs epr else "Lean Guards Not Implemented"
    in guardStmts ++ exprStr




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
                recursEnd = " : " ++ name ++ " " ++ (if not (null tyVar) then unwords tyVar ++ " " else "")
                consLines = map (\s ->  s ++ recursEnd ) (map (getDefnCons tyVar) defnCons)
            in "inductive " ++ name ++ (if not (null tyVar) then " " ++ unwords (formatParameterizedQTyVar tyVar) else "") ++ " where\n" ++ intercalate "\n" (map ("| "++) consLines)
            -- UNFINISHED

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
    TypeVar str -> str
    FType typs -> intercalate " -> " (map processType typs)     -- for actual function applications (arrows)
    AppTy ty1 ty2 -> processType ty1 ++ " " ++ processType ty2
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
        in case det of 
            ConPattPrefix _ -> ty               -- TODO: something might be missing here...
            ConPattInfix _ _ -> intercalate ty patDetails
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

processExprs :: Exprs -> String 
processExprs = \case 
    Var x -> x 
    App e1 e2 -> processExprs e1 ++ " " ++ processExprs e2 
    OperApp e1 o e2 -> processExprs e1 ++ " " ++ processExprs o ++ " " ++ processExprs e2 
    ExpList es -> "[" ++ intercalate ", " (map processExprs es) ++ "]"
    Litr l -> processLits l
    _ -> "Expr not implemented"


---------------------------------------------------------------
---------------------------------------------------------------

{- Functions for Lits -}

processLits :: Lits -> String 
processLits = \case 
    Chars c -> show c 
    Strings s -> s 
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


    let bindList = findASTPairs [SignatureD (TySig {ty_name = "frac", qual_ty = [], fun_type = [TypeVar "Rational"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "frac", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Litr (Ints 1)) (Var "/") (Litr (Ints 2))}], loc_binds = EmptyLocBinds}}]}),
                                EmptyD,
                                EmptyD,
                                EmptyD,
                                EmptyD,
                                EmptyD,
                                SignatureD (TySig {ty_name = "add", qual_ty = [], fun_type = [TypeVar "Int",TypeVar "Int",TypeVar "Int"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "add", patt_args = ["a","b"], matches = [MP {bound_var = [VariPatt "a",VariPatt "b"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "a") (Var "+") (Var "b")}], loc_binds = EmptyLocBinds}}]}),
                                ValueD (FBind {fun_name = "minus", patt_args = ["a","b"], matches = [MP {bound_var = [VariPatt "a",VariPatt "b"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "a") (Var "-") (Var "b")}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "abc", qual_ty = [], fun_type = [TypeVar "Int"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "abc", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Ints 13)}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "random", qual_ty = [], fun_type = [TypeVar "Int",EmptyT,TypeVar "Int"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "random", patt_args = ["x","ys"], matches = [MP {bound_var = [VariPatt "x",ConPatt {con_type = "[]", patt_details = ConPattPrefix {p_arg = []}}], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Var "x"}], loc_binds = EmptyLocBinds}},MP {bound_var = [VariPatt "x",ParPatt (ConPatt {con_type = ":", patt_details = ConPattInfix {p_arg1 = "y", p_arg2 = "ys"}})], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "x") (Var "-") (Litr (Ints 1))}], loc_binds = EmptyLocBinds}}]}),
                                SignatureD (TySig {ty_name = "categorizeNumber", qual_ty = [], fun_type = [TypeVar "Int",TypeVar "String"], fun_bind = EmptyB}),
                                ValueD (FBind {fun_name = "categorizeNumber", patt_args = ["x"], matches = [MP {bound_var = [VariPatt "x"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Negative")},StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Zero")},StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Small")},StmtBody {guard_stmt = [], guard_expr = Litr (Strings "Large")}], loc_binds = EmptyLocBinds}}]}),
                                ValueD (FBind {fun_name = "main", patt_args = [], matches = [MP {bound_var = [], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = App (Var "putStr") (Litr (Strings "Hello World"))}], loc_binds = EmptyLocBinds}}]})]
    
    putStrLn $ show bindList

    putStrLn "\n\n\n"
    
    putStrLn $ intercalate "\n" (astListToLean bindList)









    

    putStrLn "Hello World"