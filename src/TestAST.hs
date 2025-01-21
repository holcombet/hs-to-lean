
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
    =   FBind 
            {
                fun_name :: String,
                patt_args :: [VarPatt],
                matches :: [MatchPair]   -- not a string, but temp
            }
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
    | FunType [Types]
    | AppTy Types Types
    deriving (Eq, Show)


data Patts 
    = EmptyP
    | VariPatt VarPatt
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



-- Testing Functions -- 
astListToLean :: [AST] -> [String]
astListToLean = map astToLean 

astToLean :: AST -> String 
astToLean = \case 
    SignatureD s -> sigToLean s 
    ValueD v -> bindsToLean v 
    TyClassD t -> tyClToLean t 


-- tester function to convert Sigs type to Lean code
sigToLean :: Sigs -> String
sigToLean = \case
    TySig tyName qualTy funTy funBind -> 
        let boundVar = getBoundVar funTy funBind
            retTy = last(processFunType funTy)
            -- isValBind = head(processFunType funTy) == last(processFunType funTy)
            isValBind = length (processFunType funTy) == 1
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
    FBind name args match -> ty ++ if not (null args) then " " ++ intercalate ", " ( args) else ""  ++ " = " ++ unlines (map matchpairToLean match)  -- intercalate ", " match  -- body not finished... but at least it's something
-------



-- tester function to convert Binds type to Lean code
bindsToLean :: Binds -> String
bindsToLean = \case
    FBind name args match -> 
        -- let matchStmt = "\tmatch " ++ intercalate ", " (processPattArgs args) ++ " " ++ "with\n"
        --     matches = match         -- understanding and development of Match under construction
        -- in matchStmt ++ "\t\t" ++ matches   -- TODO: figure out indentation
        if (null args && length match == 1) || (null args) then name ++ " = " ++ unlines ( map matchpairToLean match) -- intercalate ", "  match 
        else 
            let matchStmt = "\tmatch " ++ intercalate ", " ( args) ++ " "  ++ "with\n"
                matches = (map ("| " ++) (map matchpairToLean match))
            in matchStmt ++ "\t" ++ intercalate "\t" matches
       
        
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
    let guardStmts = if null sm then " = " else "Lean Guards not Implemented"
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








-- tester function that takes the funtypes and funbinds and returns the lean code of
-- the function arguments (e.g. (a : Int))
getBoundVar :: FunType -> Binds -> String
getBoundVar funTy funBind = thing
    where 
        listTys = init (processFunType funTy)
        listBinds = case funBind of
            FBind name args match ->  args 
    
        -- returnTy = last listTys
        -- argty = init listTys
        pairedLists = zip listBinds listTys 
        thing = unwords (unzipBindTyList pairedLists)


-- helper functions for getBoundVar (unzipBindTyList, processType, processFunType,
                                --   processPatt, processPattArgs)
unzipBindTyList :: [(String, String)] -> [String]
unzipBindTyList = map (\(x, y) -> "(" ++ x ++ " : " ++ y ++ ")")




processType :: Types -> String
processType = \case
    TypeVar str -> str
    FunType typs -> intercalate " -> " (map processType typs)     -- for actual function applications (arrows)
    AppTy ty1 ty2 -> processType ty1 ++ " " ++ processType ty2
    -- _ -> "Type not implemented"



-- for type signatures (no arrows)
processFunType :: FunType -> [String]
processFunType = map processType




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

processPattArgs :: [Patts] -> [String]
processPattArgs = map processPatt


getConPattDetails :: ConPattDetails -> [String]
getConPattDetails x = case x of 
    ConPattPrefix p -> map processPatt p 
    ConPattInfix a1 a2 -> [ a1,  a2]


processExprs :: Exprs -> String 
processExprs = \case 
    Var x -> x 
    App e1 e2 -> processExprs e1 ++ " " ++ processExprs e2 
    OperApp e1 o e2 -> processExprs e1 ++ " " ++ processExprs o ++ " " ++ processExprs e2 
    ExpList es -> "[" ++ intercalate ", " (map processExprs es) ++ "]"
    _ -> "Expr not implemented"



--------- HELPER FUNCTIONS ----------

-- parameterized variable name generation function
-- returns x number of alphabetized strings, starting from a, as a list
-- Function to generate variable names
generateVarNames :: Int -> [String]
generateVarNames n = take n $ map toName [0..]
  where
    -- Convert a number to a variable name
    toName :: Int -> String
    toName x
      | x < 26    = [alphabet !! x]
      | otherwise = toName (x `div` 26 - 1) ++ [alphabet !! (x `mod` 26)]
    alphabet = ['a'..'z']



-- another generation function but excludes conflicting strings
-- pass in an int and a list of strings that cannot be generated by the function
-- (attempts to) avoid naming conflicts in a single data/type declaration
generateVarNamesExcluding :: Int -> [String] -> [String]
generateVarNamesExcluding n exclude = take n $ filter (`notElem` exclude) $ map toName [0..]
  where
    -- Convert a number to a variable name
    toName :: Int -> String
    toName x
      | x < 26    = [alphabet !! x]
      | otherwise = toName (x `div` 26 - 1) ++ [alphabet !! (x `mod` 26)]
    alphabet = ['a'..'z']



-- tester for iterating over list of Decls

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
                else (x : xs : findASTPairs ys)
        in result
    else (x : xs : findASTPairs ys )


isTySig :: AST -> Bool 
isTySig = \case 
    SignatureD s -> case s of 
        TySig ty_name qual_ty fun_type fun_bind -> True 
    _ -> False

toSig :: AST -> Sigs
toSig (SignatureD x) = x

getSigName :: Sigs -> String
getSigName = \case 
    TySig name _ _ _ -> name 
    

isFBind :: AST -> Bool 
isFBind = \case 
    ValueD v -> case v of 
        FBind fun_name patt_args matches -> True 
    _ -> False

toBinds :: AST -> Binds
toBinds (ValueD x) = x

getBindName :: Binds -> String 
getBindName = \case 
    FBind name _ _ -> name



main = do

    ------------------------------------
    --  SIGS AND BINDS TESTS
    ------------------------------------

    let emptyBinds = FBind {fun_name = "", patt_args = [], matches = []}

    let funBind2 = ValueD (FBind {fun_name = "random", patt_args = [  "x",  "Not Implemented"], matches = [MP {bound_var = [ VariPatt "x",ConPatt {con_type = "[]", patt_details = ConPattPrefix {p_arg = []}}], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = Var "x"}], loc_binds = EmptyLocBinds}},MP {bound_var = [ VariPatt "x", VariPatt "Not Implemented"], guard_body = Guards {guard_exprs = [StmtBody {guard_stmt = [], guard_expr = OperApp (Var "x") (Var "-") Other}], loc_binds = EmptyLocBinds}}]})


    putStrLn $ astToLean funBind2 
    putStrLn "\n"

    let funBind3 = ValueD (FBind {fun_name = "abc", patt_args = [], matches = [MP {bound_var = [], guard_body = EmptyG}]})
    putStrLn $ astToLean funBind3
    putStrLn "\n"

    -- more complex tests : constructors (Foo and Bar), show type class, etc...
    let myBinds = FBind {fun_name = "foo", patt_args = [  "a",   "b"], matches = []}
    let myTySig = TySig {ty_name = "foo", qual_ty = [], fun_type = [TypeVar "Int", TypeVar "String"], fun_bind = myBinds}
    
    putStrLn $ sigToLean myTySig
    putStrLn "\n"

    -- more complex tests : nested function/pattern matching, guards, where clause,  etc...
    let testerBinds = FBind {fun_name = "Month", patt_args = [  "m",  "startDay",   "maxDay"], matches = []}
    let testerTySig = TySig {ty_name = "Month", qual_ty = [], fun_type = [TypeVar "Month", TypeVar "DayOfWeek", TypeVar "Int", TypeVar "String"], fun_bind = emptyBinds}
    let newtesterTySig = testerTySig {fun_bind = testerBinds}


    let testDeclList = [SignatureD testerTySig, ValueD testerBinds]
    let newDeclList =  findASTPairs testDeclList  
    putStrLn "\n"


    putStrLn $ intercalate "\n" (map astToLean newDeclList)

    putStrLn $ sigToLean newtesterTySig
    putStrLn "\n"


    -- Testing implicit binding
    let tySig2 = TySig {ty_name = "myId", qual_ty = [], fun_type = [TypeVar "UserId"], fun_bind = FBind {fun_name = "myId", patt_args = [], matches = []}}
    putStrLn $ sigToLean tySig2 
    putStrLn "\n"

    
    -- simple test : params, grhss, etc
    let tySig3 = TySig {ty_name = "add", qual_ty = [], fun_type = [TypeVar "Int", TypeVar "Int", TypeVar "Int"], fun_bind = FBind {fun_name = "add", patt_args = [  "a",   "b"], matches = [MP {bound_var = [ VariPatt "a",  VariPatt "b"], guard_body = EmptyG}]}}
    putStrLn $ sigToLean tySig3 
    putStrLn "\n"

    putStrLn $ show tySig3 ++ "\n"


    -- testing AST generated by ProcessFile

    

    ------------------------------------
    -- TYPESYN TESTS (type synonyms)
    ------------------------------------    
    
    -- testing no qualtyvar
    let testerTypeSyn = SynDecls {syn_name = "Name", qualTy_var = [], syn_body = TypeVar "String"}
    putStrLn $ tyClToLean testerTypeSyn 
    putStrLn "\n"    


    -- testing multiple qualtyvar and function in syn body
    let testerTypeSyn2 = SynDecls {syn_name = "ResultFunction", qualTy_var = ["a", "b"], syn_body = FunType [TypeVar "a", AppTy (AppTy (TypeVar "Either") (TypeVar "String")) (TypeVar "b")]}
    putStrLn $ tyClToLean testerTypeSyn2
    putStrLn "\n"

    ------------------------------------
    --  DATADECLS TESTS
    ------------------------------------

    -- (data) testing multiple constructor types
    let testerDataDecl1 = DataDecls {defn_type = DataTy, data_name = "Tree", qualTy_var = ["a"], dataDefn_cons = [DefnConsDetail "Empty" [], DefnConsDetail "Node" [(TypeVar "a"), (AppTy (TypeVar "Tree") (TypeVar "a")), (AppTy (TypeVar "Tree") (TypeVar "a"))]], deriv_clause = []}
    putStrLn $ tyClToLean testerDataDecl1
    putStrLn "\n"

    -- (data) testing multiple parameterized values
    let testerDataDecl2 = DataDecls {defn_type = DataTy, data_name = "Something", qualTy_var = ["a", "b"], dataDefn_cons = [DefnConsDetail "Blah" [TypeVar "a"], DefnConsDetail "Bleh" [TypeVar "b"]], deriv_clause = []}
    putStrLn $ tyClToLean testerDataDecl2
    putStrLn "\n"


    -- (data) testing no parameterized values or constructor types
    let testerDataDecl3 = DataDecls {defn_type = DataTy, data_name = "Color", qualTy_var =[], dataDefn_cons = [DefnConsDetail "Red" [], DefnConsDetail "Green" [], DefnConsDetail "Blue" []], deriv_clause = []}
    putStrLn $ tyClToLean testerDataDecl3
    putStrLn "\n"













    

    putStrLn "Hello World"