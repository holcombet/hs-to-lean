
{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

module TestAST where

import Data.List (intercalate)

data AST 
    = SignatureD Sigs
    | ValueD Binds
    | TyClassD TyClassDecls
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
                pat_args :: [Patts],
                matches :: String   -- not a string, but temp
            }
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
    = VarPatt String
    | ParPatt Patts
    deriving (Eq, Show)






-- Testing Functions -- 



-- tester function to convert Sigs type to Lean code
sigToLean :: Sigs -> String
sigToLean = \case
    TySig tyName qualTy funTy funBind -> 
        let boundVar = getBoundVar funTy funBind
            retTy = last(processFunType funTy)
        in "def " ++ tyName ++ " " ++ boundVar ++ " : " ++ retTy ++ " := \n" ++ bindsToLean funBind
    -- _ -> "Not Implemented"


-- tester function to convert Binds type to Lean code
bindsToLean :: Binds -> String
bindsToLean = \case
    FBind name args match -> 
        let matchStmt = "\tmatch " ++ intercalate ", " (processPattArgs args) ++ " " ++ "with\n"
            matches = match         -- understanding and development of Match under construction
        in matchStmt ++ "\t\t" ++ matches   -- TODO: figure out indentation


-- tester function to convert TyClDecls to Lean code
tyClToLean :: TyClassDecls -> String
tyClToLean = \case 
    SynDecls name tyVar body ->
        let qtv = if not (null tyVar) then unwords tyVar else ""
            sb = processType body 
        in "abbrev " ++ name ++ " " ++ qtv ++ " := " ++ sb 
    DataDecls defTy name tyVar defnCons deriv -> case defTy of 
        NewTy -> "Structure Not Implemented"
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
            FBind name args match -> processPattArgs args 
    
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
    VarPatt str -> str 
    ParPatt par -> "(" ++ processPatt par ++ ")"

processPattArgs :: [Patts] -> [String]
processPattArgs = map processPatt



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



main = do
    let myBinds = FBind {fun_name = "foo", pat_args = [VarPatt "a", VarPatt "b"], matches = "lol"}
    let myTySig = TySig {ty_name = "foo", qual_ty = [], fun_type = [TypeVar "Int", TypeVar "String"], fun_bind = myBinds}

    let testerBinds = FBind {fun_name = "Month", pat_args = [VarPatt "m", VarPatt "startDay", VarPatt "maxDay"], matches = "lol"}
    let testerTySig = TySig {ty_name = "Month", qual_ty = [], fun_type = [TypeVar "Month", TypeVar "DayOfWeek", TypeVar "Int", TypeVar "String"], fun_bind = testerBinds}

    let testerTypeSyn = SynDecls {syn_name = "Name", qualTy_var = [], syn_body = TypeVar "String"}
    let testerTypeSyn2 = SynDecls {syn_name = "ResultFunction", qualTy_var = ["a", "b"], syn_body = FunType [TypeVar "a", AppTy (AppTy (TypeVar "Either") (TypeVar "String")) (TypeVar "b")]}

    let testerDataDecl1 = DataDecls {defn_type = DataTy, data_name = "Tree", qualTy_var = ["a"], dataDefn_cons = [DefnConsDetail "Empty" [], DefnConsDetail "Node" [(TypeVar "a"), (AppTy (TypeVar "Tree") (TypeVar "a")), (AppTy (TypeVar "Tree") (TypeVar "a"))]], deriv_clause = []}
    let testerDataDecl2 = DataDecls {defn_type = DataTy, data_name = "Something", qualTy_var = ["a", "b"], dataDefn_cons = [DefnConsDetail "Blah" [TypeVar "a"], DefnConsDetail "Bleh" [TypeVar "b"]], deriv_clause = []}
    putStrLn $ sigToLean myTySig
    putStrLn "\n"

    putStrLn $ sigToLean testerTySig
    putStrLn "\n"

    putStrLn $ tyClToLean testerTypeSyn 
    putStrLn "\n"

    putStrLn $ tyClToLean testerTypeSyn2
    putStrLn "\n"

    putStrLn $ tyClToLean testerDataDecl1
    putStrLn "\n"

    putStrLn $ tyClToLean testerDataDecl2
    putStrLn "\n"

    

    putStrLn "Hello World"