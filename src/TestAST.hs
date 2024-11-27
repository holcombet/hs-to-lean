
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
    deriving (Eq, Show)

-- type TyVar = String

type QTyVar = [String]


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



tyClToLean :: TyClassDecls -> String
tyClToLean = \case 
    SynDecls name tyVar body ->
        let qtv = if not (null tyVar) then unwords tyVar else ""
            sb = processType body 
        in "abbrev " ++ name ++ " " ++ qtv ++ " := " ++ sb 

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


-- tester function to convert matches to Lean code
-- currently under construction (meaning nothing has been done but creating the funcrtion)
-- matchToLean :: String -> String
-- matchToLean 


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




main = do
    let myBinds = FBind {fun_name = "foo", pat_args = [VarPatt "a", VarPatt "b"], matches = "lol"}
    let myTySig = TySig {ty_name = "foo", qual_ty = [], fun_type = [TypeVar "Int", TypeVar "String"], fun_bind = myBinds}

    let testerBinds = FBind {fun_name = "Month", pat_args = [VarPatt "m", VarPatt "startDay", VarPatt "maxDay"], matches = "lol"}
    let testerTySig = TySig {ty_name = "Month", qual_ty = [], fun_type = [TypeVar "Month", TypeVar "DayOfWeek", TypeVar "Int", TypeVar "String"], fun_bind = testerBinds}

    let testerTypeSyn = SynDecls {syn_name = "Name", qualTy_var = [], syn_body = TypeVar "String"}
    let testerTypeSyn2 = SynDecls {syn_name = "ResultFunction", qualTy_var = ["a", "b"], syn_body = FunType [TypeVar "a", AppTy (AppTy (TypeVar "Either") (TypeVar "String")) (TypeVar "b")]}

    putStrLn $ sigToLean myTySig
    putStrLn "\n"

    putStrLn $ sigToLean testerTySig
    putStrLn "\n"

    putStrLn $ tyClToLean testerTypeSyn 
    putStrLn "\n"

    putStrLn $ tyClToLean testerTypeSyn2
    putStrLn $ "\n"
    
    putStrLn "Hello World"