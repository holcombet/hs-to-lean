
{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

module TestAST where

import Data.List (intercalate)
import System.IO


data AST 
    = SignatureD Sigs
    | ValueD Binds
    | TyClassD TyClassDecls
    | InstancesD InstDecls 
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
    | ClassSigs 
            {
                is_default  :: Bool,
                names       :: [String],
                sig_ty      :: [Types]
            }
    | EmptySig
    deriving (Eq, Show)


type Context = [Types]
type FunType = [Types]

-- data SigTys = SigTys OuterSigBndrs Types 
--     deriving (Eq, Show)

-- type OuterSigBndrs = String 


data InstDecls 
    = ClassInst 
        {
            inst_ty     :: [Types],
            binds       :: [Binds],
            exp_sigs    :: [Sigs]
        }
    | EmptyID
    deriving (Eq, Show)

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
    | ListCons  [Exprs]
    | ParaExpr Exprs
    | LetExpr LocBinds Exprs
    | CaseExpr Exprs [MatchPair]
    | IfExpr Exprs Exprs Exprs
    | DoExprs [Stmts]
    | ExpList [Exprs]
    | Litr Lits
    | Other
    deriving (Eq, Show)

data SVar 
    = VPutStrLn
    | VPutStr
    | VPrint
    | VNothing 
    | VJust
    | VLeft
    | VRight
    | VPi
    | VTrue 
    | VFalse
    | VShow
    | VFoldr
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
    | ClassDecls 
        {
            class_name      :: String,
            qualTy_var      :: QTyVar,
            method_sigs     :: [Sigs],
            default_method  :: [Binds]
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
    | LMaybe
    | LShow         -- typeclass
    | LEq           -- typeclass 
    | LAlphaA       -- universal a
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
    | WildPatt      -- for wildcards
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










    

