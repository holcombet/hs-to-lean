{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

{-
This module takes the intermediate AST and linearizes it back into Haskell code

This module helps measure and test for the correct level of abstraction from our AST
-}

module HsToLean.ASTToHaskell (astListToHaskell, astToHaskell) where 

import AST

import Data.List (intercalate)



astListToHaskell :: [AST] -> [String]
astListToHaskell = map astToHaskell 

astToHaskell :: AST -> String 
astToHaskell = \case 
    SignatureD s -> sigToHaskell s 
    ValueD v -> bindsToHaskell v ++ "\n"
    _ -> ""


sigToHaskell :: Sigs -> String 
sigToHaskell = \case 
    TySig tyName qualTy funTy funBind -> 
        let funTyList = funTypeToHaskell funTy 
        in tyName ++ " :: " ++ intercalate " -> " funTyList 


bindsToHaskell :: Binds -> String 
bindsToHaskell = \case 
    FBind name args match -> 
        let funBody = map matchPairToHaskell match
            n = name ++ " " 
            -- var = map ((n) ++)  args 
            -- funlines = zipWith (\s1 s2 -> s1 ++ s2) var funBody
            funlines = map (n++) funBody
        in unlines funlines
        -- in name ++ " " ++ unwords var ++ " " ++ funBody



matchPairToHaskell :: MatchPair -> String 
matchPairToHaskell (MP pvar bod) = 
    let args = unwords (pattArgsToHaskell pvar)
        bodstring = grhssToHaskell bod 
    in args ++ bodstring 


grhssToHaskell :: GuardRHSs -> String 
grhssToHaskell = \case 
    EmptyG -> "Guard Not Implemented"
    Guards gs loc ->
        let guards = map grhsToHaskell gs 
            bndr = unlines (map ("        "++) (lines $ locBindsToHaskell loc ))
        in unwords guards ++ if bndr == "" then "" else "\n    where\n" ++ bndr 


locBindsToHaskell :: LocBinds -> String 
locBindsToHaskell bind = case bind of 
    EmptyLocBinds -> ""
    ValsBinds bndlst sig ->
        let bndlist = map bindsToHaskell bndlst 
            sigs = map sigToHaskell sig 
        in intercalate "" bndlist ++ unlines sigs

grhsToHaskell :: GuardRHS -> String 
grhsToHaskell (StmtBody sm epr) =
    let guardStmts = if null sm then " = " else " | " ++ stmtsToHaskell sm ++ " = "
        exprStr = if null sm then exprToHaskell epr else exprToHaskell epr ++ "\n"
    in guardStmts ++ exprStr 

stmtsToHaskell :: [Stmts] -> String 
stmtsToHaskell sts = intercalate ", " (map stmtToHaskell sts)

stmtToHaskell :: Stmts -> String 
stmtToHaskell = \case 
    BodyStmts e -> exprToHaskell e 
    BindStmts b e -> "BindStmts not implemented"
    _ -> "Stmts not implemented"



exprToHaskell :: Exprs -> String 
exprToHaskell = \case 
    Var x -> x 
    SpecialVar x -> case x of 
        VPutStrLn -> "putStrLn"
        VPutStr -> "putStr"
        VPrint -> "print"
        VJust -> "Just"
        VNothing -> "Nothing"
        VShow -> "Show"
        VPi -> "pi"
        VTrue -> "True"
        VFalse -> "False"
        _ -> "Special Expr Not Implemented"
    App e1 e2 -> exprToHaskell e1 ++ " " ++ exprToHaskell e2 
    OperApp e1 o e2 -> exprToHaskell e1 ++ " " ++ exprToHaskell o ++ " " ++ exprToHaskell e2
    ParaExpr e -> "(" ++ exprToHaskell e ++ ")"
    LetExpr bind exp ->
        let bndrs = intercalate "\n        " (lines $ locBindsToHaskell bind )
            exps = exprToHaskell exp 
        in "\n    let " ++ bndrs ++ "\n    in " ++ exps
    DoExprs s -> "do\n" ++ unlines (map ("    "++) (map stmtToHaskell s))
    IfExpr e1 e2 e3 -> "if " ++ exprToHaskell e1 ++ " then " ++ exprToHaskell e2 ++ " else " ++ exprToHaskell e3 
    ExpList es -> "[" ++ intercalate ", " (map exprToHaskell es) ++ "]"
    Litr l -> litsToHaskell l
    _ -> "Expr Not Implemented"




processTypeToHaskell :: Types -> String 
processTypeToHaskell = \case 
    TypeVar str -> str 
    FunVar f -> case f of 
        LRational -> "Rational"
        LEither -> "Either"
        LMaybe -> "Maybe"
        LAlphaA -> "a"
        LShow -> "Show"
        LEq -> "Eq"
        _ -> ""
    FType typs -> intercalate " -> " (map processTypeToHaskell typs)
    AppTy ty1 ty2 -> processTypeToHaskell ty1 ++ " " ++ processTypeToHaskell ty2 
    ExpListTy l -> "[" ++ intercalate ", " (map processTypeToHaskell l) ++ "]"
    ListTy t -> "[" ++ processTypeToHaskell t ++ "]"
    ParaTy t -> "(" ++ processTypeToHaskell t ++ ")"
    _ -> "Not Implemented"

funTypeToHaskell :: FunType -> [String]
funTypeToHaskell = getFunTypeListHaskell 

getFunTypeListHaskell :: [Types] -> [String]
getFunTypeListHaskell = map processTypeToHaskell


pattsToHaskell :: Patts -> String 
pattsToHaskell = \case 
    VariPatt p -> p 
    ParPatt par -> "(" ++ pattsToHaskell par ++ ")"
    ListPatt p1 -> "[" ++ intercalate ", " (map pattsToHaskell p1) ++ "]"
    ConPatt ty det ->
        let patDetails = conPattDetailsToHaskell det 
        in case det of 
            ConPattPrefix p -> ty ++ " " ++ unwords (map pattsToHaskell p)
            ConPattInfix _ _ -> intercalate ty patDetails
    LitPatt l -> litsToHaskell l
    WildPatt -> "_"

pattArgsToHaskell :: [Patts] -> [String]
pattArgsToHaskell = map pattsToHaskell

conPattDetailsToHaskell :: ConPattDetails -> [String]
conPattDetailsToHaskell x = case x of 
    ConPattPrefix p -> map pattsToHaskell p 
    ConPattInfix a1 a2 -> [a1, a2]


litsToHaskell :: Lits -> String 
litsToHaskell = \case 
    Chars c -> show c 
    Strings s -> "\"" ++ s ++ "\""
    Ints i -> show i 
    Fractionals f -> show f
