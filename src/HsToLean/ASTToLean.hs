
{-# LANGUAGE PackageImports, CPP, TypeApplications, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}

module HsToLean.ASTToLean (astListToLean, astToLean, findASTPairs) where 

import TestAST

import Data.List (intercalate)
-- import qualified GHC.Data.ShortText as T



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
            let matchStmt = "match " ++ intercalate ", " ( args) ++ " "  ++ "with\n"
                matches = (map ("| " ++) (map matchpairToLean match))
            in matchStmt  ++ intercalate "" (map removeTrailingWhitespace matches)
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
            in if guardStmts == "" then  ex else ex ++ " else"

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
        in " => " ++ if last guardEnd == "else" then unlines (init guards) ++  unwords (init guardEnd) else unlines guards ++ if bndr == "" then "" else "\n"
        -- in " => " ++ unlines guards ++ if bndr == "" then "" else "\n"
    
guardRHSToLean :: GuardRHS -> String 
guardRHSToLean (StmtBody sm epr) = 
    let guardStmts = if null sm then "" else --if null sm then " => " else --"Lean Guards not Implemented"
            let stm = guardStmtsToLean sm 
            in "\n" ++ if null stm ||  stm == "" then "" else stm ++ " then "
        exprStr = if null sm then processExprs epr else --"Lean Guards Not Implemented"
            let ex = processExprs epr 
            in if guardStmts == "" then ex else ex ++ " else"
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
        in if exp == "otherwise" then "" else "if " ++ exp
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
            ConPattInfix _ _ ->  intercalate con_ty patDetails
                -- if con_ty == " :: " then unwords (map ("List.con " ++ ) patDetails) else intercalate con_ty patDetails
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
    OperApp e1 o e2 -> -- processExprs e1 ++ " " ++ processExprs o ++ " " ++ processExprs e2 
        let oper = processExprs o 
        in if oper == ":" then processExprs e1 ++ " " ++ "::" ++ " " ++ processExprs e2 else processExprs e1 ++ " " ++  oper ++ " " ++ processExprs e2 
    ParaExpr e -> "(" ++ processExprs e ++ ")"
    LetExpr bind exp -> 
        -- let bndrs = unzipLocBindsToBinds (processLocBindsToLean bind)
        let bndrs = removeEmptyStrFromList $ lines (processLocBindsToLean bind)
            exps = processExprs exp
        in unlines ( map ("let "++)  bndrs )++ "(" ++ exps ++ ")"
        -- in show bndrs ++ " " ++ "\n\t(" ++ exps ++ ")"
    IfExpr e1 e2 e3 -> "if " ++ processExprs e1 ++ " then " ++ processExprs e2 ++ " else " ++ processExprs e3
    ExpList es -> "[" ++ intercalate ", " (map processExprs es) ++ "]"
    Litr l -> processLits l
    -- If 
    -- Par 
    -- Explicit Tuple
    _ -> "Expr not implemented"


-- processListConstructor :: Exprs -> Exprs -> String 
-- processListConstructor e1 e2 =
    -- let ex2 = processExprs e2 
-- processListConstructor e1 e2 = "(List.cons " ++ "(" ++ processExprs e1  ++ ") (" ++ processExprs e2 ++ ")) "




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


{-
removeTrailingWhitespace 
    Helper function
-}

removeTrailingWhitespace :: String -> String 
removeTrailingWhitespace = reverse . dropWhile (== ' ') . reverse
-- removeTrailingWhitespace x = intercalate "" (words x)


{-
listLengthIsOdd
    function that determines if list length is odd
-}

