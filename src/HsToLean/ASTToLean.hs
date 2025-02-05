
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
    InstancesD i -> instanceToLean i
    _ -> ""


-- tester function to convert Sigs type to Lean code
sigToLean :: Sigs -> String
sigToLean = \case
    TySig tyName qualTy funTy funBind -> 
        let boundVar = getBoundVar funTy funBind
            funTyList = processFunType funTy        -- parameter types
            retTy = last funTyList      -- return type
            isValBind = length funTyList == 1
            specialCase = containsCase funBind 
        -- in  "def " ++ tyName ++ " " ++ boundVar ++ " : " ++ retTy ++ " := \n" ++ (if isValBind   then valBindsToLean tyName funBind else bindsToLean funBind) 
        in  "def " ++ tyName ++ " " ++ boundVar ++ " : " ++ retTy ++ " := \n" ++ bindsToLean funBind
    ClassSigs isDefault names sigTy -> 
        let d = if isDefault then "default" else "" 
            nameStr = unwords names 
            typeStr = map processType sigTy 
        in nameStr ++ " : " ++ intercalate " -> " typeStr 

    _ -> "Not Implemented"


-- functions for value binding functions
getFirstTy :: Types -> String
getFirstTy = \case 
    TypeVar v -> v 
    AppTy typ1 typ2 -> processType typ1

valBindsToLean :: String -> Binds -> String 
valBindsToLean ty bind = case bind of 
    -- FBind name args match -> ty ++ if not (null args) then " " ++ intercalate ", " ( args) else ""  ++ " = " ++ unlines (map matchpairToLean match)  -- intercalate ", " match  -- body not finished... but at least it's something
    FBind name args match -> 
        let bod = (map getConstantFunBody match)  
        in if length bod == 1 then unlines bod else bindsToLean bind
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
        -- if length match == 1 && mpNotCaseExpr (head match) then unlines ( map singleMatchPairToLean match)
        if length match == 1 && not (mpContainsCase (head match)) then unlines ( map singleMatchPairToLean match)
        else 
            let matchStmt = "match " ++ intercalate ", " ( args) ++ " "  ++ "with\n"
                matches = (map ("| " ++) (map matchpairToLean match))
            in matchStmt  ++ intercalate "" (map removeTrailingWhitespace matches)
    _ -> ""
       
-- mpNotCaseExpr :: MatchPair -> Bool 
-- mpNotCaseExpr (MP _ bod) = case bod of 
--     Guards e _ -> case e of 
--         [StmtBody _ ex] -> case ex of 
--             CaseExpr _ _ -> True 
--             _ -> False 
--         _ -> False
--     _ -> False



{-
For let statements
-}
bindingsToLean :: Binds -> String 
bindingsToLean = \case 
    FBind name a e -> name ++ " " ++ unwords a++ " := " ++ unwords (map singleMatchPairToLean e)
    _ -> "Bindings Not Implemented" 


{-
for instance binds (because it works differently)
-}
instanceBindsToLean :: Binds -> String 
instanceBindsToLean = \case 
    FBind name a e -> 
        let var = if a == [""] && length e > 1 then generateVarNames (1) else a 
            ms = if length e == 1 then map singleMatchPairToLean e else  map matchpairToLean e 
        in name ++ " " ++ unwords var ++ " := " ++ (if length e > 1 then "\nmatch " ++ intercalate ", " var ++ " with\n" ++ unlines (map ("| "++) ms) else unlines ms)
    _ -> "Instance Binding not implemented"

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
containsCase
-}

containsCase :: Binds -> Bool 
containsCase = \case 
    FBind name a e -> mpContainsCase (head e)


mpContainsCase :: MatchPair -> Bool 
mpContainsCase (MP pvar bod) = getExprFromGuard bod == "CaseExpr"

getExprFromGuard :: GuardRHSs -> String 
getExprFromGuard = \case 
    EmptyG -> "Other" 
    Guards gs loc -> getExprFromStmt (head gs)

getExprFromStmt :: GuardRHS -> String 
getExprFromStmt (StmtBody sm expr) = case expr of 
    CaseExpr _ _ -> "CaseExpr"
    _ -> "Other"


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
                bndr = processLocBindsToLean loc
            in unlines guards ++ (if bndr == "" then "" else "where\n" ++ bndr)

singleGuardRHSToLean :: GuardRHS -> String 
singleGuardRHSToLean (StmtBody sm epr) = 
    let guardStmts = if null sm then "" else 
            let stm = guardStmtsToLean sm 
            in if stm == "" then "" else stm ++ " then "
        exprStr = if null sm then processExprs epr else 
            let ex = processExprs epr 
            in if guardStmts == "" then  ex else ex ++ " else"
    in guardStmts ++ exprStr


{-
matchpairToLean, guardRHSsToLean, and guardRHSToLean 

functions for getting guard body for non-constant functions
-}

matchpairToLeanWithToString :: MatchPair -> String 
matchpairToLeanWithToString (MP pvar bod) = 
    let args = intercalate ", " (processPattArgs pvar)
        bodystring = words (guardRHSsToLean bod)
    in args ++ head bodystring ++ " " ++ "String.mk (" ++ unwords (tail bodystring) ++ ")"

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
            bndr =  processLocBindsToLean loc
            guardBody = if last guardEnd == "else" then unlines (init guards) ++  unwords (init guardEnd) else unlines guards
        in " => " ++ guardBody ++ (if bndr == "" then "" else "where\n" ++ bndr)
        -- in " => " ++ (if last guardEnd == "else" then unlines (init guards) ++  unwords (init guardEnd) else unlines guards) ++ (if bndr == "" then "" else "where\n\t" ++ bndr)
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
        in unlines bindlst  ++  unwords sigs
        -- in "is this thing on"
    

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
            let consList = map getConsDetails defnCons 
                consNames = map getConsNames defnCons 

            in ""
        DataTy -> 
            let consList = map getConsDetails defnCons -- list of details of each constructor
                consNames = map getConsNames defnCons
                derivTy = map processType deriv
                consLines = map (getDefnCons tyVar) defnCons
            in "inductive " ++ name ++ (if not (null tyVar) then " " ++ unwords (formatParameterizedQTyVar tyVar) else "") ++ " where\n" ++ intercalate "\n" (map ("| "++) consLines)
                ++ (if null derivTy then "" else "\nderiving " ++ intercalate ", " (removeEmptyStrFromList derivTy) )++ "\n\n" ++ "open " ++ name ++ "\n\n"
            -- UNFINISHED
    ClassDecls name qualT msigs dmet -> 
        let qt = formatParameterizedQTyVar qualT 
            s = map sigToLean msigs 
            dm = map bindsToLean dmet
            openStmt = "open " ++ name ++ "\n"
        in "class " ++ name ++ " " ++ unwords qt ++ " where\n" ++ unlines (map ("    "++) s) ++ "\n" ++ openStmt
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
            -- FBind name args match -> if args == [""] then generateVarNames (length listTys) else args 
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

instanceToLean :: InstDecls -> String 
instanceToLean i = case i of 
    ClassInst ty bind es -> 
        let t =  map addParenToType (map processType ty)
            b = map instanceBindsToLean bind 
            s = map sigToLean es
        in "instance : " ++ unwords t ++ " where\n" ++  unlines b
    EmptyID -> "Instance not implemented"


addParenToType :: String -> String
addParenToType x = 
    let ws = words x 
    in unwords (findListType ws)


findListType :: [String] -> [String]
findListType [] = [] 
findListType [x] = [x]
findListType (x : y : xs) = 
    if x == "List" then ("(" ++ x ++ " " ++ y ++ ")") : findListType xs else x : findListType (y : xs)
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
        LShow -> "Repr"
        LEq -> "DecidableEq"
        LEmpty -> ""
        -- _ -> "FunType not "
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
            con_ty = convertConTy ty
        in case det of 
            ConPattPrefix p -> con_ty ++ " " ++ if null p then "" else  unwords (map processPatt p)--con_ty               -- TODO: something might be missing here...
            ConPattInfix _ _ ->  intercalate con_ty patDetails
                -- if con_ty == " :: " then unwords (map ("List.con " ++ ) patDetails) else intercalate con_ty patDetails
    LitPatt l -> processLits l 
    WildPatt -> " _ "

processPattArgs :: [Patts] -> [String]
processPattArgs = map processPatt

convertConTy :: String -> String 
convertConTy x 
    | x == ":" = " :: "
    | x == "Just" = "some"
    | x == "Nothing" = "none"
    | otherwise = x

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
        VPrint -> "IO.print"
        VJust -> "some"
        VNothing -> "none"
        VShow -> "toString"
        VPi -> "3.14159"
        VTrue -> "true"
        VFalse -> "false"
        _ -> "Special Expr Not Implemented"
    App e1 e2 -> 
        let ex1 = processExprs e1 
            ex2 = processExprs e2 
        in ex1 ++ " " ++ ex2
    OperApp e1 o e2 -> -- processExprs e1 ++ " " ++ processExprs o ++ " " ++ processExprs e2 
        let oper = processExprs o 
        in if oper == ":" then processExprs e1 ++ " " ++ "::" ++ " " ++ processExprs e2 else 
            if oper == "." then processExprs e1 ++  " ∘ " ++ processExprs e2 else 
                if oper == "mod" then processExprs e1 ++ " % " ++ processExprs e2 else 
                processExprs e1 ++ " " ++  oper ++ " " ++ processExprs e2 
    ParaExpr e -> "(" ++ processExprs e ++ ")"
    LetExpr bind exp -> 
        -- let bndrs = unzipLocBindsToBinds (processLocBindsToLean bind)
        let bndrs = removeEmptyStrFromList $ lines (processLocBindsToLean bind)
            exps = processExprs exp
        in unlines ( map ("let "++)  bndrs )++ "(" ++ exps ++ ")"
        -- in show bndrs ++ " " ++ "\n\t(" ++ exps ++ ")"
    CaseExpr e mp -> 
        let ex = processExprs e 
            caseBod = map matchpairToLeanWithToString mp
        in "match " ++ (if head (words ex) == "toString" then "(" ++ ex ++ ").data" else ex) ++ " with\n" ++ unlines (map ("| "++) caseBod)
    DoExprs s -> "do\n" ++ unlines (map ("    "++) (map processStmts s))
    IfExpr e1 e2 e3 -> "if " ++ processExprs e1 ++ " then " ++ processExprs e2 ++ " else " ++ processExprs e3
    ExpList es -> "[" ++ intercalate ", " (map processExprs es) ++ "]"
    Litr l -> processLits l
    -- If 
    -- Par 
    -- Explicit Tuple
    _ -> "Expr not implemented"


processStmts :: Stmts -> String 
processStmts s = case s of 
    BodyStmts e -> processExprs e 
    BindStmts p e -> processPatt p ++ " <- " ++ processExprs e 
    EmptyS -> "Empty Stmt"
    _ -> "Stmts Not Implemented"

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
                    if getNumSigParam ty >= 1 && (getBindArgs f == [""]) then 
                        let newArgs = generateVarNames (getNumSigParam ty)
                            newFunB = f {patt_args = newArgs}
                            combined = ty {fun_bind = newFunB}
                        in SignatureD combined : findASTPairs ys 
                    else
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
    EmptySig -> ""
    
getNumSigParam :: Sigs -> Int 
getNumSigParam = \case 
    TySig _ _ f _ -> (length f) - 1
    EmptySig -> -1

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

getBindArgs :: Binds -> [VarPatt]
getBindArgs = \case 
    FBind name args match -> args 
    _ -> []

{-
removeTrailingWhitespace 
    Helper function
-}

removeTrailingWhitespace :: String -> String 
removeTrailingWhitespace = reverse . dropWhile (== ' ') . reverse
-- removeTrailingWhitespace x = intercalate "" (words x)


findCustomFunTypes :: [AST] -> [String]
findCustomFunTypes [] = []
findCustomFunTypes (x : xs) = case x of 
    SignatureD s -> case s of 
        TySig _ _ ft _ -> 
            let c = findCustomFunTy ft 
            in c ++ findCustomFunTypes xs 
        _ -> [] 
    _ -> []


findCustomFunTy :: FunType -> [String]
findCustomFunTy [] = []
findCustomFunTy (x : xs) = case x of 
    TypeVar s -> 
        if s `notElem` builtInTypes then s : findCustomFunTy xs else findCustomFunTy xs 
    _ -> []


builtInTypes :: [String]
builtInTypes = ["Integer", "Int", "Float", "Bool", "Maybe", "Either", "a"]


-- 
--
{- Helper functions -}

