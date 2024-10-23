module StructureAst (structAst) where

import System.IO
import Control.Applicative
import Control.Monad

parseAst :: String -> IO ()
parseAst ast = do
    let delimStack = []
        newAstFile = "hs_structured_AST.txt"
    newAst <- openFile newAstFile WriteMode
    parseAstHelper ast delimStack newAst ""
    hClose newAst

parseAstHelper :: String -> [Char] -> Handle -> String   -> IO ()
parseAstHelper [] _ _ _ = return ()
parseAstHelper (x:xs) delimStack newAst line
    | x == '(' = do
        when (not (null delimStack) && not (null line)) $
            hPutStrLn newAst  (replicate (length delimStack - 1) '\t' ++ line)
        -- hPutStr newAst "("
        parseAstHelper xs (x:delimStack) newAst "("
    | x == ')' = do
        let line' = line ++ ")"
        if null line
            then hPutStrLn newAst ("\n" ++ replicate (length delimStack - 1) '\t' ++ ")")
            else hPutStr newAst (replicate (length delimStack - 1) '\t' ++ line')
        parseAstHelper xs (tail delimStack) newAst ""
    | otherwise = do
        let line' = line ++ [x]
        parseAstHelper xs delimStack newAst line'


structAst :: String -> IO()
structAst filename = do
    contents <- readFile filename
    parseAst contents

main :: IO ()
main = do
    contents <- readFile "original_AST.txt"
    parseAst contents

