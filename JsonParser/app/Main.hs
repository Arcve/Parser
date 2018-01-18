module Main where

import Lib
import System.Environment
import Control.Monad as M
import Parser

main :: IO ()
main = do
    s <- getArgs
    case s of 
        [x]->  readJsonValue x >>= (\s -> case s of
                  Left s -> putStrLn s
                  Right (a, s) -> print a)
        _ -> putStrLn "usage: filename.json" 
