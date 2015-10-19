{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment
import qualified Ast
import qualified Parse

main :: IO ()
main = do
  args <- getArgs
  if elem "-test" args || elem "-t" args
     then Parse.testMod >>= \succ ->
                  putStrLn (if succ then "[ PASSED ]" else "[ FAIL ]")
     else putStrLn "unimplimented"
