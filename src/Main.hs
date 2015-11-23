
{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import System.Console.GetOpt
import Control.Exception
import Data.Either

import qualified Parse
import qualified GenC
import qualified TypeCheck
import qualified Exceptions



data OptDump = DumpAst | DumpGen | DumpNoDump
  deriving(Eq,Ord,Show)
data OptTgt = C
  deriving(Eq,Ord,Show)

readTgt :: String -> OptTgt
readTgt "c" = C
readTgt "C" = C
readTgt lang = throw $ Exceptions.UnknownTgtLang lang

data Options = Options { optTest :: Bool
                       , optDump :: OptDump
                       , optTgt :: OptTgt
                       , optOutput :: String -> IO ()
                       }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option [] ["test"]
      (NoArg (\opts -> return opts { optTest = True }))
      "Run the tests."
  , Option "a" ["dump-ast"]
      (NoArg (\opts -> return opts { optDump = DumpAst }))
      "Dump the byte block abstract syntax."
  , Option "i" ["dump-intermediary"]
      (NoArg (\opts -> return opts { optDump = DumpGen }))
      "Dump the intermediary representation."
  , Option "t" ["target-lang"]
      (ReqArg
        (\arg opts -> return opts { optTgt = readTgt arg })
        "TARGET")
      "The target langauge to output."
  , Option "o" ["output-file"]
      (ReqArg
        (\arg opts -> return opts {optOutput = writeFile arg })
        "OUTPUT-FILE")
      "The file to which output should be written."
  ]

startOptions :: Options
startOptions = Options { optTest = False
                       , optDump = DumpNoDump
                       , optTgt = C
                       , optOutput = putStrLn
                       }

main :: IO ()
main = do
  args <- getArgs
  let (actions, inputFiles, _ {- errors -}) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return startOptions) actions
  runCompiler opts inputFiles

tests :: [IO Bool]
tests = [Parse.testMod]

runCompiler :: Options -> [String] -> IO ()
runCompiler opts files =
  let allBlocks = mapM parseFile files
      parseFile f = do
            contents <- readFile f
            return $ runParser Parse.parseFile () f contents
      Options { optTest = test
              , optDump = dump
              , optTgt = tgt
              , optOutput = out } = opts
  in do
    parses <- allBlocks
    if any isLeft parses -- test if there were any parse errors
    then putStrLn $ unlines $ map show $ lefts parses
    else
      let parseResults = concat $ rights parses
          gamma = TypeCheck.typeCheck parseResults TypeCheck.gammaInit
      in
        if test
        then let testsPass = foldl
                        (\flag tst -> -- TODO has to be a cleaner way
                            flag >>= \f -> tst >>= \t -> return (t && f))
                        (return True) tests
              in testsPass >>= \ok ->
                          putStrLn (if ok then "[ PASSED ]" else "[ FAIL ]")
        else
          case dump of
            DumpAst -> out $ show parseResults
            DumpGen -> putStrLn "DumpGen Unimplimented."
            DumpNoDump -> do
              out $ case tgt of
                      C -> GenC.gen gamma parseResults
