
{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Environment
import System.Console.GetOpt
import Control.Exception
import Data.Either

import qualified Data.Text.IO as Tio

import qualified GenC
import qualified GenC2
import qualified TypeCheck
import qualified Exceptions
import qualified Parse

data OptDump = DumpAst | DumpGen | DumpNoDump
  deriving(Eq,Ord,Show)
data OptTgt = C
  deriving(Eq,Ord,Show)

readTgt :: String -> OptTgt
readTgt "c" = C
readTgt "C" = C
readTgt lang = throw $ Exceptions.UnknownTgtLang lang

data Options = Options { optDump :: OptDump
                       , optTgt :: OptTgt
                       , optOutput :: String -> IO ()
                       , optHelp :: Bool
                       , optNew :: Bool
                       }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "a" ["dump-ast"]
      (NoArg (\opts -> return opts { optDump = DumpAst }))
      "Dump the byte block abstract syntax."
  , Option "i" ["dump-intermediary"]
      (NoArg (\opts -> return opts { optDump = DumpGen }))
      "Dump the intermediary representation."
  , Option "h" ["help"]
      (NoArg (\opts -> return opts { optHelp = True }))
      "Print this message."
  , Option "n" ["new-format"]
      (NoArg (\opts -> return opts { optNew= True }))
      "Output the new C api."
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
startOptions = Options { optDump = DumpNoDump
                       , optTgt = C
                       , optOutput = putStrLn
                       , optHelp = False
                       , optNew = False
                       }


main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (actions, inputFiles, []) -> do
      opts <- foldl (>>=) (return startOptions) actions
      if optHelp opts
        then usage
        else runCompiler opts inputFiles
      where usage = putStrLn $ usageInfo header options
    (_, _, errs) -> usage
      where usage = putStrLn $ concat errs ++ usageInfo header options
  where header = "Usage: bbc [OPTIONS...] files"

tests :: IO ()
tests = sequence_ [Parse.testMod]

runCompiler ::
  Options -> -- The compiler options as parsed out with getOpt
  [String] -> -- The list of files to compile
  IO ()
runCompiler opts files =
  let allBlocks = mapM parseFile files
      parseFile f = do
            contents <- Tio.readFile f
            return $ Parse.parseFile f contents
      Options { optDump = dump
              , optTgt = tgt
              , optOutput = out 
              , optNew = newFormat } = opts
  in do
    parses <- allBlocks
    if any isLeft parses -- test if there were any parse errors
      then putStrLn $ unlines $ map show $ lefts parses
    else
      let parseResults = concat $ rights parses
          gamma = TypeCheck.typeCheck parseResults TypeCheck.gammaInit
      in
      case dump of
        DumpAst -> out $ show parseResults
        DumpGen -> putStrLn "DumpGen Unimplimented."
        DumpNoDump -> do
          out $ case tgt of
                C -> (if newFormat then GenC2.gen else GenC.gen)
                        gamma parseResults
