module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexE
import ParE
import SkelE
import PrintE
import PrintETAC
import AbsETAC


import TypeChecker
import ThreeAddressCode


import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showProgram v "\n[Abstract Syntax]\n\n" tree
                          showTree v "\n[Abstract Tree]\n\n" tree
                          putStrLn "[Type Checker]"
                          case typeCheck tree of
                              Bad err -> do putStrLn err
                                            exitFailure
                              Ok (env,prog) -> do putStrLn "\nCorrect Typing!"
                                                  showProgram v "\n[Annotated Program]\n\n" prog
                                                  showTree v "\n[Annotated tree]\n\n" prog
                                                  putStrLn "[Three Address Code]"
                                                  case generateTAC prog of
                                                      Bad err -> do putStrLn err
                                                                    exitFailure
                                                      Ok tacprog -> do  showProgram v "\n[TAC]\n\n" tacprog
                                                                        showTree v "\n[TAC]\n\n" tacprog
                                                                        exitSuccess


showTree :: (Show a, Print a) => Int -> String -> a -> IO ()
showTree v string tree = putStrV v $ string ++ printTree tree

showProgram :: (Show a, Print a) => Int -> String -> a -> IO()
showProgram v string prog = putStrV v $ string ++ show prog

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs
