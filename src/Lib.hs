module Lib (
    someFunc
) where

import System.Environment(getArgs)
import System.Exit(exitFailure)

import Stella.Abs(Program)
import Stella.Lex(Token, mkPosToken)
import Stella.Par(pProgram, myLexer)
--import Stella.Print(printTree)
import Stella.Skel()

import Checker

type Err        = Either String
type ParseFun a = [Token] -> Err a

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = do
    --putStrLn f
    readFile f >>= run p

run :: ParseFun Program -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn "Tokens:"
      mapM_ (putStrLn . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      --putStrLn "\nParse Successful!"
      --showTree tree
      runChecker tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

{-
showTree :: Program -> IO ()
showTree tree = do
  putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
  --putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree
--}

runChecker :: Program -> IO ()
runChecker tree = case checkProgram tree of
    Left err -> do
        putStr $ show err
        exitFailure
    Right _ -> return ()

someFunc :: IO ()
someFunc = do
  args <- getArgs
  case args of
    []         -> getContents >>= run pProgram
    fs         -> mapM_ (runFile pProgram) fs
