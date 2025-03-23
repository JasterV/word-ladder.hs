module Main (main) where

import Ladder (ladderSolve, readDictionary)
import System.Environment

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename> <start> <end>")

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dictFile, start, end] -> do
      dict <- readDictionary dictFile
      case ladderSolve dict start end of
        Nothing -> putStrLn "No solution"
        Just solution -> do
          print solution
          putStrLn ("Length: " ++ show (length solution))
    _ -> printHelpText "Wrong arguments"
