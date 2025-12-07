module Main where

import System.Environment (getArgs)
import AdventOfCode.Days2025 as D2025

main :: IO ()
main = do
  args <- getArgs
  case args of
    (year:day:rest) ->
      case (year, day) of
        ("2025", "1") -> run01 rest
        ("2025", "2") -> run02 rest
        ("2025", "3") -> run03 rest
        ("2025", "4") -> run04 rest
        ("2025", "5") -> run05 rest
        ("2025", "6") -> run06 rest
        ("2025", "7") -> run07 rest
        _   -> putStrLn ("Unknown day: " ++ day)
    _ ->
      usage

usage :: IO ()
usage =
  putStrLn $
    unlines
      [ "Usage: aoc <year> <day> [part]"
      , "Examples:"
      , "  aoc 2025 1"
      , "  aoc 2025 1 2"
      ]
