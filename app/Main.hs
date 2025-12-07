module Main where

import System.Environment (getArgs)
import AdventOfCode.Days

main :: IO ()
main = do
  args <- getArgs
  case args of
    (day:rest) ->
      case day of
        "1" -> run01 rest
        "2" -> run02 rest
        "3" -> run03 rest
        "4" -> run04 rest
        "5" -> run05 rest
        "6" -> run06 rest
        "7" -> run07 rest
        _   -> putStrLn ("Unknown day: " ++ day)
    _ ->
      usage

usage :: IO ()
usage =
  putStrLn $
    unlines
      [ "Usage: aoc2025 <day> [part]"
      , "Examples:"
      , "  aoc2025 1"
      , "  aoc2025 1 2"
      ]
