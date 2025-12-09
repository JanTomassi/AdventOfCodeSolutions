module Main where

import System.Environment (getArgs)
import qualified AdventOfCode.Days2017 as D2017
import qualified AdventOfCode.Days2025 as D2025

main :: IO ()
main = do
  args <- getArgs
  case args of
    (year:day:rest) ->
      case year of
        -- 2017
        "2017" ->
          case day of
            "2" -> D2017.run02 rest
            "4" -> D2017.run04 rest
            "5" -> D2017.run05 rest
            _   -> putStrLn ("Unknown day for 2025: " ++ day)
        -- 2025
        "2025" ->
          case day of
            "1" -> D2025.run01 rest
            "2" -> D2025.run02 rest
            "3" -> D2025.run03 rest
            "4" -> D2025.run04 rest
            "5" -> D2025.run05 rest
            "6" -> D2025.run06 rest
            "7" -> D2025.run07 rest
            "8" -> D2025.run08 rest
            "9" -> D2025.run09 rest
            _   -> putStrLn ("Unknown day for 2025: " ++ day)
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
