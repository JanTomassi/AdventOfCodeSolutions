module AdventOfCode.Days2017 where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import Debug.Trace

run02 :: [String] -> IO ()
run02 args = do
  input0 <- readFile "inputs/2017/day02_t.txt"
  input1 <- readFile "inputs/2017/day02.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 02, part 1: " ++ (show $ part02_1 input0)
      putStrLn $ "test: Day 02, part 2: " ++ (show $ part02_2 input0)
      putStrLn $ "Day 02, part 1: " ++ (show $ part02_1 input1)
      putStrLn $ "Day 02, part 2: " ++ (show $ part02_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 02, part 1: " ++ (show $ part02_1 input0)
      putStrLn $ "test: Day 02, part 2: " ++ (show $ part02_2 input0)
    ["1"] -> do
      putStrLn $ "Day 02, part 1: " ++ (show $ part02_1 input1)
    ["2"] -> do
      putStrLn $ "Day 02, part 2: " ++ (show $ part02_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

part02_1 :: String -> Int
part02_1 = sum . map diffMaxMin . map (map read) . map words . lines
  where
    diffMaxMin :: [Int] -> Int
    diffMaxMin ints = let
      mx = maximum ints
      mn = minimum ints
      in mx - mn

part02_2 :: String -> Int
part02_2 = sum . map (recSearch) . map (map read) . map words . lines
  where
    recSearch :: [Int] -> Int
    recSearch [] = 0 -- error "Impossible puzzle"
    recSearch (n:ns) = case firstDivisor n ns of
      (Just a) -> a
      Nothing -> recSearch ns

    firstDivisor :: Int -> [Int] -> Maybe Int
    firstDivisor n ns = do
      d <- L.find (\d -> (mod n d) == 0 || (mod d n) == 0) ns
      pure (if n >= d then n `div` d else d `div` n)


run03 :: [String] -> IO ()
run03 args = do
  input0 <- readFile "inputs/2017/day03_t.txt"
  input1 <- readFile "inputs/2017/day03.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 03, part 1: " ++ (show $ part03_1 input0)
      putStrLn $ "test: Day 03, part 2: " ++ (show $ part03_2 input0)
      putStrLn $ "Day 03, part 1: " ++ (show $ part03_1 input1)
      putStrLn $ "Day 03, part 2: " ++ (show $ part03_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 03, part 1: " ++ (show $ part03_1 input0)
      putStrLn $ "test: Day 03, part 2: " ++ (show $ part03_2 input0)
    ["1"] -> do
      putStrLn $ "Day 03, part 1: " ++ (show $ part03_1 input1)
    ["2"] -> do
      putStrLn $ "Day 03, part 2: " ++ (show $ part03_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

part03_1 :: String -> Int
part03_1 = length . lines

part03_2 :: String -> Int
part03_2 = length . lines


run04 :: [String] -> IO ()
run04 args = do
  input0 <- readFile "inputs/2017/day04_t.txt"
  input1 <- readFile "inputs/2017/day04.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 04, part 1: " ++ (show $ part04_1 input0)
      putStrLn $ "test: Day 04, part 2: " ++ (show $ part04_2 input0)
      putStrLn $ "Day 04, part 1: " ++ (show $ part04_1 input1)
      putStrLn $ "Day 04, part 2: " ++ (show $ part04_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 04, part 1: " ++ (show $ part04_1 input0)
      putStrLn $ "test: Day 04, part 2: " ++ (show $ part04_2 input0)
    ["1"] -> do
      putStrLn $ "Day 04, part 1: " ++ (show $ part04_1 input1)
    ["2"] -> do
      putStrLn $ "Day 04, part 2: " ++ (show $ part04_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

part04_1 :: String -> Int
part04_1 = sum . map recSearch . map words . lines
  where
    recSearch :: [String] -> Int
    recSearch []     = 1
    recSearch (s:ss) = if matchAny s ss then 0 else recSearch ss

    matchAny :: String -> [String] -> Bool
    matchAny s = L.foldl' (flip ((||).(==s))) False

part04_2 :: String -> Int
part04_2 = sum . map recSearch . map words . lines
  where
    recSearch :: [String] -> Int
    recSearch []     = 1
    recSearch (s:ss) = if matchAny (strToCharMap s) ss then 0 else recSearch ss

    matchAny :: M.Map Char Int -> [String] -> Bool
    matchAny s = L.foldl' (flip ((||).(==s).strToCharMap)) False

    strToCharMap :: String -> M.Map Char Int
    strToCharMap []     = M.empty
    strToCharMap (s:ss) = M.insertWith (+) s 1 (strToCharMap ss)


run05 :: [String] -> IO ()
run05 args = do
  input0 <- readFile "inputs/2017/day05_t.txt"
  input1 <- readFile "inputs/2017/day05.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 05, part 1: " ++ (show $ part05_1 input0)
      putStrLn $ "test: Day 05, part 2: " ++ (show $ part05_2 input0)
      putStrLn $ "Day 05, part 1: " ++ (show $ part05_1 input1)
      putStrLn $ "Day 05, part 2: " ++ (show $ part05_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 05, part 1: " ++ (show $ part05_1 input0)
      putStrLn $ "test: Day 05, part 2: " ++ (show $ part05_2 input0)
    ["1"] -> do
      putStrLn $ "Day 05, part 1: " ++ (show $ part05_1 input1)
    ["2"] -> do
      putStrLn $ "Day 05, part 2: " ++ (show $ part05_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

part05_1 :: String -> Int
part05_1 str = go 0 (A.array (0, (length $ lines str) - 1) [(i, read j) | (i, j) <- (zip [0..] (lines str))]) 0
  where
    go :: Int -> A.Array Int Int -> Int -> Int
    go acc arr i = if A.inRange (A.bounds arr) i then go (acc+1) (arr A.// [(i, (arr A.! i) + 1)]) (i + (arr A.! i)) else acc

part05_2 :: String -> Int
part05_2 str = go 0 0 (A.array (0, (length $ lines str) - 1) [(i, read j) | (i, j) <- (zip [0..] (lines str))])
  where
    go :: Int -> Int -> A.Array Int Int -> Int
    go acc i arr = if A.inRange (A.bounds arr) i then go (acc+1) (i + (arr A.! i)) (arr A.// [(i, ((arr A.! i) + if (arr A.! i) < 3 then 1 else (-1)))]) else acc


run06 :: [String] -> IO ()
run06 args = do
  input0 <- readFile "inputs/2017/day06_t.txt"
  input1 <- readFile "inputs/2017/day06.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 06, part 1: " ++ (show $ part06_1 input0)
      putStrLn $ "test: Day 06, part 2: " ++ (show $ part06_2 input0)
      putStrLn $ "Day 06, part 1: " ++ (show $ part06_1 input1)
      putStrLn $ "Day 06, part 2: " ++ (show $ part06_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 06, part 1: " ++ (show $ part06_1 input0)
      putStrLn $ "test: Day 06, part 2: " ++ (show $ part06_2 input0)
    ["1"] -> do
      putStrLn $ "Day 06, part 1: " ++ (show $ part06_1 input1)
    ["2"] -> do
      putStrLn $ "Day 06, part 2: " ++ (show $ part06_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

part06_1 :: String -> Int
part06_1 = go S.empty . map read . words
  where
    go :: S.Set [Int] -> [Int] -> Int
    go visited cur = if S.member cur visited
                     then S.size visited
                     else go (S.insert cur visited) $ step cur

    step :: [Int] -> [Int]
    step v = let curMax = (maximum v)
                 (left,_:right) = break (==curMax) v
                 (r,right') = advance curMax right
             in recAdvance r $ left ++ (0:right')
      where
        recAdvance :: Int -> [Int] -> [Int]
        recAdvance n [] = []
        recAdvance n ls =
          let len     = length ls
              (q, r)  = n `divMod` len
              inc i l = l + q + (if i < r then 1 else 0)
          in zipWith inc [0..] ls


        advance :: Int -> [Int] -> (Int, [Int])
        advance 0 ls = (0, ls)
        advance a [] = (a, [])
        advance a (l:ls) = let (a', ls') = advance (a-1) ls
                           in (a', (l+1:ls'))

part06_2 :: String -> Int
part06_2 = loopSize S.empty . findLoopInit S.empty . map read . words
  where
    loopSize :: S.Set [Int] -> [Int] -> Int
    loopSize visited cur = if S.member cur visited
                     then S.size visited
                     else loopSize (S.insert cur visited) $ step cur

    findLoopInit :: S.Set [Int] -> [Int] -> [Int]
    findLoopInit visited cur = if S.member cur visited
                     then cur
                     else findLoopInit (S.insert cur visited) $ step cur

    step :: [Int] -> [Int]
    step v = let curMax = (maximum v)
                 (left,_:right) = break (==curMax) v
                 (r,right') = advance curMax right
             in recAdvance r $ left ++ (0:right')
      where
        recAdvance :: Int -> [Int] -> [Int]
        recAdvance n [] = []
        recAdvance n ls =
          let len     = length ls
              (q, r)  = n `divMod` len
              inc i l = l + q + (if i < r then 1 else 0)
          in zipWith inc [0..] ls


        advance :: Int -> [Int] -> (Int, [Int])
        advance 0 ls = (0, ls)
        advance a [] = (a, [])
        advance a (l:ls) = let (a', ls') = advance (a-1) ls
                           in (a', (l+1:ls'))
