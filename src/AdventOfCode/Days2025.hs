module AdventOfCode.Days2025 where

import Debug.Trace (trace, traceId, traceShow, traceShowId)
import Data.Char   (digitToInt, isDigit)
import Data.Array
import Data.List   (sortOn, transpose, foldl', foldl1', sortBy, tails, elemIndex, find)
import Data.Ord    (comparing)
import Data.Maybe  (fromMaybe)
import Control.Monad.State.Strict

import qualified Data.List       as L
import qualified Data.Set        as S
-- import qualified Data.Vector     as V
import qualified Data.Map.Strict as M
import qualified Data.Sequence   as Seq
import qualified Data.Text       as T
import Data.Sequence (Seq(..), (|>))


---------------
-- First Day --
---------------
run01 :: [String] -> IO ()
run01 args = do
  input1 <- readFile "inputs/day01_1.txt"
  input2 <- readFile "inputs/day01_2.txt"
  case args of
    [] -> do
      putStrLn ("Day 1, part 1: " ++ show (part01_1 (lines input1) 0 50))
      putStrLn ("Day 1, part 2: " ++ show (part01_2 (lines input2) 0 50))
    ["1"] ->
      putStrLn ("Day 1, part 1: " ++ show (part01_1 (lines input1) 0 50))
    ["2"] ->
      putStrLn ("Day 1, part 2: " ++ show (part01_2 (lines input2) 0 50))
    _ ->
      putStrLn "Usage: aoc2025 1 [1|2]"

-- Fill these in with your actual solution logic.
part01_1 :: [String] -> Int -> Int -> Int
part01_1 [] res _pos = res
part01_1 (l:ls) res pos =
  let rot_amount = (read $ tail l)
      new_pos    = clockAround $ if (head l) == 'L'
                                 then (pos - rot_amount)
                                 else (pos + rot_amount)
      should_inc = boolToInt (new_pos == 0)
  in part01_1 ls (res + should_inc) new_pos
  where
    clockAround :: Int -> Int
    clockAround i = mod i 100

    boolToInt :: Bool -> Int
    boolToInt False = 0
    boolToInt True = 1

part01_2 :: [String] -> Int -> Int -> Int
part01_2 [] res _pos = res
part01_2 (l:ls) res pos =
  let rot_amount = (read $ tail l)
      new_pos    = clockAround $ if (head l) == 'L'
                                 then (pos - rot_amount)
                                 else (pos + rot_amount)
      inc_amount = if (head l) == 'L'
                       then (rot_amount `div` 100) + boolToInt ((pos /= 0 && (clockAround rot_amount) > pos) || (new_pos == 0))
                       else (rot_amount `div` 100) + boolToInt ((pos /= 0 && (clockAround rot_amount) > 100-pos) || new_pos == 0)
  in part01_2 ls (res + inc_amount) new_pos
  where
    clockAround :: Int -> Int
    clockAround i = mod i 100

    boolToInt :: Bool -> Int
    boolToInt False = 0
    boolToInt True = 1

----------------
-- Second Day --
----------------
run02 :: [String] -> IO ()
run02 args = do
  input0 <- readFile "inputs/day02.txt"
  input1 <- readFile "inputs/day02_1.txt"
  input2 <- readFile "inputs/day02_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 2, part 1: " ++ (show $ part02_1 input0)
      putStrLn $ "test: Day 2, part 2: " ++ (show $ part02_2 input0)
      putStrLn $ "Day 2, part 1: " ++ (show $ part02_1 input1)
      putStrLn $ "Day 2, part 2: " ++ (show $ part02_2 input2)
    ["t"] -> do
      putStrLn $ "test: Day 2, part 1: " ++ (show $ part02_1 input0)
      putStrLn $ "test: Day 2, part 2: " ++ (show $ part02_2 input0)
    ["1"] ->
      putStrLn $ "Day 2, part 1: " ++ (show $ part02_1 input1)
    ["2"] ->
      putStrLn $ "Day 2, part 2: " ++ (show $ part02_2 input2)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"


part02_1 :: String -> Int
part02_1 ranges =
  let
    rangeList = map (splitOnChar '-') (splitOnChar ',' ranges)
    listListMatch = map (\m -> case m of
                                 (s:e:_) -> matches (read s) (read e)
                                 _ -> error "Missing mathc") rangeList
  in sum $ concat listListMatch
  where
    matches :: Int -> Int -> [Int]
    matches s e = if s <= e then if match s then s : matches (s+1) e else matches (s+1) e else []

    match :: Int -> Bool
    match x =
      let str = show x
          len = length str
      in
        if even len
        then
          let (left, right) = splitAt (div len 2) str
          in left == right
        else False

    splitOnChar :: Char -> String -> [String]
    splitOnChar c s =
      case break (== c) s of
        (chunk, [])     -> [chunk]
        (chunk, _:rest) -> chunk : splitOnChar c rest


part02_2 :: String -> Int
part02_2 ranges =
  let
    rangeList = map (splitOnChar '-') (splitOnChar ',' ranges)
    listListMatch = map (\m -> case m of
                                 (s:e:_) -> matches (read s) (read e)
                                 _ -> error "Missing mathc") rangeList
  in sum $ concat listListMatch
  where
    matches :: Int -> Int -> [Int]
    matches s e = if s <= e then if match (show s) then s : matches (s+1) e else matches (s+1) e else []

    match :: String -> Bool
    match str =
      let
        len = length str
        listTake = (map (\n -> (\(x,xs) -> (x, splitEvery n xs))(splitAt n str)) [1..(div len 2)])
      in or $ map matchRec listTake

    splitEvery :: Int -> String -> [String]
    splitEvery n xs
        | n <= 0    = error "splitEvery: n must be > 0"
        | null xs   = []
        | otherwise = take n xs : splitEvery n (drop n xs)

    matchRec :: (String, [String]) -> Bool
    matchRec (str, strs) = all (== str) strs

    splitOnChar :: Char -> String -> [String]
    splitOnChar c s =
      case break (== c) s of
        (chunk, [])     -> [chunk]
        (chunk, _:rest) -> chunk : splitOnChar c rest


---------------
-- Third Day --
---------------
run03 :: [String] -> IO ()
run03 args = do
  input0 <- readFile "inputs/day03.txt"
  input1 <- readFile "inputs/day03_1.txt"
  input2 <- readFile "inputs/day03_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 3, part 1: " ++ (show $ part03_1 input0)
      putStrLn $ "test: Day 3, part 2: " ++ (show $ part03_2 input0)
      putStrLn $ "Day 3, part 1: " ++ (show $ part03_1 input1)
      putStrLn $ "Day 3, part 2: " ++ (show $ part03_2 input2)
    ["t"] -> do
      putStrLn $ "test: Day 3, part 1: " ++ (show $ part03_1 input0)
      putStrLn $ "test: Day 3, part 2: " ++ (show $ part03_2 input0)
    ["1"] -> do
      putStrLn $ "Day 3, part 1: " ++ (show $ part03_1 input1)
    ["2"] -> do
      putStrLn $ "Day 3, part 2: " ++ (show $ part03_2 input2)
    _ ->
      putStrLn "Usage: aoc2025 1 [0|1|2]"

part03_1 :: String -> Int
part03_1 =
    sum . map (go (-1) 0) . lines
    where
        -- twoPointSearch :: String -> String -> Int -> Int -> Int -> Int
        -- twoPointSearch [] [] _ _ acc = acc
        -- twoPointSearch [] (y:ys) _ _ acc = acc
        -- twoPointSearch (x:xs) [] _ _ acc = acc
        -- twoPointSearch x y xl yl acc =
        --     if xl <= yl
        --     then acc
        --     else
        --         let
        --             val = let f = digitToInt . head in 10 * f x + f y
        --             t1 = twoPointSearch (x) (tail y) xl (yl-1) $ max val acc
        --             t2 = twoPointSearch (tail x) (y) (xl-1) yl $ max val acc
        --         in max t1 t2
        go :: Int -> Int -> String -> Int
        go _         bestPair []     = bestPair
        go bestFirst bestPair (c:cs) =
          let d = digitToInt c

              -- if we already have a first digit, form a pair with current digit
              bestPair' =
                if bestFirst >= 0
                  then max bestPair (10 * bestFirst + d)
                  else bestPair

              -- update the best first digit we've seen so far
              bestFirst' = max bestFirst d
          in
              go bestFirst' bestPair' cs



-- part2 :: String -> Int
-- part2 =
--     sum . map (knapRec 11) . map (map digitToInt) . lines
--     where
--         knapRec :: Int       -- Num of items still takable
--                 -> [Int]     -- Element that can be taken
--                 -> Int       -- max value
--         knapRec cap (x:xs)
--           | cap < 0   = knapRec cap xs        -- can't take it
--           | otherwise =
--               max (10 ^ cap * x + knapRec (cap - 1) xs)  -- take it
--                   (knapRec cap xs)            -- skip it

part03_2 :: String -> Int
part03_2 =
    sum . map (maxKDigits 12) . map (map digitToInt) . lines
    where
        maxKDigits :: Int -> [Int] -> Int
        maxKDigits k digits =
            let initBest  = 0 : replicate k (-1)
                finalBest = foldl (step k) initBest digits
            in finalBest !! k
        step :: Int -> [Int] -> Int -> [Int]
        step k best d =
            let shifted = 0 : map mk (take k best)
                mk prev = if prev < 0 then -1 else prev * 10 + d
            in zipWith max best shifted


---------------
-- Forth Day --
---------------
run04 :: [String] -> IO ()
run04 args = do
  input0 <- readFile "inputs/day04.txt"
  input1 <- readFile "inputs/day04_1.txt"
  input2 <- readFile "inputs/day04_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 4, part 1: " ++ (show $ part04_1 input0)
      putStrLn $ "test: Day 4, part 2: " ++ (show $ part04_2 input0)
      putStrLn $ "Day 4, part 1: " ++ (show $ part04_1 input1)
      putStrLn $ "Day 4, part 2: " ++ (show $ part04_2 input2)
    ["t"] -> do
      putStrLn $ "test: Day 4, part 1: " ++ (show $ part04_1 input0)
      putStrLn $ "test: Day 4, part 2: " ++ (show $ part04_2 input0)
    ["1"] -> do
      putStrLn $ "Day 4, part 1: " ++ (show $ part04_1 input1)
    ["2"] -> do
      putStrLn $ "Day 4, part 2: " ++ (show $ part04_2 input2)
    _ ->
      putStrLn "Usage: aoc2025 1 [0|1|2]"

part04_1 :: String -> Int
part04_1 strs = let
  cols = lines strs
  colsN = length $ cols
  rowN = length $ cols !! 0
  grid = constructBoard rowN colsN cols
  ((gLR, gLC), (gHR, gHC)) = bounds grid
  cords = [(i,j)| i<-[-1..1], j<-[-1..1], (i/=0 || j/=0)]
  in foldl
     accRemuvable
     0
     [(length . filter id) $
       map (defaultAccess grid)
       (map (\(x,y) -> (x+i,y+j)) cords) |
      i <- [gLR..gHR],
      j <- [gLC..gHC],
      grid ! (i, j)]
  where
    constructBoard :: Int -> Int -> [String] -> Array (Int, Int) Bool
    constructBoard rS cS cols = array ((0,0),(rS-1,cS-1)) [((i,j), (val == '@')) |
                                                           (i, col) <- zip [0..] cols,
                                                           (j, val) <- zip [0..] col]
    defaultAccess :: Array (Int, Int) Bool -> (Int, Int) -> Bool
    defaultAccess a i = if inRange (bounds a) i then a ! i else False

    accRemuvable :: Int -> Int -> Int
    accRemuvable acc v = if v < 4 then acc + 1 else acc

part04_2 :: String -> Int
part04_2 strs = let
  cols = lines strs
  colsN = length $ cols
  rowN = length $ cols !! 0
  grid = constructBoard rowN colsN cols
  ((gLR, gLC), (gHR, gHC)) = bounds grid
  cords = [(i,j)| i<-[-1..1], j<-[-1..1], (i/=0 || j/=0)]
  in go grid cords gLR gHR gLC gHC
  where
    constructBoard :: Int -> Int -> [String] -> Array (Int, Int) Bool
    constructBoard rS cS cols = array ((0,0),(rS-1,cS-1)) [((i,j), (val == '@')) |
                                                           (i, col) <- zip [0..] cols,
                                                           (j, val) <- zip [0..] col]

    defaultAccess :: Array (Int, Int) Bool -> (Int, Int) -> Bool
    defaultAccess a i = if inRange (bounds a) i then a ! i else False

    doStep grid cords gLR gHR gLC gHC =
      filter
      ((<4).(snd))
      [((i,j), ((length . filter id) (map (defaultAccess grid) (map (\(x,y) -> (x+i,y+j)) cords)))) |
      i <- [gLR..gHR],
      j <- [gLC..gHC],
      grid ! (i, j)]

    rmElms grid elms =
      grid // [(pos, False) | pos <- elms]

    go grid cords gLR gHR gLC gHC = let
      step = doStep grid cords gLR gHR gLC gHC
      match = length step
      in if match > 0 then match + go (rmElms grid (map fst step)) cords gLR gHR gLC gHC else 0


---------------
-- Fifth Day --
---------------
run05 :: [String] -> IO ()
run05 args = do
  input0 <- readFile "inputs/day05.txt"
  input1 <- readFile "inputs/day05_1.txt"
  input2 <- readFile "inputs/day05_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 05, part 1: " ++ (show $ part05_1 input0)
      putStrLn $ "test: Day 05, part 2: " ++ (show $ part05_2 input0)
      putStrLn $ "Day 05, part 1: " ++ (show $ part05_1 input1)
      putStrLn $ "Day 05, part 2: " ++ (show $ part05_2 input2)
    ["t"] -> do
      putStrLn $ "test: Day 05, part 1: " ++ (show $ part05_1 input0)
      putStrLn $ "test: Day 05, part 2: " ++ (show $ part05_2 input0)
    ["1"] -> do
      putStrLn $ "Day 05, part 1: " ++ (show $ part05_1 input1)
    ["2"] -> do
      putStrLn $ "Day 05, part 2: " ++ (show $ part05_2 input2)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

part05_1 :: String -> Int
part05_1 strs = let
  lns = lines strs
  (ranges, ids) = splitOn (== "") lns
  ranges' = map ((\(f,s) -> (read f, read s)) . (splitOn (=='-'))) ranges
  ids' = [read v | v <- ids]
  in foldl (countWhenInRange ranges') 0 ids'
  where
    splitOn p xs = let
      (chunk, rest) = break p xs
      in (chunk, drop 1 rest)

    countWhenInRange :: [(Int, Int)] -> Int -> Int -> Int
    countWhenInRange ranges acc i =
      if (any ((flip inRange) i) ranges)
      then acc+1
      else acc

type Range a = (a, a)

part05_2 :: String -> Int
part05_2 strs = let
  lns = lines strs
  (ranges', _) = splitOn (== "") lns
  ranges = map ((\(f,s) -> (read f, read s)) . (splitOn (=='-'))) ranges'
  in foldl (\acc (l,h) -> (h-l+1)+acc) 0 (mergeRanges (mergeRanges ranges))

  where

    splitOn p xs = let
      (chunk, rest) = break p xs
      in (chunk, drop 1 rest)

    mergeRanges :: (Num a, Ord a, Show a) => [Range a] -> [Range a]
    mergeRanges = foldr step [] . sortOn id

    step :: (Num a, Ord a, Show a) => Range a -> [Range a] -> [Range a]
    step r [] = [r]
    step (l1,u1) acc@((l2,u2):rs)
      | (u1+1) < l2 = (l1,u1) : acc
      | otherwise = (min l1 l2, max u1 u2) : rs


---------------
-- Sixth Day --
---------------
run06 :: [String] -> IO ()
run06 args = do
  input0 <- readFile "inputs/day06.txt"
  input1 <- readFile "inputs/day06_1.txt"
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
part06_1 = sum
  . map (\(op,nums) -> foldl1' op nums)
  . map convert
  . transpose
  . reverse
  . map splitOnSpace
  . lines
  where
    splitOnSpace :: String -> [String]
    splitOnSpace l =
      case dropWhile isWhiteSpace l of
        [] -> []
        val -> let (n,rst) = break isWhiteSpace val
          in n : splitOnSpace rst

    convert :: (Num a) => [String] -> ((a -> a -> a), [Int])
    convert [] = error "Missing match"
    convert (op:nums) = ((convertOps op), map read nums)

    convertOps :: (Num a) => String -> a -> a -> a
    convertOps "+" = (+)
    convertOps "*" = (*)
    convertOps _   = error "Unknown op"

    isWhiteSpace = (==' ')

part06_2 :: String -> Int
part06_2 str =
  let ls = lines str
      (nums, ops) = splitAt ((length ls)-1) ls
      (nums', ops') = ((splitList . transpose) nums, (map convertOps . splitOnSpace . head) ops)
  in sum . map (\(ns, op) -> foldl1' op $ map read ns) $ zip nums' ops'
  where
    splitOnSpace :: String -> [String]
    splitOnSpace l =
      case dropWhile isWhiteSpace l of
        [] -> []
        val -> let (n,rst) = break isWhiteSpace val
          in n : splitOnSpace rst

    splitList :: [String] -> [[String]]
    splitList ns = let
      (n1, n2) = break (foldl' (flip ((&&) . isWhiteSpace)) True) ns
      in if n2 == [] then [n1] else n1 : splitList (tail n2)

    convertOps :: String -> Int -> Int -> Int
    convertOps "+" = (+)
    convertOps "*" = (*)
    convertOps _   = error "Unknown op"

    isWhiteSpace = (==' ')


-----------------
-- Seventh Day --
-----------------

data Elm07 = Null | Split | Beam | Source
     deriving (Read, Show, Enum, Eq, Ord)

run07 :: [String] -> IO ()
run07 args = do
  input0 <- readFile "inputs/day07.txt"
  input1 <- readFile "inputs/day07_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 07, part 1: " ++ (show $ part07_1 input0)
      putStrLn $ "test: Day 07, part 2: " ++ (show $ part07_2 input0)
      putStrLn $ "Day 07, part 1: " ++ (show $ part07_1 input1)
      putStrLn $ "Day 07, part 2: " ++ (show $ part07_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 07, part 1: " ++ (show $ part07_1 input0)
      putStrLn $ "test: Day 07, part 2: " ++ (show $ part07_2 input0)
    ["1"] -> do
      putStrLn $ "Day 07, part 1: " ++ (show $ part07_1 input1)
    ["2"] -> do
      putStrLn $ "Day 07, part 2: " ++ (show $ part07_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

part07_1 :: String -> Int
part07_1 str = let
  ls = lines str
  board = map (convertStrToElm) ls
  in snd $ update (head board) (tail board)
  where
    convertStrToElm :: String -> [Elm07]
    convertStrToElm [] = []
    convertStrToElm (c:cs) =
      (case c of
        '.' -> Null
        '^' -> Split
        '|' -> Beam
        'S' -> Source
        _   -> Null) : convertStrToElm cs

    update :: [Elm07] -> [[Elm07]] -> ([[Elm07]], Int)
    update _ [] = ([], 0)
    update [] _ = ([], 0)
    update p (c:cs) = let
      (updatedRow, accRow) = updateRow 0 p c
      (restRows, restAcc)  = update updatedRow cs
      totalAcc             = accRow + restAcc
      in (updatedRow : restRows, totalAcc)

    updateRow :: Int -> [Elm07] -> [Elm07] -> ([Elm07], Int)
    updateRow acc []              _              = ([], acc)
    updateRow acc _               []             = ([], acc)
    updateRow acc (p1:Null:p2:ps) (_:Split:_:cs) =
      let (rest, acc') = updateRow acc ps cs
      in  (p1 : Split : p2 : rest, acc')

    updateRow acc (_:Beam:_:ps) (_:Split:_:cs) =
      let (rest, acc') = updateRow (acc+1) ps cs
      in (Beam : Split : Beam : rest, acc')

    updateRow acc (Null:p1:ps) (Split:_:cs) =
      let (rest, acc') = updateRow acc ps cs
      in (Split : p1 : rest, acc')

    updateRow acc (Beam:_:ps) (Split:_:cs) =
      let (rest, acc') = updateRow (acc+1) ps cs
      in (Split : Beam : rest, acc')

    updateRow acc (Split : ps) (Null : cs) =
      let (rest, acc') = updateRow acc ps cs
      in (Null : rest, acc')

    updateRow acc (Null : ps) (Null : cs) =
      let (rest, acc') = updateRow acc ps cs
      in (Null : rest, acc')

    updateRow acc (Beam : ps) (Null : cs) =
      let (rest, acc') = updateRow acc ps cs
      in (Beam : rest, acc')

    updateRow acc (Source : ps) (Null : cs) =
      let (rest, acc') = updateRow acc ps cs
      in (Beam : rest, acc')

    updateRow acc ps cs =
      error ("Unmatched pattern acc: "
      ++ show acc
      ++ "\nUnmatched pattern ps: "
      ++ show ps
      ++ "\nUnmatched pattern cs: "
      ++ show cs
      ++ "\nMissing pattern")


part07_2 :: String -> Int
part07_2 str =
  case lines str of
    [] -> error "Missing lines"
    (first:restLines) -> case elemIndex 'S' first of
      Nothing   -> error "No 'S' in first line"
      Just sPos ->
        let s0 = M.singleton sPos 1
            a0 = 1
        in goLines s0 a0 (first:restLines)
  where
    goLines :: M.Map Int Int -> Int -> [String] -> Int
    goLines _ acc []       = acc
    goLines s acc (l:ls) =
      let (s', acc') = goCols s acc 0 l
      in goLines s' acc' ls

    goCols :: M.Map Int Int -> Int -> Int -> String -> (M.Map Int Int, Int)
    goCols s acc _   []     = (s, acc)
    goCols s acc col (c:cs)
      | c == '^' && M.member col s =
          let sj  = M.findWithDefault 0 col s
              s1  = M.insertWith (+) (col - 1) sj s
              s2  = M.insertWith (+) (col + 1) sj s1
              acc' = acc + sj
              s3  = M.insert col 0 s2
          in goCols s3 acc' (col + 1) cs
      | otherwise =
          goCols s acc (col + 1) cs



type JunctionBox08 = (Int,Int,Int)
run08 :: [String] -> IO ()
run08 args = do
  input0 <- readFile "inputs/day08_t.txt"
  input1 <- readFile "inputs/day08_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 08, part 1: " ++ (show $ part08_1 10 input0)
      putStrLn $ "test: Day 08, part 2: " ++ (show $ part08_2 input0)
      putStrLn $ "Day 08, part 1: " ++ (show $ part08_1 1000 input1)
      putStrLn $ "Day 08, part 2: " ++ (show $ part08_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 08, part 1: " ++ (show $ part08_1 10 input0)
      putStrLn $ "test: Day 08, part 2: " ++ (show $ part08_2 input0)
    ["1"] -> do
      putStrLn $ "Day 08, part 1: " ++ (show $ part08_1 1000 input1)
    ["2"] -> do
      putStrLn $ "Day 08, part 2: " ++ (show $ part08_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"


part08_1 :: Int -> String -> Int
part08_1 nCable strs = let
  ls  = lines strs
  psd = pairsByDistance $ map (createJunctionBox . splitOnChar ',') $ ls
  ssj = createSet $ ls
  in foldl' (\acc (s,_) -> s*acc) 1
     $ take 3
     $ sortBy (flip $ comparing fst)
     $ S.toList
     $ S.map (\it -> (S.size it, it))
     $ doNShortConnection (take nCable psd) ssj
  where
    pairsByDistance :: [JunctionBox08] -> [((JunctionBox08, JunctionBox08), Double)]
    pairsByDistance pts =
      sortBy (comparing snd) (allPairs pts)

    allPairs pts =
      [ ((p, q), dist p q)
      | (p:rest) <- tails pts
      , q        <- rest
      ]

    -- Euclidean distance between two 3D points
    dist :: JunctionBox08 -> JunctionBox08 -> Double
    dist (x1, y1, z1) (x2, y2, z2) =
      sqrt $ (fromIntegral(x1-x2))**2 + (fromIntegral(y1 - y2))**2 + (fromIntegral(z1 - z2))**2

    createSet :: [String] -> S.Set (S.Set JunctionBox08)
    createSet [] = S.empty
    createSet (s:ss) = S.insert (S.singleton $ createJunctionBox $ splitOnChar ',' s) $ createSet ss

    splitOnChar :: Char -> String -> [String]
    splitOnChar c s =
      case break (== c) s of
        (chunk, [])     -> [chunk]
        (chunk, _:rest) -> chunk : splitOnChar c rest

    createJunctionBox :: [String] -> JunctionBox08
    createJunctionBox (x:y:z:[]) = ((read x), (read y), (read z))
    createJunctionBox _ = error "Not possible to create JunctionBox"

    doNShortConnection
      :: [((JunctionBox08, JunctionBox08), Double)]
      -> S.Set (S.Set JunctionBox08)
      -> S.Set (S.Set JunctionBox08)
    doNShortConnection [] ssj  = ssj
    doNShortConnection (psd:psds) ssj = doNShortConnection psds $ doShortConnection psd ssj

    doShortConnection
      :: ((JunctionBox08, JunctionBox08), Double)
      -> S.Set (S.Set JunctionBox08)
      -> S.Set (S.Set JunctionBox08)
    doShortConnection ((p1,p2),_) s = fromMaybe s $ do
      s1 <- find (S.member p1) s
      s2 <- find (S.member p2) s
      pure $ S.insert (S.union s1 s2) $ S.delete s1 $ S.delete s2 s


part08_2 :: String -> Int
part08_2 strs = let
  ls  = lines strs
  psd = pairsByDistance $ map (createJunctionBox . splitOnChar ',') $ ls
  ssj = createSet $ ls
  in case doNShortConnection psd ssj of
       Just (((x1,_,_),(x2,_,_)),_) -> x1 * x2
       Nothing -> error "Impossible to solve"
  where
    pairsByDistance :: [JunctionBox08] -> [((JunctionBox08, JunctionBox08), Double)]
    pairsByDistance pts =
      sortBy (comparing snd) (allPairs pts)

    allPairs pts =
      [ ((p, q), dist p q)
      | (p:rest) <- tails pts
      , q        <- rest
      ]

    -- Euclidean distance between two 3D points
    dist :: JunctionBox08 -> JunctionBox08 -> Double
    dist (x1, y1, z1) (x2, y2, z2) =
      sqrt $ (fromIntegral(x1-x2))**2 + (fromIntegral(y1 - y2))**2 + (fromIntegral(z1 - z2))**2

    createSet :: [String] -> S.Set (S.Set JunctionBox08)
    createSet [] = S.empty
    createSet (s:ss) = S.insert (S.singleton $ createJunctionBox $ splitOnChar ',' s) $ createSet ss

    splitOnChar :: Char -> String -> [String]
    splitOnChar c s =
      case break (== c) s of
        (chunk, [])     -> [chunk]
        (chunk, _:rest) -> chunk : splitOnChar c rest

    createJunctionBox :: [String] -> JunctionBox08
    createJunctionBox (x:y:z:[]) = ((read x), (read y), (read z))
    createJunctionBox _ = error "Not possible to create JunctionBox"

    doNShortConnection
      :: [((JunctionBox08, JunctionBox08), Double)]
      -> S.Set (S.Set JunctionBox08)
      -> Maybe ((JunctionBox08, JunctionBox08), Double)
    doNShortConnection [] _  = Nothing
    doNShortConnection (psd:psds) ssj =
      let updateSsj = doShortConnection psd ssj
      in if S.size updateSsj == 1
         then Just psd
         else doNShortConnection psds updateSsj

    doShortConnection
      :: ((JunctionBox08, JunctionBox08), Double)
      -> S.Set (S.Set JunctionBox08)
      -> S.Set (S.Set JunctionBox08)
    doShortConnection ((p1,p2),_) s = fromMaybe s $ do
      s1 <- find (S.member p1) s
      s2 <- find (S.member p2) s
      pure $ S.insert (S.union s1 s2) $ S.delete s1 $ S.delete s2 s


type Point = (Int, Int)
type BoundingBox09 = (Point, Point, Int)
type Segment09 = (Point, Point)
run09 :: [String] -> IO ()
run09 args = do
  input0 <- readFile "inputs/day09_t.txt"
  input1 <- readFile "inputs/day09_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 09, part 1: " ++ (show $ part09_1 input0)
      putStrLn $ "test: Day 09, part 2: " ++ (show $ part09_2 input0)
      putStrLn $ "Day 09, part 1: " ++ (show $ part09_1 input1)
      putStrLn $ "Day 09, part 2: " ++ (show $ part09_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 09, part 1: " ++ (show $ part09_1 input0)
      putStrLn $ "test: Day 09, part 2: " ++ (show $ part09_2 input0)
    ["1"] -> do
      putStrLn $ "Day 09, part 1: " ++ (show $ part09_1 input1)
    ["2"] -> do
      putStrLn $ "Day 09, part 2: " ++ (show $ part09_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"


part09_1 :: String -> Int
part09_1 = third3
  . head
  . reverse
  . sortBy (comparing third3)
  . allPairs
  . map createPoint
  . map (splitOnChar ',')
  . lines
  where
    splitOnChar :: Char -> String -> [String]
    splitOnChar c s =
      case break (== c) s of
        (chunk, [])     -> [chunk]
        (chunk, _:rest) -> chunk : splitOnChar c rest

    createPoint :: [String] -> Point
    createPoint (x:y:[]) = (read x, read y)
    createPoint e = error $ "Not possible to create Point from " ++ (show e)

    allPairs :: [Point] -> [(Point, Point, Int)]
    allPairs pts = [(p, q, area p q) | (p:rest) <- tails pts, q <- rest]

    area :: Point -> Point -> Int
    area (x1,y1) (x2,y2) = ((abs $ x1-x2)+1)*((abs $ y1-y2)+1)

    third3 :: (Point,Point,Int) -> Int
    third3 (_,_,d) = d

part09_2 :: String -> Int
part09_2 strs = let points = map createPoint $ map (splitOnChar ',') $ lines strs
                    segments = createSegments points $ (tail points)++[head points]
                    allPointCombinations = allPairs points
                  in foldl (go segments) 0 allPointCombinations
  where
    splitOnChar :: Char -> String -> [String]
    splitOnChar c s =
      case break (== c) s of
        (chunk, [])     -> [chunk]
        (chunk, _:rest) -> chunk : splitOnChar c rest

    createPoint :: [String] -> Point
    createPoint (x:y:[]) = (read x, read y)
    createPoint e = error $ "Not possible to create Point from " ++ (show e)

    allPairs :: [Point] -> [(Point, Point, Int)]
    allPairs pts = [(p, q, area p q) | (p:rest) <- tails pts, q <- rest]

    area :: Point -> Point -> Int
    area (x1,y1) (x2,y2) = ((abs $ x1-x2)+1)*((abs $ y1-y2)+1)

    createSegments :: [Point] -> [Point] -> [(Point, Point)]
    createSegments [] [] = []
    createSegments ((p1x,p1y):ps) ((p2x,p2y):ns) =
      ((min p1x p2x, min p1y p2y),(max p1x p2x, max p1y p2y)) : createSegments ps ns
    createSegments _ _ = error "List size are not equal"

    go :: [(Point, Point)] -> Int -> (Point, Point, Int) -> Int
    go segments res ((b1x,b1y),(b2x,b2y),a) =
      if res >= a
      then res
      else let (min_x, max_x) = if b1x < b2x then (b1x, b2x) else (b2x, b1x)
               (min_y, max_y) = if b1y < b2y then (b1y, b2y) else (b2y, b1y)
           in if isFullyContained segments min_x min_y max_x max_y then a else res

    isFullyContained :: [(Point, Point)] -> Int -> Int -> Int -> Int -> Bool
    isFullyContained     []     _     _     _     _ = True
    isFullyContained (((s1x,s1y),(s2x,s2y)):ss) min_x min_y max_x max_y =
      if min_x < s2x && max_x > s1x && min_y < s2y && max_y > s1y
      then False
      else isFullyContained ss min_x min_y max_x max_y


run10 :: [String] -> IO ()
run10 args = do
  input0 <- readFile "inputs/day10_t.txt"
  input1 <- readFile "inputs/day10_1.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 10, part 1: " ++ (show $ part10_1 input0)
      putStrLn $ "test: Day 10, part 2: " ++ (show $ part10_2 input0)
      putStrLn $ "Day 10, part 1: " ++ (show $ part10_1 input1)
      putStrLn $ "Day 10, part 2: " ++ (show $ part10_2 input1)
    ["t"] -> do
      putStrLn $ "test: Day 10, part 1: " ++ (show $ part10_1 input0)
      putStrLn $ "test: Day 10, part 2: " ++ (show $ part10_2 input0)
    ["1"] -> do
      putStrLn $ "Day 10, part 1: " ++ (show $ part10_1 input1)
    ["2"] -> do
      putStrLn $ "Day 10, part 2: " ++ (show $ part10_2 input1)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

type Joltage = Int
data Button = On | Off
  deriving (Show, Eq, Ord)
data ButtonBoard = State [Button] [[Int]] [Joltage]
  deriving (Show, Eq)

-- createbuttonBoard :: String -> ButtonBoard
createbuttonBoard str = let (buttons,rest)   = createFinalButtonState str []
                            (wiring,rest')   = createButtonWiring rest []
                            (joltage,rest'') = createJoltageList rest' []
  in State buttons wiring joltage
  where
    createFinalButtonState :: String -> [Button] -> ([Button], String)
    createFinalButtonState (s:ss) buttons = case s of
      '[' -> createFinalButtonState ss buttons
      '.' -> createFinalButtonState ss (Off:buttons)
      '#' -> createFinalButtonState ss (On:buttons)
      ']' -> (reverse buttons, ss)
      ' ' -> createFinalButtonState ss buttons

    createButtonWiring :: String -> [[Int]] -> ([[Int]], String)
    createButtonWiring str@(s:ss) acc = case s of
      ' ' -> createButtonWiring ss acc
      '{' -> (reverse acc, str)
      '(' -> let (wiring,rest) = parseWiring str [] []
             in createButtonWiring rest (wiring:acc)

    parseWiring :: String -> [Int] -> [Int] -> ([Int], String)
    parseWiring (s:ss) number wiring = case s of
      ' ' -> parseWiring ss number wiring
      '(' -> parseWiring ss [] wiring
      n | isDigit n -> parseWiring ss ((digitToInt n):number) wiring
      ',' -> parseWiring ss [] ((foldl ((+).(*10)) 0 $ reverse number):wiring)
      ')' -> (reverse ((foldl ((+).(*10)) 0 $ reverse number):wiring), ss)
      e -> error ("parseWiring: Missing match for " ++ e : " the rest is " ++ ss)

    createJoltageList :: String -> [Int] -> ([Int], String)
    createJoltageList str@(s:ss) acc = case s of
      ' ' -> createJoltageList ss acc
      '{' -> parseJoltage str [] []
      e -> error ("createJoltageList: Missing match for " ++ e : " the rest is " ++ ss)

    parseJoltage :: String -> [Int] -> [Int] -> ([Int], String)
    parseJoltage str@(s:ss) number wiring = case s of
      '{' -> parseJoltage ss [] wiring
      n | isDigit n -> parseJoltage ss ((digitToInt n):number) wiring
      ',' -> parseJoltage ss [] ((foldl ((+).(*10)) 0 $ reverse number):wiring)
      '}' -> (reverse ((foldl ((+).(*10)) 0 $ reverse number):wiring), str)
      ' ' -> parseJoltage ss number wiring
      e -> error ("parseJoltage: Missing match for " ++ e : " the rest is " ++ ss)


part10_1 = sum
  . fmap initCall
  . fmap createbuttonBoard
  . lines
  where
    initCall it@(State buttons _ _) =
      findMinimumNumOfPress (replicate (length buttons) Off) it

    findMinimumNumOfPress :: [Button] -> ButtonBoard -> Int
    findMinimumNumOfPress start buttonBoard@(State target wirings joltages) =
      go (Seq.singleton (start, 0)) (S.singleton start)
      where
        go :: Seq ([Button], Int) -> S.Set [Button] -> Int
        go Seq.Empty _ = error "No solution reachable"
        go ((cur,p) :<| q) visited
          | cur == target = p
          | otherwise =
              let nextStates = filter (`S.notMember` visited) $ [ updateButtons cur w | w <- wirings ]
                  visited'   = foldl' (flip S.insert) visited nextStates
                  q'         = foldl' (\qq b' -> qq |> (b', p+1)) q nextStates
              in go q' visited'


    toggle :: Button -> Button
    toggle On  = Off
    toggle Off = On

    -- toggle the button at position i
    toggleAt :: Int -> [Button] -> [Button]
    toggleAt i bs =
      case splitAt i bs of
        (before, b:after) -> before ++ (toggle b : after)
        _                 -> bs

    -- apply all toggles in order
    updateButtons :: [Button] -> [Int] -> [Button]
    updateButtons = foldl' (\buttons i -> toggleAt i buttons)

part10_2 :: String -> Int
part10_2 = error "Solved using Z3 a hopefully an Haskell implementation will arrive soon"
  -- sum
  -- . fmap (traceShowId . initCall)
  -- . fmap createbuttonBoard
  -- . lines
  -- where
  --   initCall it@(State _ _ joltage) =
  --     findMinimumNumOfPress (replicate (length joltage) 0) it

  --   findMinimumNumOfPress :: [Joltage] -> ButtonBoard -> Int
  --   findMinimumNumOfPress start buttonBoard@(State _ wirings target) =
  --     go (Seq.singleton (start, 0)) (S.singleton start)
  --     where
  --       go :: Seq ([Int], Int) -> S.Set [Int] -> Int
  --       go Seq.Empty _ = error "No solution reachable"
  --       go ((cur,p) :<| q) visited
  --         | cur == target = p
  --         | otherwise =
  --             let nextStates = filter (\it -> (it <= target && it `S.notMember` visited)) [ updateJoltage cur w | w <- wirings ]
  --                 visited'   = foldl' (flip S.insert) visited nextStates
  --                 q'         = foldl' (\qq b' -> qq |> (b', p+1)) q nextStates
  --             in go q' visited'

  --   -- toggle the button at position i
  --   incrementAt :: Int -> [Joltage] -> [Joltage]
  --   incrementAt i bs =
  --     case splitAt i bs of
  --       (before, b:after) -> before ++ ((b+1) : after)
  --       _                 -> bs

  --   -- apply all toggles in order
  --   updateJoltage :: [Joltage] -> [Int] -> [Joltage]
  --   updateJoltage = foldl' (flip incrementAt)

type ServerId = String
type ServerConnection = M.Map ServerId [ServerId]
type ServerCache = M.Map (ServerId, S.Set ServerId) Int
run11 :: [String] -> IO ()
run11 args = do
  input1t <- readFile "inputs/day11_t1.txt"
  input2t <- readFile "inputs/day11_t2.txt"
  input <- readFile "inputs/day11.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 11, part 1: " ++ (show $ part11_1 input1t)
      putStrLn $ "test: Day 11, part 2: " ++ (show $ part11_2 input2t)
      putStrLn $ "Day 11, part 1: " ++ (show $ part11_1 input)
      putStrLn $ "Day 11, part 2: " ++ (show $ part11_2 input)
    ["t"] -> do
      putStrLn $ "test: Day 11, part 1: " ++ (show $ part11_1 input1t)
      putStrLn $ "test: Day 11, part 2: " ++ (show $ part11_2 input2t)
    ["1"] -> do
      putStrLn $ "Day 11, part 1: " ++ (show $ part11_1 input)
    ["2"] -> do
      putStrLn $ "Day 11, part 2: " ++ (show $ part11_2 input)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

-- Do a DFS search for the number of path leading to out
part11_1 :: String -> Int
part11_1 strs = let ls = lines strs
                    nodeConnectionMap = map createServerElement ls
                    graph = foldr (uncurry M.insert) M.empty nodeConnectionMap
                in dfs "out" S.empty graph "you"
  where
    createServerElement :: String -> (ServerId, [ServerId])
    createServerElement strs = let (node:connections) = words strs
                                   node' = reverse $ tail $ reverse node
                               in (node', connections)

    dfs :: ServerId -> S.Set ServerId -> ServerConnection -> ServerId -> Int
    dfs stop visited graph cur = let visited' = S.insert cur visited
                                     subRes = map (\it ->
                                                     if S.member cur visited
                                                     then 0
                                                     else
                                                       if it == stop
                                                       then 1
                                                       else dfs stop visited' graph it
                                                  )
                                              $ graph M.! cur
                                 in if S.member cur visited
                                    then 0
                                    else sum subRes

part11_2 :: String -> Int
part11_2 strs = let ls = lines strs
                    nodeConnectionMap = map createServerElement ls
                    graph = foldr (uncurry M.insert) M.empty nodeConnectionMap
                in dfsCached "out" graph "svr"
  where
    createServerElement :: String -> (ServerId, [ServerId])
    createServerElement strs = let (node:connections) = words strs
                                   node' = reverse $ tail $ reverse node
                               in (node', connections)

    dfsCached :: ServerId -- ^ stop
      -> ServerConnection -- ^ graph
      -> ServerId         -- ^ start
      -> Int
    dfsCached stop graph start =
      evalState (dfs (S.fromList ["dac","fft"]) S.empty start) M.empty
      where
        dfs :: S.Set ServerId -> S.Set ServerId -> ServerId -> State ServerCache Int
        dfs constraints visited cur = do
          cache <- get
          let key = (cur, S.delete cur constraints)
            in case M.lookup key cache of
                 Just v -> pure v
                 Nothing -> do
                   res <- compute (snd key) visited cur
                   modify' (M.insert key res)
                   pure res

        compute :: S.Set ServerId -> S.Set ServerId -> ServerId -> State ServerCache Int
        compute constraints visited cur
          | S.member cur visited = pure 0
          | otherwise = do
              let visited' = S.insert cur visited
                  neighbours =
                    case M.lookup cur graph of
                      Nothing -> error $ "Missing element " ++ cur ++ " from graph"
                      Just x -> x
              subRes <- mapM (recursiveStep constraints visited') neighbours
              pure $ sum subRes

        recursiveStep :: S.Set ServerId -> S.Set ServerId -> ServerId -> State ServerCache Int
        recursiveStep constraints visited it
          | it == stop =
            if S.member "fft" visited && S.member "dac" visited
            then pure 1
            else pure 0
          | otherwise = dfs constraints visited it


run12 :: [String] -> IO ()
run12 args = do
  inputt <- readFile "inputs/day12_t.txt"
  input <- readFile "inputs/day12.txt"
  case (args) of
    [] -> do
      putStrLn $ "test: Day 12, part 1: " ++ (show $ part12_1 inputt)
      putStrLn $ "Day 12, part 1: " ++ (show $ part12_1 input)
      putStrLn $ "Day 12, part 2: " ++ (show $ part12_2 input)
    ["t"] -> do
      putStrLn $ "test: Day 12, part 1: " ++ (show $ part12_1 inputt)
    ["1"] -> do
      putStrLn $ "Day 12, part 1: " ++ (show $ part12_1 input)
    _ ->
      putStrLn "Usage: aoc2025 1 [t|1|2]"

data Presents = Present | Empty

-- part12_1 :: String -> Int
part12_1 = sum
  . map (\((a,b),xs)->if ((a`div`3)*(b`div`3))>=(sum xs) then 1 else 0)
  . map makeTuple
  . filter (not.match)
  . map T.stripEnd
  . map T.pack
  . lines
  where
    makeTuple :: T.Text -> ((Int, Int), [Int])
    makeTuple t = let (w:rest) = T.splitOn (T.pack "x") t
                      (h:rest') = T.splitOn (T.pack ":") (T.concat rest)
                      blocks = T.splitOn (T.pack " ") (T.tail $ head $ rest')
                  in (((read.T.unpack) w, (read.T.unpack) h), map (read.T.unpack) blocks)

    match :: T.Text -> Bool
    match t = t == T.empty
      || T.last t == ':'
      || T.foldl containShape False t
      where
        containShape :: Bool -> Char -> Bool
        containShape acc c = acc || c=='#' || c=='.'
