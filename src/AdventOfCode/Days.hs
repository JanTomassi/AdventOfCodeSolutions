module AdventOfCode.Days
       ( run01
       , run02
       , run03
       , run04
       , run05
       , run06
       , run07
       ) where

import Debug.Trace (trace, -- traceId,
                    traceShow, -- traceShowId
                   )
import Data.Char   (digitToInt)
import Data.Array
import Data.List   (sortOn)
import Data.List   (transpose, foldl', foldl1')
import Data.List   (elemIndex)

-- import qualified Data.Set        as Set
import qualified Data.Map.Strict as M


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
            in traceShow shifted $ zipWith max best shifted


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
  (ranges', ids') = splitOn (== "") lns
  ranges = map ((\(f,s) -> (read f, read s)) . (splitOn (=='-'))) ranges'
  ids :: [Int] = [read v | v <- ids']
  in foldl (\acc i -> if (any ((flip inRange) i) ranges) then acc+1 else acc) 0 ids

  where
    splitOn p xs = let
      (chunk, rest) = break p xs
      in (chunk, drop 1 rest)

type Range a = (a, a)

part05_2 :: String -> Int
part05_2 strs = let
  lns = lines strs
  (ranges', _) = splitOn (== "") lns
  ranges :: [Range Int] = map ((\(f,s) -> (read f, read s)) . (splitOn (=='-'))) ranges'
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


data Elm07 = Null | Split | Beam | Source
     deriving (Read, Show, Enum, Eq, Ord)
-----------------
-- Seventh Day --
-----------------
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
      trace ("Unmatched pattern acc: " ++ show acc)
      $ trace ("Unmatched pattern ps: " ++ show ps)
      $ trace ("Unmatched pattern cs: " ++ show cs)
      $ error "Missing pattern"


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
