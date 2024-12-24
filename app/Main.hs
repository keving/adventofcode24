{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Main where

import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, tails, singleton, group, sort, sortBy, sortOn, transpose, nub)
import Data.Map ((!), insertWith, empty, findWithDefault, update)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord
import Text.Parsec
import Text.Parsec.Number

parseLocIdLists :: Parsec String () [(Int, Int)]
parseLocIdLists = many $ do
  el1 <- many1 digit <* spaces <&> read
  el2 <- many1 digit <* spaces <&> read
  return (el1, el2)

day1_1 :: IO ()
day1_1 = do
  inp <- getContents
  let (ls,rs) = unzip $ fromRight [] $ parse parseLocIdLists "(input)" inp
  print $ sum $ zipWith (\x y -> abs $ x - y) (sort ls) (sort rs)

day1_2 :: IO ()
day1_2 = do
  inp <- getContents
  let (ls,rs) = unzip $ fromRight [] $ parse parseLocIdLists "(input)" inp
  print $ foldl (\s el -> s + (el * length (filter (== el) rs))) 0 ls

day2_1 :: IO ()
day2_1 = do
  inp <- getContents
  let reports = map (fromRight []. parse parseInts "(input)") $ lines inp
  print $ length $ filter valid $ map listIncs reports
  where
    parseInts :: Parsec String () [Int]
    parseInts = many1 $ do int <* spaces
    listIncs (x:y:ys) =(x-y):listIncs (y:ys)
    listIncs _ = []
    valid xs = (all (>0) xs || all (<0) xs) && all ((<= 3) . abs) xs

day2_2 :: IO ()
day2_2 = do
  inp <- getContents
  let augReports = map ((\xs -> xs:stripped xs) . fromRight []. parse parseInts "(input)") $ lines inp
  print $ length $ filter (any (valid . listIncs)) augReports
  where
    parseInts :: Parsec String () [Int]
    parseInts = many1 $ do int <* spaces
    listIncs :: [Int] -> [Int]
    listIncs (x:y:ys) =(x-y):listIncs (y:ys)
    listIncs _ = []
    valid xs = (all (>0) xs || all (<0) xs) && all ((<= 3) . abs) xs
    stripped [] = []
    stripped (x:xs) = xs : map ([x]++) (stripped xs)

day3_1 :: IO ()
day3_1 = do
  inp <- getContents
  print $ foldl (\s (m1, m2) -> s+(m1*m2)) 0 $ map (fromRight (0, 0) . parse parseMul "(input)") $ tails inp
  where
    parseMul :: Parsec String () (Int, Int)
    parseMul = do
      m1 <- string "mul(" *> many1 digit <* char ',' <&> read
      m2 <- many1 digit <* char ')' <&> read
      return (m1, m2)

day3_2 :: IO ()
day3_2 = do
  inp <- getContents
  print $ process True $ tails inp
  where
    parseMul :: Parsec String () (Int, Int)
    parseMul = do
      m1 <- string "mul(" *> many1 digit <* char ',' <&> read
      m2 <- many1 digit <* char ')' <&> read
      return (m1, m2)
    process _ [] = 0
    process _ (xs:xss) | "do()" `isPrefixOf` xs = process True xss
    process _ (xs:xss) | "don't()" `isPrefixOf` xs = process False xss
    process False (_:xss) = process False xss
    process True (xs:xss) = let (m1, m2) = fromRight (0, 0) . parse parseMul "(input)" $ xs in
                               m1*m2 + process True xss

day4_1 :: IO ()
day4_1 = do
  inp <- fmap lines getContents
  let forwards = inp ++ transpose inp ++ diagonals inp ++ diagonals (map reverse inp)
  print $ length $ concatMap (filter (isPrefixOf "XMAS") . tails) (forwards ++ map reverse forwards)
  where
    diagonals :: [[a]] -> [[a]]
    diagonals = tail . go [] where
      go b es_ = [h | h:_ <- b] : case es_ of
                                    []   -> transpose ts
                                    e:es -> go (e:ts) es
                                    where ts = [t | _:t <- b]

day4_2 :: IO ()
day4_2 = do
  inp@(hinp:_) <- fmap lines getContents
  -- Surround with dots.
  let ws = hf hinp ++ map (\l -> ['.'] ++ l ++ ['.']) inp ++ hf hinp
  let ws_coords = concat $ zipWith (\r xs -> zipWith (\c l -> ((r, c), l)) [0::Int ..] xs) [0::Int ..] ws
  print $ length $ filter (\((r,c), _) -> is_xmas ws r c) $ filter (\(_, l) -> l == 'A') ws_coords
  where
    hf xs = [replicate (length xs + 2) '.']
    is_xmas ws r c = ["MS", "MS"] == [sort [(ws!!(r-1))!!(c-1), (ws!!(r+1))!!(c+1)],
                                      sort [(ws!!(r-1))!!(c+1), (ws!!(r+1))!!(c-1)]]


day5_1 :: IO ()
day5_1 = do
  inp <- getContents
  let (cs,ms) = fromRight ([], []) $ parse parsePages "(input)" inp
  let cs_map = foldl (\m (k, v) -> insertWith (++) k v m) empty cs
  print $ sum $ map (\xs -> xs !! div (length xs) 2) $ filter (snd . foldl (\(s,v) e -> (s ++ findWithDefault [] e cs_map, v && (e `notElem` s))) ([], True)) ms
  where
    parsePages :: Parsec String () ([(Int, [Int])], [[Int]])
    parsePages = do
      constraints <- many $ do
                               l <- int
                               _ <- char '|'
                               r <- int
                               _ <- space
                               return (r, [l])
      spaces
      manuals <- many1 $ do
                           m <- sepBy int (char ',')
                           _ <- space
                           return m
      return (constraints, manuals)

day5_2 :: IO ()
day5_2 = do
  inp <- getContents
  let (cs,ms) = fromRight ([], []) $ parse parsePages "(input)" inp
  -- k -> [v | v must be before k]
  let cs_map = foldl (\m (k, v) -> insertWith (++) k v m) empty $ map (\(l, r) -> (l, [r])) cs
  -- k -> [v | v must be after k]
  let cs_map_inv = foldl (\m (k, v) -> insertWith (++) k v m) empty $ map (\(l, r) -> (r, [l])) cs
  let wrongs = filter (not . snd . foldl (\(s,v) e -> (s ++ findWithDefault [] e cs_map, v && (e `notElem` s))) ([], True)) ms
  print $ sum $ map ((\xs -> xs !! div (length xs) 2) . foldl (\s e -> ins (findWithDefault [] e cs_map_inv) e s) []) wrongs
  where
    ins _ e [] = [e]
    ins bs e xs@(h:_) | h `elem` bs = e:xs
    ins bs e (h:t) = h:ins bs e t
    parsePages :: Parsec String () ([(Int, Int)], [[Int]])
    parsePages = do
      constraints <- many $ do
                               l <- int
                               _ <- char '|'
                               r <- int
                               _ <- space
                               return (r, l)
      spaces
      manuals <- many1 $ do
                           m <- sepBy int (char ',')
                           _ <- space
                           return m
      return (constraints, manuals)

day6_1 :: IO ()
day6_1 = do
  xss <- fmap lines getContents
  let xss_coords = zipWith (\r xs -> zipWith (\c l -> ((r, c), l)) [0::Int ..] xs) [0::Int ..] xss
  let start = head $ filter (\(_,c) -> c `notElem` ".#") $ concat xss_coords
  let board_maps = Map.fromList [('>', xss_coords), ('v', transpose xss_coords), ('<', map reverse xss_coords), ('^', map reverse $ transpose xss_coords)]
  print $ length $ nub $ find_path start board_maps
  where
    turn '>' = 'v'
    turn 'v' = '<'
    turn '<' = '^'
    turn '^' = '>'

    find_path :: ((Int, Int), Char) -> Map.Map Char [[((Int,Int), Char)]] -> [(Int, Int)]
    find_path ((x,y),d) b | null rest = map fst travel
                          | otherwise = map fst travel ++ find_path (np, turn d) b
      where
       path = concatMap (dropWhile ((/= (x,y)) . fst)) (b!d)
       (travel, rest) = span ((/= '#') . snd) path
       (np,_) = last travel

day6_2 :: IO ()
day6_2 = do
  xss <- fmap lines getContents
  let xss_coords = zipWith (\r xs -> zipWith (\c l -> ((r, c), l)) [0::Int ..] xs) [0::Int ..] xss
  let start = head $ filter (\(_,c) -> c `notElem` ".#") $ concat xss_coords
  let board_maps = Map.fromList [('>', xss_coords), ('v', transpose xss_coords), ('<', map reverse xss_coords), ('^', map reverse $ transpose xss_coords)]
  let visited = find_path start board_maps []
  print $ length $ filter snd $ tail $ map (\p -> find_path start (add_block board_maps p) []) (fst visited)
  where
    turn '>' = 'v'
    turn 'v' = '<'
    turn '<' = '^'
    turn '^' = '>'

    -- Returns (path, is_cycle)
    find_path :: ((Int, Int), Char) -> Map.Map Char [[((Int,Int), Char)]] -> [(Int, Int, Char)] -> ([(Int, Int)], Bool)
    find_path ((x,y),d) b t | null rest = (squares, False)
                            | (x, y, d) `elem` t = ([], True)
                            | otherwise = (nub (squares ++ tail_squares), is_cycle)
      where
       path = concatMap (dropWhile ((/= (x,y)) . fst)) (b!d)
       (travel, rest) = span ((/= '#') . snd) $ ((x,y), '.'):path
       squares = map fst travel
       (tail_squares, is_cycle) = find_path (np, turn d) b ((x,y,d):t)
       (np,_) = last travel

    add_block :: Map.Map Char [[((Int,Int), Char)]] -> (Int,Int) -> Map.Map Char [[((Int,Int), Char)]]
    add_block m p = Map.map (map (map (\e -> if fst e == p then (fst e, '#') else e))) m

day7_1 :: IO ()
day7_1 = do
  xss <- fmap lines getContents
  let ls = map (fromRight (0, []) . parse parseLine "(input)") $ xss
  print $ sum $ map (\(s,c:cs) -> f s c cs) ls
  where
    f :: Int -> Int -> [Int] -> Int
    f s n [] | n == s = s
             | otherwise = 0
    f s n (o:os) = max (f s (n+o) os) (f s (n*o) os)

    parseLine :: Parsec String () (Int, [Int])
    parseLine = do
      total <- many1 digit <* char ':' <* spaces <&> read
      components <- many1 $ many1 digit <* spaces <&> read
      return (total, components)

main :: IO ()
main = day7_1
