{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Main where

import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, tails, singleton, group, sort, sortBy, sortOn, transpose)
import Data.Map ((!), insertWith, empty, findWithDefault)
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



main :: IO ()
main = day5_2
