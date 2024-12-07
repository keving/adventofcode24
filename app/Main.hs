{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Main where

import Control.Monad (void)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (isPrefixOf, tails, singleton, group, sort, sortBy, sortOn)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord
import Text.Parsec
import Text.Parsec.Number
import Text.Regex.TDFA

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

main :: IO ()
main = day3_1
