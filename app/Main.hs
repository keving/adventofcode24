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


day1 :: IO ()
day1 = do
  inp <- getContents
  let (ls,rs) = unzip $ fromRight [] $ parse parseLocIdLists "(input)" inp
  print $ sum $ zipWith (\x y -> abs $ x - y) (sort ls) (sort rs)

day1_2 :: IO ()
day1_2 = do
  inp <- getContents
  let (ls,rs) = unzip $ fromRight [] $ parse parseLocIdLists "(input)" inp
  print $ foldl (\s el -> s + (el * length (filter (== el) rs))) 0 ls
  
main :: IO ()
main = day1_2
