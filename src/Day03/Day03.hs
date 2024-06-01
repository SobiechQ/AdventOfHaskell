{-# LANGUAGE LambdaCase #-}
module Day03.Day03 where

import Data.Char (digitToInt, isDigit)
import qualified Data.Foldable

first :: IO ()
first = do
  input <- readFile "src/Day03/input.txt"

  --   print $ symbolParts input
  print $ numberParts input

  return ()

data Position = Position {x :: Int, y :: Int} deriving (Show)

data Part = SymbolPart {c :: Char, position :: Position} | NumberPart {n :: Int, position :: Position} deriving (Show)

symbolParts :: String -> [Part]
symbolParts s =
  map (\(x, y, c) -> SymbolPart c $ Position x y) $ filter (\(_, _, c) -> c /= '.' && not (isDigit c)) $ yAxis s >>= xAxis

numberParts :: String -> [Part]
numberParts s =
  map (\(x, y, n) -> NumberPart (digitToInt n) $ Position x y) $ filter (\(_, _, c) -> isDigit c) $ yAxis s >>= xAxis

reduceNumerParts :: [Part] -> [Part]
reduceNumerParts p =
    let pn = p >>= \case
            NumberPart n p -> [NumberPart n p]
            _ -> []
    in
        pn

-- f s =
yAxis :: String -> [(Int, String)]
yAxis s = zip [0 .. length $ lines s] $ lines s

xAxis :: (Int, String) -> [(Int, Int, Char)]
xAxis (row, line) = zipWith (\x c -> (x, row, c)) [0 .. length line] line

-- (\(x, (y, z)) -> (x, y, z))
-- yAxis :: String -> [(Int, String)]
-- yAxis s = zip [0..length $ lines s] $ lines s