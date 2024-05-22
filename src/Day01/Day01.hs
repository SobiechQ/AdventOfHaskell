module Day01.Day01 (first, second) where
import Data.Char (isDigit)
import qualified Data.Text as T (pack, replace, unpack)

first :: IO ()
first = do
  input <- readFile "src/Day01/input.txt"
  print $ sum $ map ((\n -> read n :: Int) . (\n -> head n : [last n]) . filter isDigit) $ lines input
  return ()

second :: IO ()
second = do
  input <- readFile "src/Day01/input.txt"
  print $ sum $ map ((\n -> read n :: Int) . (\n -> head n : [last n]) . filter isDigit . myReplace) $ lines input
  return ()

myReplace :: String -> String
myReplace = foldl app []
  where
    app :: String -> Char -> String
    app acc c = namesReplace (acc ++ [c])

namesReplace :: String -> String
namesReplace s =
  T.unpack
    ( ( T.replace (T.pack "one") (T.pack "1")
          . T.replace (T.pack "two") (T.pack "2")
          . T.replace (T.pack "three") (T.pack "3")
          . T.replace (T.pack "four") (T.pack "4")
          . T.replace (T.pack "five") (T.pack "5")
          . T.replace (T.pack "six") (T.pack "6")
          . T.replace (T.pack "seven") (T.pack "7")
          . T.replace (T.pack "eight") (T.pack "8")
          . T.replace (T.pack "nine") (T.pack "9")
      )
        (T.pack s)
    )
