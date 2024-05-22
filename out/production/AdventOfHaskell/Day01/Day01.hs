module Day01.Day01 (first) where -- todo usun numbersonly
import Helpers.Reader
import Data.Char (isDigit)
import qualified  Data.Text as T (replace)


  
first :: IO ()
first = do
  input <- fileRead "src/Day01/input.txt"
  print $ sum $ map ((\n -> read n :: Int) . (\n -> head n : [last n]) . filter isDigit ) $ lines input
  return ()

second :: IO()
second = do
  input <- fileRead "src/Day01/input.txt"
  
  return ()
-- Yhah3OneasTwo8oneight -> Yhah31as281ight

--replace :: String -> String
--replace = 