module Day02.Day02 (first) where
import qualified Data.Text as T

first :: IO()
first = do
    input <- readFile "src/Day02/input.txt"
    let myLines = lines input
    return ()

data Result result = Result {red::Int, green::Int, blue::Int}

--Game 1: 4 red, 3 blue; 6 blue, 16 green; 9 blue, 13 green, 1 red; 10 green, 4 red, 6 blue
toResults :: String -> [Result(Int, Int, Int)]
toResults = undefined

--4 red, 3 blue, 5 green
-- toResult :: String -> Result (Int, Int, Int)
-- toResult s = map (\_ -> T.unpack) (T.split (\_ -> (==) ',') (T.pack s))