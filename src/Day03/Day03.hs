module Day03.Day03 where
import qualified Data.Foldable

first :: IO()
first = do
    input <- readFile "src/Day03/input.txt"
    print input

    return ()

data Position = Position {x :: Int, y :: Int} deriving Show
data Part = SymbolPart {c :: Char, position :: Position} | NumberPart {n :: Int, position :: Position}  deriving Show

symbolPars :: String -> [Part]
symbolPars = undefined
    where
        yAxis :: String -> [(Int, String)]
        yAxis s = zip [0..length $ lines s] $ lines s
        xAxis :: (Int, String) -> [(Int, Int, Char)]
        xAxis (row, line) = zipWith (\x c -> (x, row, c)) [0..length line] line





-- (\(x, (y, z)) -> (x, y, z))
-- yAxis :: String -> [(Int, String)]
-- yAxis s = zip [0..length $ lines s] $ lines s