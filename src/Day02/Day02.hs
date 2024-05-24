module Day02.Day02 (first, second) where
import qualified Data.Text as T
import Data.List ( maximumBy )
import Data.Char (isNumber, isAlpha)


first :: IO()
first = do
    input <- readFile "src/Day02/input.txt"
    let games = map toResultsWithId $ lines input
    let possible = foldl (\acc g -> acc + fst g) 0 (possibleGames games)
    print possible
    return ()

second :: IO()
second = do
    input <- readFile "src/Day02/input.txt"
    let games = map toResultsWithId $ lines input
    print $ sum $ map (resultPower . gameFewest) games

    return ()

data Result = Result {red::Int, green::Int, blue::Int} deriving Show

addResult :: Result -> (String, Int) -> Result
addResult acc ("red", count) = Result (((+) count . red) acc) (green acc) (blue acc)
addResult acc ("green", count) = Result (red acc) (((+) count . green) acc) (blue acc)
addResult acc ("blue", count) = Result (red acc) (green acc) (((+) count . blue) acc)
addResult acc _ = acc

toResult :: String -> Result
toResult s = foldl addResult (Result 0 0 0) $ toColors s
    where
        singleColor :: String -> (String, Int)
        singleColor color = (filter isAlpha color, read (filter isNumber color) :: Int)
        toColors :: String -> [(String, Int)]
        toColors str = map (singleColor . T.unpack) (T.split  (',' ==) (T.pack str))

resultPower :: Result -> Int
resultPower r = red r * green r * blue r

isResultPossible :: Result -> Bool
isResultPossible (Result r g b) = (r <= 12) && (g <= 13) && (b <= 14)

toResultsWithId :: String -> (Int, [Result])
toResultsWithId s =
    let t = T.pack s
        gameId :: Int
        gameId = read $ filter isNumber $ T.unpack . head $ T.split (':'==) t :: Int
        games :: [String]
        games = map T.unpack $ T.split (';'==) $ last $ T.split (':'==) t
    in
        (gameId, map toResult games)

possibleGames :: [(Int, [Result])] -> [(Int, [Result])]
possibleGames = filter isGamePossible
    where
        isGamePossible :: (Int, [Result]) -> Bool
        isGamePossible (_, results) = all isResultPossible results

gameFewest :: (Int, [Result]) -> Result
gameFewest (_, l) = Result (red $ maximumBy (\a b -> compare (red a) (red b)) l)
    (green $ maximumBy (\a b -> compare (green a) (green b)) l)
    (blue $ maximumBy (\a b -> compare (blue a) (blue b)) l)