module Day03.Day03 where

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
        xAxis :: String -> [((Int, Int), Char)]
        xAxis s = foldl app [] $ yAxis s
            where
                app :: [((Int, Int), Char)] -> (Int, String) -> [((Int, Int), Char)]
                app acc (lineId, line) = foldl app2 acc (zip line [0 .. length line])
                    where
                        -- app2 :: 


yAxis :: String -> [(Int, String)]
yAxis s = zip [0..length $ lines s] $ lines s