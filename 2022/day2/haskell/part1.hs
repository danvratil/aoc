{-# LANGUAGE BlockArguments #-}

import Text.ParserCombinators.Parsec
import Text.Printf

data Object = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show)
data Outcome = Win | Draw | Loss deriving (Eq, Ord, Enum, Show)

generateGame :: [[Outcome]]
generateGame = [
    --               Rock   Paper   Scissors
    {-     Rock -} [ Draw,  Loss,   Win ],
    {-    Paper -} [ Win,   Draw,   Loss ],
    {- Scissors -} [ Loss,  Win,    Draw ]
    ]

game = generateGame

_parseObject :: Parser Object
_parseObject = do
    s <- oneOf "ABCXYZ"
    return $ case s of
        'A' -> Rock
        'B' -> Paper
        'C' -> Scissors
        'X' -> Rock
        'Y' -> Paper
        'Z' -> Scissors

parseObject :: String -> Object
parseObject o = case parse _parseObject "" o of
    Right obj -> obj
    Left _ -> error "Failed to parse input"

objectScore :: Object -> Int
objectScore o = case o of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

objectIndex :: Object -> Int
objectIndex o = case o of
    Rock -> 0
    Paper -> 1
    Scissors -> 2

outcomeScore :: Outcome -> Int
outcomeScore o = case o of
    Win -> 6
    Draw -> 3
    Loss -> 0

resolveOutcome :: (Object, Object) -> Outcome
resolveOutcome (p1, p2) = game !! objectIndex p2 !! objectIndex p1

calculateScore :: (Object, Object) -> Int
calculateScore (p1, p2) = objectScore p2 + outcomeScore (resolveOutcome (p1, p2))

parseLine :: [String] -> (Object, Object)
parseLine (p1:p2:_) = (parseObject p1, parseObject p2)

totalScore :: [String] -> Int
totalScore = foldr ((+) . calculateScore . parseLine . words) 0

main :: IO ()
main = do
    content <- readFile "input"
    let r = totalScore (lines content)
    print r

