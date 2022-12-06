{-# LANGUAGE BlockArguments #-}

import Text.ParserCombinators.Parsec
import Text.Printf

data Object = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show)
data Outcome = Win | Draw | Loss deriving (Eq, Ord, Enum, Show)

generateGame :: [[Object]]
generateGame = [
    --           Rock      Paper     Scissors
    {-  Win -} [ Paper,    Scissors, Rock     ],
    {- Loss -} [ Scissors, Rock,     Paper    ],
    {- Draw -} [ Rock,     Paper,    Scissors ]
    ]

game = generateGame

_parseObject :: Parser Object
_parseObject = do
    s <- oneOf "ABC"
    return $ case s of
        'A' -> Rock
        'B' -> Paper
        'C' -> Scissors

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

outcomeIndex :: Outcome -> Int
outcomeIndex o = case o of
    Win -> 0
    Loss -> 1
    Draw -> 2

_parseOutcome :: Parser Outcome
_parseOutcome = do
    s <- oneOf "XYZ"
    return $ case s of
        'X' -> Loss
        'Y' -> Draw
        'Z' -> Win

parseOutcome :: String -> Outcome
parseOutcome o = case parse _parseOutcome "" o of
    Right outcome -> outcome
    Left _ -> error "Failed to parse input"

resolveObject :: Object -> Outcome -> Object
resolveObject obj outcome = game !! outcomeIndex outcome !! objectIndex obj

calculateScore :: (Object, Outcome) -> Int
calculateScore (p1, outcome) = objectScore (resolveObject p1 outcome) + outcomeScore outcome

parseLine :: [String] -> (Object, Outcome)
parseLine (p1:p2:_) = (parseObject p1, parseOutcome p2)

totalScore :: [String] -> Int
totalScore = foldr ((+) . calculateScore . parseLine . words) 0

main :: IO ()
main = do
    content <- readFile "input"
    let r = totalScore (lines content)
    print r

