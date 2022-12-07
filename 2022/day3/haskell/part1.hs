calculatePriority :: Char -> Int
calculatePriority c = 0


findCommonElement :: [Char] -> [Char] -> Maybe Char
findCommonElement (s:xs1) s2 = if (elem s s2) then Just s else findCommonElement xs1 s2
findCommonElement [] s2 = Nothing

processLine:: (String, String) -> Int
processLine(s1, s2) = case findCommonElement s1 s2 of
    Just s -> calculatePriority s
    Nothing -> error "Invalid input"

parseLine :: String -> (String, String)
parseLine line = splitAt (length line / 2) line

totalPriorities :: [String] -> Int
totalPriorities = foldr ((+) . processLine . parseLine ) 0

main :: IO ()
main = do
    content <- readFile "input"
    let r = totalPriorities (lines content)
    print r
