module Day16 where

import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String
import Data.List
import Data.Ord

type Range = (Int, Int)

inRange :: Int -> Range -> Bool
inRange val (lower, upper) = val >= lower && val <= upper

invalidTicket :: [(Range, Range)] -> [Int] -> Bool
invalidTicket rules = any (\val -> all (\(r1, r2) -> not (val `inRange` r1 || val `inRange` r2)) rules)

getErrors :: [(Range, Range)] -> [Int] -> [Int]
getErrors rules = filter (\val -> all (\(r1, r2) -> not (val `inRange` r1 || val `inRange` r2)) rules)

integer :: Parser Int
integer = read <$> many1 digit

parseRange :: Parser Range
parseRange = (,) <$> integer <* char '-' <*> integer

parseRule :: Parser (String, (Range, Range))
parseRule = do 
    rule <- manyTill anyChar (char ':')
    space
    range1 <- parseRange
    string " or "
    range2 <- parseRange
    endOfLine
    return (rule, (range1, range2))

parseTicket :: Parser [Int]
parseTicket = integer `sepBy1` char ',' <* endOfLine

parseInput :: Parser (M.Map String (Range, Range), [Int], [[Int]])
parseInput = do 
    rules <- M.fromList <$> manyTill parseRule endOfLine
    spaces 
    string "your ticket:"
    spaces 
    yours <- parseTicket
    spaces
    string "nearby tickets:"
    spaces
    tickets <- many1 parseTicket
    return (rules, yours, tickets)

part1 :: Parser Int
part1 = parseInput >>= \(rules, _, others) -> 
    return $ sum $ concatMap (getErrors (M.elems rules)) others

-- | Test if a field is valid for all tickets (within given range of rule)
allValid :: (Range, Range) -> [Int] -> Bool
allValid (r1, r2) = all (\val -> val `inRange` r1 || val `inRange` r2)

getAllValids :: M.Map String (Range, Range) -> [[Int]] -> [[String]]
getAllValids rules tickets = 
    let validTickets = filter (not.invalidTicket (M.elems rules)) tickets 
        fields = transpose validTickets 
    in map (\field -> M.keys $ M.filter (`allValid` field) rules) fields

part2 :: Parser Integer
part2 = parseInput >>= \(rules, self, others) ->
    return $ solveConstraints (getAllValids rules others) self

-- | This only works for this input since the fields have 1, 2, 3, ... , 20 possible matches 
solveConstraints :: [[String]] -> [Int] -> Integer
solveConstraints matches self = 
    let (sorted, sortedSelf) = unzip $ sortOn (length . fst) $ zip matches self
        go (x:xs) = head x : go (map (filter (/= head x)) xs)
        go [] = []
        solvedFields = go sorted
    in product $ map (toInteger . snd) $ filter (isPrefixOf "departure" . fst) $ zip solvedFields sortedSelf

main :: IO ()
main = readFile "input/16.txt" >>= print . runParser part2 () "" 