module Day7 where

import Text.Parsec hiding ( State )
import Text.Parsec.String ( Parser )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (forM_, forM, void)
import Data.Maybe (catMaybes)


parseBag :: Parsec String a String
parseBag = manyTill anyChar (try (string " bag" >> optional (char 's')))



parseGraph :: String -> Either ParseError (M.Map String [String])
parseGraph = runParser (many parseLine >> getState) M.empty "" 

parseLine :: Parsec String (M.Map String [String]) ()
parseLine = do 
    bag <- parseBag
    spaces 
    string "contain"
    spaces
    insides <- (many1 digit >> spaces >> parseBag) `sepBy` string ", "
    if null insides then void (string "no other bags.") else void (char '.')
    endOfLine
    let insertOrCons :: String -> Maybe [String] -> Maybe [String]
        insertOrCons thing Nothing = Just [thing]
        insertOrCons thing (Just things) = Just (thing:things) 
    forM_ insides $ \b -> do 
        modifyState (M.alter (insertOrCons bag) b)

target :: String
target = "shiny gold"

getAllParents :: S.Set String -> S.Set String -> M.Map String [String] -> S.Set String
getAllParents currSet new graph = 
    let new' = S.fromList $ concat $ catMaybes $ S.toList $ S.map (`M.lookup` graph) new
    in  if new' `S.isSubsetOf` currSet 
        then currSet
        else getAllParents (S.union currSet new) new' graph

part1 :: M.Map String [String] -> Int
part1 = S.size . getAllParents (S.singleton target) (S.singleton target)

solution1 :: String -> Either ParseError Int
solution1 input = part1 <$> parseGraph input

main :: IO ()
main = readFile "input/7.txt" >>= print . solution2


parseLineNum :: Parser (String, [(Integer, String)])
parseLineNum = do 
    bag <- parseBag
    spaces 
    string "contain"
    spaces
    let parseBagNum = do
            num <- read <$> many1 digit
            spaces
            bag <- parseBag
            return (num, bag)
    insides <- parseBagNum `sepBy` string ", "
    if null insides then void (string "no other bags.") else void (char '.')
    endOfLine
    return (bag, insides)

parseGraphNum :: String -> Either ParseError (M.Map String [(Integer, String)])
parseGraphNum = parse (M.fromList <$> many parseLineNum) "" 

dfs :: M.Map String [(Integer, String)] -> (Integer, String) -> Integer
dfs graph (count, node) =
    let children = graph M.! node
        totalBags = 1 + sum (map (dfs graph) children)
    in totalBags * count

solution2 :: String -> Either ParseError Integer
solution2 input = subtract 1 . (`dfs` (1, target)) <$> parseGraphNum input