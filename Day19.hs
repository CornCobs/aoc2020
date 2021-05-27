module Day19 where

import Control.Monad ( foldM )
import qualified Data.IntMap.Strict as M
import Text.Parsec
import Text.Parsec.String ( Parser )

data Rule = Exact Char | Refs [Int] deriving Show

-- evalRule is lazy and thus handles the infinite case (part 2) perfectly without modification
evalRule :: M.IntMap [Rule] -> Rule -> String -> [String]
evalRule _ _ "" = []
evalRule rules (Exact c) (x:xs) = [xs | x == c]
evalRule rules (Refs ls) str = foldM (\str' rulels -> concatMap (\rule -> evalRule rules rule str') rulels) str (map (rules M.!) ls) 

match0 :: M.IntMap [Rule] -> String -> Bool
match0 rules str = any null $ evalRule rules (head $ rules M.! 0) str

main :: IO ()
main = do 
    input <- readFile "input/19pt2.txt"
    let Right (rules, messages) = parse parseInput "" input
    print $ length $ filter (match0 rules) messages


-- Menial parsing work below

int :: Parser Int
int = read <$> many1 digit

parseExact :: Parser Rule
parseExact = Exact <$> try (char ' ' *> between (char '"') (char '"') anyChar)

parseRefs :: Parser Rule
parseRefs = Refs <$> try (char ' ' *> int `sepEndBy1` char ' ')

parseRules :: Parser (M.IntMap [Rule])
parseRules = M.fromList <$> many1 (do 
    ix <- int
    char ':'
    rules <- return <$> parseExact <|> parseRefs `sepBy1` char '|'
    endOfLine
    return (ix, rules))

parseInput :: Parser (M.IntMap [Rule], [String])
parseInput = do
    rules <- parseRules
    spaces
    messages <- getInput
    return (rules, lines messages)