module Day1 where

import Control.Monad (guard)

part1 :: [Int] -> Int
part1 lst = head $ do
    a <- lst 
    b <- lst 
    guard $ a + b == 2020 && a /= b
    return $ a * b

part2 :: [Int] -> Int
part2 lst = head $ do 
    a <- lst
    let lst' = filter (< 2020 - a) lst
    b <- lst'
    c <- lst'
    guard $ a + b + c == 2020 && a /= b && b /= c
    return $ a * b * c

main :: IO ()
main = do
    input <- fmap read . lines <$> readFile "input/1.txt"
    print $ part1 input
    print $ part2 input