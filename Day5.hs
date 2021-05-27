module Day5 where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Maybe (mapMaybe )
import Data.List ( sort )

data Partition = Lower | Upper deriving (Eq, Ord, Show)

type Seat = ([Partition], [Partition])

readBF :: Char -> Partition
readBF 'B' = Upper
readBF 'F' = Lower

readLR :: Char -> Partition
readLR 'L' = Lower
readLR 'R' = Upper

readPass :: String -> Seat
readPass = bimap (map readBF) (map readLR) . splitAt 7

binaryPartition :: [Partition] -> (Int, Int) -> Maybe Int
binaryPartition [] (l, u) = if l == u then Just l else Nothing
binaryPartition (x:xs) (l, u) = case x of 
    Lower -> binaryPartition xs (l, middle)
    Upper -> binaryPartition xs (middle + 1, u)
    where middle = (l + u) `div` 2

getID :: Seat -> Maybe Int
getID (rows, cols) = do 
    row <- binaryPartition rows (0, 127)
    col <- binaryPartition cols (0, 7)
    return $ row * 8 + col
    

part1 :: [String] -> Maybe Int
part1 = getID . maximum . map readPass

part2 :: [String] -> Int
part2 pass = let ids = sort $ mapMaybe (getID . readPass) pass 
                 find (x:xs) expected
                    | expected == x = find xs (x + 1)
                    | otherwise = expected
             in find ids (head ids) 

main :: IO ()
main = readFile "input/5.txt" >>= print . part2 . lines 