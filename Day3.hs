{-# LANGUAGE BangPatterns #-}
module Day3 where

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.Maybe (fromMaybe)

parseLine :: String -> S.IntSet
parseLine = S.fromDistinctAscList . foldr collectTrees [] . zip [0..] 
    where collectTrees (ix, '#') = (:) ix
          collectTrees _ = id

parseMap :: [String] -> M.IntMap S.IntSet
parseMap = M.fromDistinctAscList . foldr (\(ix, line) acc -> (ix, parseLine line):acc) [] . zip [0..]

solution :: (Int, Int) -> String -> Int
solution (right, down) trees = 
    let rows = lines trees
        width = length $ head rows
        height = length rows
        imap = parseMap rows
        go row col !treeCount
          | row < height = if S.member col $ fromMaybe S.empty $ M.lookup row imap
                           then go (row+down) col' (treeCount + 1)
                           else go (row+down) col' treeCount
          | otherwise = treeCount
          where col' = (col + right) `mod` width
    in go 0 0 0

slopes :: [(Int, Int)]
slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

main :: IO ()
main = do 
    input <- readFile "input/3.txt"
    print $ product $ map (`solution` input) slopes

