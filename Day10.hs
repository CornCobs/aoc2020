module Day10 where

import Data.List (sort, foldl')
import Data.Maybe (mapMaybe)
import qualified Data.IntMap.Strict as M

part1 :: [Int] -> Int
part1 lst = let sorted = sort lst
                counter (ones, threes, prev) next = 
                    case next - prev of
                        3 -> (ones, threes + 1, next)
                        1 -> (ones + 1, threes, next)
                        _ -> (ones, threes, next)
                (ones, threes, _) = foldl' counter (0, 0, 0) sorted
            in ones * (threes + 1) -- device built-in adapter always 3 more than highest adapter thus threes + 1

-- dynamic programming algo using an IntMap
part2 :: [Int] -> Maybe (Int, Int)
part2 lst = let sorted = sort lst
                initWays = M.singleton 0 1
                finalWays = foldl' dp initWays sorted
                dp currMap val = let ways = sum $ mapMaybe (currMap M.!?) [val-3..val-1]
                                 in M.insert val ways currMap
            in M.lookupMax finalWays

main :: IO ()
main = readFile "input/10.txt" >>= print . part2 . map read . lines

