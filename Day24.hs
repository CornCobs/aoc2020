module Day24 where

import qualified Data.Set as S

steps :: String -> (Int, Int) -> (Int, Int)
steps "" pos = pos
steps ('e':dirs) (x, y) = steps dirs (x+2, y)
steps ('w':dirs) (x, y) = steps dirs (x-2, y)
steps ('n':'e':dirs) (x, y) = steps dirs (x+1, y+1)
steps ('n':'w':dirs) (x, y) = steps dirs (x-1, y+1)
steps ('s':'e':dirs) (x, y) = steps dirs (x+1, y-1)
steps ('s':'w':dirs) (x, y) = steps dirs (x-1, y-1)

flips :: S.Set (Int, Int) -> [String]  -> S.Set (Int, Int)
flips blacks [] = blacks
flips blacks (x:xs) = let tile = steps x (0,0)
                          blacks' = if S.member tile blacks
                                    then S.delete tile blacks
                                    else S.insert tile blacks
                      in flips blacks' xs

part1 :: String -> Int
part1 = S.size . flips S.empty . lines

neighbours :: (Int, Int) -> S.Set (Int, Int)
neighbours (x, y) = S.fromList
                    [ (x+2, y)
                    , (x-2, y)
                    , (x+1, y+1)
                    , (x-1, y+1)
                    , (x+1, y-1)
                    , (x-1, y-1)
                    ]

-- simulate a single day passing
day :: S.Set (Int, Int) -> S.Set (Int, Int)
day blacks = S.filter rules neighbourSet
    where neighbourSet = S.unions (S.map neighbours blacks)
          rules pos
            | pos `S.member` blacks = not $ blackNeighbours == 0 || blackNeighbours > 2
            | otherwise = blackNeighbours == 2
            where blackNeighbours = S.size $ S.filter (`S.member` blacks) $ neighbours pos

part2 :: String -> Int
part2 = S.size . (!! 100) . iterate day . flips S.empty . lines