module Day15 where

import qualified Data.IntMap as M

input :: [Int]
input = [0,13,1,16,6,17]

play :: Int -> M.IntMap Int -> Int
play turn prevTurns
    | turn > 2020 = prevTurns M.! 2020
    | otherwise = let Just (lastSpoken, beforePrev) = M.maxView prevTurns
                      sameNumber = M.filter (== lastSpoken) beforePrev
                      nextNumber = if M.null sameNumber 
                                    then 0
                                    else turn - 1 - fst (M.findMax sameNumber)
                  in  play (turn + 1) $ M.insert turn nextNumber prevTurns 
                      
part1 :: Int
part1 = play (length input + 1) $ M.fromDistinctAscList $ zip [1..] input

-- | Part 2 | -----------------------------------------------------------------
-- Part 2 requires a rewrite to make lookup of previous much faster. Instead of storing (turn, number) as (k, v) pairs
-- we instead store (number, lastSeen) (k, v) pairs in the IntMap. Thus lookup of previous seen turn of any number is O(lgN)
-- In fact this is what I meant to do in part 1 but kinda got it upside down. LOL

play2 :: Int -> Int -> M.IntMap Int -> Int
play2 turn prevNumber prevTurns 
    | turn > 30000000 = prevNumber
    | otherwise = case prevTurns M.!? prevNumber of 
        Nothing       -> play2 (turn + 1) 0 $ M.insert prevNumber (turn - 1) prevTurns 
        Just lastSeen -> play2 (turn + 1) (turn - 1 - lastSeen) $ M.insert prevNumber (turn - 1) prevTurns

startMap :: M.IntMap Int
startMap = M.fromList $ zip input [1..]

part2 :: Int
part2 = play2 8 0 startMap

main :: IO ()
main = print part2