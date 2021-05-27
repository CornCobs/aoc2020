{-# LANGUAGE ForeignFunctionInterface #-}

module Day23 where

import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict ((!))
import Data.Function ((&))

import Foreign.C.Types (CLong(..))

-- implements wrap around subtraction from 1..max
nextLabel :: Int -> Int -> Int
nextLabel max x = ((x+max-2) `mod` max) + 1

move :: [Int] -> [Int]
move (curr:cups) = 
    let next = nextLabel 9
        (removed, rest) = splitAt 3 cups
        circle = rest ++ [curr]
        destination = until (not.(`elem` removed)) next (next curr)
        (front, dest:back) = break (== destination) circle
    in front ++ (dest:removed) ++ back

input :: [Int]
input = [3,2,7,4,6,5,1,8,9]

part1 :: [Int]
part1 = let final = iterate move input !! 100
            (before, one:after) = break (== 1) final
        in after ++ before

{- Brute Force Approach (Doesn't work)

part2Input :: [Int]
part2Input = input ++ [10..1000000]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

part2 :: Int
part2 = let final = applyN 1000 move part2Input
            one:rest = dropWhile (/= 1) final
            [a, b] = take 2 rest
        in a * b

---------------------------------------}

part2Input :: IM.IntMap Int
part2Input = IM.fromList $ zip cupNums nextCups
    where cupNums = input ++ [10..1000000]
          nextCups = tail cupNums ++ take 1 cupNums

nextLabel2 :: Int -> Int
nextLabel2 = nextLabel 1000000

move2 :: (Int, IM.IntMap Int) -> (Int, IM.IntMap Int)
move2 (current, circle) = 
    let c1 = circle ! current 
        c2 = circle ! c1
        c3 = circle ! c2
        next = circle ! c3
        destination = until (\x -> x /= c1 && x /= c2 && x /= c3) nextLabel2 (nextLabel2 current)
        newCircle = IM.insert current next circle
                  & IM.insert destination c1
                  & IM.insert c3 (circle ! destination)
    in (next, newCircle)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN n f x = applyN (n-1) f (f x)

part2 :: Int
part2 = fromIntegral c_solve


foreign import ccall "solve" c_solve :: CLong