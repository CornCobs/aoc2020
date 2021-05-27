module Day9 where

import qualified Data.Vector.Unboxed as V
import Data.List (find)
import Control.Monad (guard)
import Debug.Trace

part1 :: V.Vector Int -> Maybe Int
part1 numbers = find noProperty [25..] >>= (numbers V.!?)
    where preIx = [0..24]
          noProperty ix = 
            let val = numbers V.! ix 
                preamble = V.slice (ix - 25) 25 numbers
            in null $ do
                a <- preIx
                b <- preIx 
                guard $ (a /= b) && (preamble V.! a + preamble V.! b == val)
                
main :: IO ()
main = do
    input <- V.fromList . map read . lines <$> readFile "input/9.txt"
    let sol1 = part1 input
    print sol1
    print $ flip part2 input <$> sol1

part2 :: Int -> V.Vector Int -> [Int]
part2 target numbers = 
    let cumsum = V.scanl' (+) 0 numbers
        indices = [0..V.length cumsum - 1]
    in do 
        a <- indices
        b <- [a+1..V.length cumsum - 1]
        guard $ cumsum V.! b - cumsum V.! a == target
        let range = V.slice a (b-a) numbers
        return $ V.maximum range + V.minimum range