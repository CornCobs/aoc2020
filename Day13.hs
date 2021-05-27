{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as T
import System.IO
import Data.Either (fromRight, rights)
import Data.Bifunctor
import Data.List (iterate', find, sortOn)

getEarliestTime :: Int -> Int -> (Int, Int)
getEarliestTime arrival busId = 
    let nearest = busId * (arrival `div` busId)
    in if nearest == arrival 
        then (0, busId)
        else (nearest + busId - arrival, busId)

earliestBus :: Int -> [Int] -> Int
earliestBus arrival lst = uncurry (*) $ minimum $ map (getEarliestTime arrival) lst

part1 :: IO ()
part1 = withFile "input/13.txt" ReadMode $ \hdl -> do
    arrival <- fromRight 0 . fmap fst . T.decimal <$> TIO.hGetLine hdl
    buses <- map fst . rights . map T.decimal . filter (/= "x") . T.splitOn "," <$> TIO.hGetLine hdl
    print $ earliestBus arrival buses


-- | Part 2 | ---------------------------------------------------------------------------------------
-- Make use of a modified Chinese Remainder Theorem sieving method to find solution
-- since all bus IDs are prime

parseBusOffsets :: T.Text -> [(Integer, Integer)]
parseBusOffsets txt = sortOn (negate . snd) $ rights $ map parse $ zip [0..] $ T.splitOn "," txt
    where parse :: (Integer, T.Text) -> Either String (Integer, Integer)
          parse (ix, t) = case T.decimal t of 
            Right (ans, _) -> Right ((ans - ix) `mod` ans , ans) -- we take ans - ix as the remainder to find (but mod ans to make)
            Left z -> Left z                                     -- it within the range 0..ans (else if ans < ix we get negative remainder)

getCongruentModulo :: (Integer, Integer) -> [Integer]
getCongruentModulo (remainder, x) = iterate' (+ x) remainder

getSyncTime :: [(Integer, Integer)] -> (Integer, Integer)
getSyncTime = foldl1 sync 
    where sync (rem1, x) (rem2, y) =
            let Just nextVal = find (\x1 -> x1 `mod` y == rem2) $ getCongruentModulo (rem1, x)
            in  (nextVal, x * y)

part2 :: IO ()
part2 = withFile "input/13.txt" ReadMode $ \hdl -> do
    TIO.hGetLine hdl
    offsets <- parseBusOffsets <$> TIO.hGetLine hdl
    print $ getSyncTime offsets

main :: IO ()
main = part2