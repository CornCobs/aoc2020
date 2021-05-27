module Day2 where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Control.Monad (liftM2)

isValid1 :: String -> Maybe Bool
isValid1 pass = do
    [rangestr, [letter, ':'], passstr] <- return $ words pass 
    (lower, upper) <- getRange rangestr
    let letterCount = length . filter (== letter) $ passstr
    return $ letterCount >= lower && letterCount <= upper

getRange :: String -> Maybe (Int, Int)
getRange range = do
    (lower, '-':upper) <- return $ break (== '-') range
    liftM2 (,) (readMaybe lower) (readMaybe upper)

isValid2 :: String -> Maybe Bool
isValid2 pass = do
    [rangestr, [letter, ':'], passstr] <- return $ words pass 
    (lower, upper) <- getRange rangestr
    return $ (==) 1 $
        foldl' (\count (ix, char) -> 
                if (ix == lower || ix == upper) && char == letter 
                    then count + 1 
                    else count) 
               0
               $ zip [1..] passstr


solution :: (String -> Maybe Bool) -> String -> Int
solution pred = length . filter (fromMaybe False) . map pred . lines

main :: IO ()
main = (solution isValid2 <$> readFile "input/2.txt") >>= print
    
    