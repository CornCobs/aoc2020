{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.Char (isSpace)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

part1 :: [T.Text] -> Int
part1 = sum . map countDistinct
    where countDistinct = S.size . S.fromList . T.unpack . T.filter (not . isSpace)

part2 :: [T.Text] -> Int
part2 = sum . map countAllAgree
    where countAllAgree = S.size . foldr1 S.intersection . map (S.fromList . T.unpack) . T.lines

main :: IO ()
main = TIO.readFile "input/6.txt" >>= print . part2 . T.splitOn "\n\n"