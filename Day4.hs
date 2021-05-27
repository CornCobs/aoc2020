{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Day4 where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR (decimal)
import Data.Either (fromRight)
import Data.Char (isDigit)

requiredFields :: S.Set T.Text
requiredFields = S.fromList [
        "byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid"
    ]

-- solution for part 1
validPassport :: T.Text -> Bool
validPassport pass = null $ requiredFields S.\\ S.fromList (map (T.take 3) $ T.words pass)

-- solution for part 2
validPassport2 :: T.Text -> Bool
validPassport2 pass = all (`any` fields) [byr, iyr, eyr, hgt, hcl, ecl, pid]
    where fields = T.words pass

solution :: (T.Text -> Bool) -> T.Text -> Int
solution validPassport = length . filter validPassport . T.splitOn "\n\n" 

byr, iyr, eyr, hgt, hcl, ecl, pid :: T.Text -> Bool
byr = numberField "byr:" (1920, 2002)
iyr = numberField "iyr:" (2010, 2020)
eyr = numberField "eyr:" (2020, 2030)
hgt = validField "hgt:" $ \val -> fromRight False $ do
        (num, end) <- TR.decimal @Integer val
        case end of "cm" -> return $ inRangeIncl (150, 193) num 
                    "in" -> return $ inRangeIncl (59, 76) num
                    _    -> Left ""
hcl = validField "hcl:" $ \val -> 
    case T.uncons val of 
        Just ('#', hex) -> T.length hex == 6 && T.all isHex hex
        _               -> False
    where isHex x = isDigit x || inRangeIncl ('a', 'f') x
ecl = validField "ecl:" (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
pid = validField "pid:" $ \val -> T.length val == 9 && T.all isDigit val

validField :: T.Text -> (T.Text -> Bool) -> T.Text -> Bool
validField field fun txt = let (header, val) = T.splitAt 4 txt 
                           in field == header && fun val 

numberField :: T.Text -> (Integer, Integer) -> T.Text -> Bool
numberField field range = validField field $ \val -> fromRight False $ do 
        (num, extra) <- TR.decimal @Integer val 
        return $ inRangeIncl range num && T.null extra

inRangeIncl :: Ord a => (a, a) -> a -> Bool
inRangeIncl (lower, upper) val = val >= lower && val <= upper

main :: IO ()
main = TIO.readFile "input/4.txt" >>= print . solution validPassport2