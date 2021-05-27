module Day14 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (intToDigit, digitToInt)
import Data.Bifunctor
import Control.Monad (forM_, foldM)

import Text.Parsec
import qualified Data.IntMap.Strict as M

type Interpreter = Parsec T.Text (T.Text, M.IntMap T.Text)

toBinary :: Int -> T.Text
toBinary val = T.justifyRight 36 '0' $ T.pack $ go val []
    where go x accum | x == 0 = accum
                     | otherwise = let (quotient, remainder) = x `quotRem` 2
                                   in go quotient (intToDigit remainder:accum)

toInt :: T.Text -> Int
toInt = T.foldl' (\acc c -> acc * 2 + digitToInt c) 0 

mask :: T.Text -> T.Text -> T.Text
mask = T.zipWith bitMask
    where bitMask 'X' b = b
          bitMask x   _ = x

parseMask :: Interpreter ()
parseMask = do 
    string "mask = "
    mask <- T.pack <$> manyTill anyChar endOfLine 
    modifyState (first (const mask))

parseMem :: Interpreter ()
parseMem = do
    let number = read <$> many1 digit
    string "mem"
    mem <- between (char '[') (char ']') number
    string " = "
    currMask <- fst <$> getState
    val <- mask currMask . toBinary <$> number
    endOfLine
    modifyState $ second $ M.insert mem val


-- | Part 2 | --------------------------------------------------------------------

-- | In part 2, we need to change our binary to value convertor (formerly 'toInt' above in part 1)
-- The main difference is since it is non-deterministic (X masks result in floating values which take all possible values)
-- thus we make use of the List monad and foldM instead of foldl
decodeMemAddress :: Int -> T.Text -> [Int]
decodeMemAddress addr mask = 
    let bitMask '0' b = b
        bitMask x   _ = x
        maskedAddr = T.zipWith bitMask mask (toBinary addr)
    in foldM (\acc c ->
            case c of 
                'X' -> do
                    options <- [0,1]
                    return (2 * acc + options)
                d   -> return (2 * acc + digitToInt d)) 0 $ T.unpack maskedAddr


parseMem2 :: Interpreter ()
parseMem2 = do 
    let number = read <$> many1 digit
    string "mem"
    mem <- between (char '[') (char ']') number
    string " = "
    currMask <- fst <$> getState
    let addresses = decodeMemAddress mem currMask
    val <- toBinary <$> number
    endOfLine
    forM_ addresses $ \addr -> modifyState $ second $ M.insert addr val

-- | memParser is parseMem in part1 and parseMem2 in part2
interpret :: Interpreter () -> T.Text -> Either ParseError Int
interpret memParser prog = sum . M.elems <$> runParser parseProg (T.empty, M.empty) "" prog
    where parseProg = do 
            many (try parseMask <|> try memParser)
            M.map toInt . snd <$> getState


main :: IO ()
main = TIO.readFile "input/14.txt" >>= print . interpret parseMem2