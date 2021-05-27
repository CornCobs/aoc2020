{-# LANGUAGE LambdaCase #-}

module Day11 where

import qualified Data.Map.Strict as M
import Data.Function ((&))
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Data.Bifunctor ( Bifunctor(second, bimap, first) )



data Seat = Occupied | Free deriving (Eq, Show)

parseMap :: String -> M.Map (Int, Int) Seat
parseMap txt = zip [0..] (lines txt) & 
               foldl' (\currMap (row, line) -> 
                   zip [0..] line &
                   foldl' (\currMap' (col, char) -> 
                       if char == 'L'
                        then M.insert (row, col) Free currMap'
                        else currMap'
                   ) currMap 
               ) M.empty

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (r, c) = do
    r' <- [r-1, r, r+1]
    c' <- [c-1, c, c+1]
    guard $ not (r' == r && c' == c)
    return (r', c')

seatChange :: M.Map (Int, Int) Seat -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)
seatChange seats alreadyChanged pos = \case
    Free -> if Occupied `elem` adjacentSeats
            then (alreadyChanged, Free)
            else (True, Occupied)
    Occupied -> if 4 <= length (filter (== Occupied) adjacentSeats)
                then (True, Free)
                else (alreadyChanged, Occupied)
    where adjacentSeats = mapMaybe (seats M.!?) (getNeighbours pos)

-- simulate a single round of seat changes
simulate :: M.Map (Int, Int) Seat -> (Bool, M.Map (Int, Int) Seat)
simulate prev = M.mapAccumWithKey (seatChange prev) False prev

-- simulate rounds until stability, then count occupied seats
solution :: (M.Map (Int, Int) Seat -> (Bool, M.Map (Int, Int) Seat)) -> M.Map (Int, Int) Seat -> Int
solution simulator prev = 
    let (changes, next) = simulator prev
    in if changes
        then solution simulator next
        else M.size (M.filter (== Occupied) prev)

part1 :: M.Map (Int, Int) Seat -> Int
part1 = solution simulate

-- | Part 2 | ------------------------------------------------------

withinBounds :: (Int, Int) -> (Int, Int) -> Bool
withinBounds (height, width) (row, col) = row >= 0 && row <= height && col >= 0 && col <= width

getFirstSeat :: M.Map (Int, Int) Seat -> (Int, Int) -> (Int, Int) -> ((Int, Int) -> (Int, Int)) -> Maybe (Int, Int)
getFirstSeat seats bounds pos update = go (update pos)
    where go coord | not (withinBounds bounds coord) = Nothing
                   | M.member coord seats = Just coord
                   | otherwise = go (update coord)

getNeighbours2 :: M.Map (Int, Int) Seat -> M.Map (Int, Int) [(Int, Int)]
getNeighbours2 seats = 
    let bounds = fst $ M.findMax seats
        directions = 
            [ bimap (+1) (+1)
            , bimap (subtract 1) (subtract 1)
            , bimap (+1) (subtract 1)
            , bimap (subtract 1) (+1)
            , first (+1)
            , first (subtract 1)
            , second (+1)
            , second (subtract 1)
            ] :: [(Int, Int) -> (Int, Int)]
    in M.mapWithKey (\pos _ -> mapMaybe (getFirstSeat seats bounds pos) directions) seats

seatChange2 :: M.Map (Int, Int) [(Int, Int)] -> M.Map (Int, Int) Seat -> Bool -> (Int, Int) -> Seat -> (Bool, Seat)
seatChange2 neighbourMap seats alreadyChanged pos = \case
    Free -> if Occupied `elem` adjacentSeats
            then (alreadyChanged, Free)
            else (True, Occupied)
    Occupied -> if 5 <= length (filter (== Occupied) adjacentSeats)
                then (True, Free)
                else (alreadyChanged, Occupied)
    where adjacentSeats = map (seats M.!) (neighbourMap M.! pos)

part2 :: M.Map (Int, Int) Seat -> Int
part2 seats = 
    let neighbourMap = getNeighbours2 seats
        simulate2 prev = M.mapAccumWithKey (seatChange2 neighbourMap prev) False prev
    in solution simulate2 seats


main :: IO ()
main = readFile "input/11.txt" >>= print . part2 . parseMap