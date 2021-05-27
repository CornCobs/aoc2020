{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import qualified Data.Text as T
import qualified Data.Set as S
import Debug.Trace

parse :: T.Text -> ([Int], [Int])
parse input = let (p1, p2) = T.breakOn "\n\n" input
                  stack1 = map (read . T.unpack) $ tail $ filter (not.T.null) $ T.lines p1
                  stack2 = map (read . T.unpack) $ tail $ filter (not.T.null) $ T.lines p2
              in (stack1, stack2)

playGame :: ([Int], [Int]) -> Int
playGame (c1:s1, c2:s2)
    | c1 > c2 = playGame (s1 ++ [c1, c2], s2)
    | otherwise = playGame (s1, s2 ++ [c2, c1])
playGame (s1, s2) = snd $ foldr (\val (mult, sum) -> (mult+1, sum + mult*val)) (1, 0) (s1 ++ s2)

part1 :: T.Text -> Int
part1 = playGame . parse

gameStateCode :: ([Int], [Int]) -> Int
gameStateCode (s1, s2) = snd $ foldr (\val (mult, acc) -> (mult+1, acc + mult*val)) (1, 0) (s1 ++ s2)

data Winner = P1 | P2 deriving Show

recursiveCombat :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> (Int, Winner)
recursiveCombat seen st@(c1:s1, c2:s2) 
    | st `S.member` seen = (gameStateCode (c1:s1, []), P1) -- Game instantly ends to prevent infinite recursion
    | c1 <= length s1 && c2 <= length s2 = 
        let (_, subgameWinner) = recursiveCombat S.empty (take c1 s1, take c2 s2)
        in case subgameWinner of P1 -> recursiveCombat seen' (s1 ++ [c1, c2], s2)
                                 P2 -> recursiveCombat seen' (s1, s2 ++ [c2, c1])
    | c1 > c2 = recursiveCombat seen' (s1 ++ [c1, c2], s2)
    | otherwise = recursiveCombat seen' (s1, s2 ++ [c2, c1])
    where seen' = S.insert st seen
recursiveCombat seen st@(s1, []) = (gameStateCode st, P1)
recursiveCombat seen st@([], s2) = (gameStateCode st, P2)