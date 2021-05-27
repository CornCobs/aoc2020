{-# LANGUAGE TemplateHaskell #-}

module Day12 where

import Lens.Micro.Platform
import Data.List (foldl')

data Direction = N | E | S | W deriving (Eq, Show)

instance Enum Direction where
  toEnum i = case i `mod` 4 of 
    0 -> N
    1 -> E
    2 -> S
    3 -> W
  fromEnum N = 0
  fromEnum E = 1
  fromEnum S = 2
  fromEnum W = 3

data Nav = Nav 
    { _lat :: Int
    , _lng :: Int
    , _facing :: Direction
    }
    deriving (Show)

data WayPointNav = WP
    { _ship :: Nav
    , _waypoint :: Nav
    } 

makeLenses ''Nav
makeLenses ''WayPointNav

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ = id
applyN x fn = applyN (x - 1) fn . fn

manhattan :: Nav -> Int
manhattan nav = abs (nav^.lat) + abs (nav^.lng)

move :: Nav -> String -> Nav
move nav (action:valStr) = case action of 
    'N' -> nav & lat +~ val
    'S' -> nav & lat -~ val
    'E' -> nav & lng +~ val
    'W' -> nav & lng -~ val
    'F' -> move nav (show (nav^.facing) ++ valStr)
    'R' -> nav & facing %~ applyN (val `div` 90) succ
    'L' -> nav & facing %~ applyN (val `div` 90) pred
    where val = read valStr

part1 :: String -> Int
part1 = manhattan . foldl' move Nav { _lat = 0, _lng = 0, _facing = E } . lines

-- | Part 2 | --------------------------------------------------------------------

forward :: WayPointNav -> Int -> WayPointNav
forward nav val = nav & ship.lat +~ (nav^.waypoint.lat * val)
                      & ship.lng +~ (nav^.waypoint.lng * val)

rotateRight90 :: WayPointNav -> WayPointNav
rotateRight90 nav = nav & waypoint.lat .~ (- nav^.waypoint.lng)
                        & waypoint.lng .~ (  nav^.waypoint.lat)

action :: WayPointNav -> String -> WayPointNav
action nav (action:valStr) = case action of 
    'N' -> nav & waypoint.lat +~ val
    'S' -> nav & waypoint.lat -~ val
    'E' -> nav & waypoint.lng +~ val
    'W' -> nav & waypoint.lng -~ val
    'F' -> forward nav val
    'R' -> applyN (val `div` 90) rotateRight90 nav
    'L' -> applyN ((-val `div` 90) `mod` 4) rotateRight90 nav
    where val = read valStr

part2 :: String -> Int
part2 = manhattan . (^.ship) . foldl' action initial . lines
    where initial = WP (Nav { _lat = 0, _lng = 0, _facing = E }) (Nav { _lat = 1, _lng = 10, _facing = E })

main :: IO ()
main = readFile "input/12.txt" >>= print . part2