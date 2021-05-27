{-# LANGUAGE TemplateHaskell #-}

module Lens where

import Lens.Micro.Platform

data MyObject = MO { _myField :: Int }

makeLenses ''MyObject

myList :: [MyObject]
myList = MO <$> [1..10]

-- question: how to extract [Int] from myList?

answer :: [Int]
answer = myList^..each.myField