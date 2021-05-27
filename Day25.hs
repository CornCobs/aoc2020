module Day25 where

cardPK = 1965712
doorPK = 19072108

step :: Int -> Int -> Int
step subjectNum val = (val * subjectNum) `rem` 20201227

getLoopSizes :: (Int, Int)
getLoopSizes = transform (Nothing, Nothing) 0 1
    where transform (Just x, Just y) _ _ = (x, y)
          transform (a, b) count val
            | val == cardPK = transform (Just count, b) (count+1) (step 7 val)
            | val == doorPK = transform (a, Just count) (count+1) (step 7 val)
            | otherwise = transform (a, b) (count+1) (step 7 val)

getEncryptionKey :: Int -> Int -> Int
getEncryptionKey loopsize pk = (!! loopsize) . iterate (step pk) $ 1