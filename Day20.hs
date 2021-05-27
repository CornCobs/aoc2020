{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiWayIf #-}

module Day20 where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as TIO
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.List
import Data.Bifunctor (Bifunctor(bimap))
import Control.Monad.State
import Lens.Micro.Platform
import Data.Maybe
import Debug.Trace

parseInput :: T.Text -> IM.IntMap [T.Text]
parseInput puzzle = IM.fromList $ map parseSingle $ filter (not . T.null) $ T.splitOn "\n\n" puzzle
    where parseSingle txt = 
            let (tile:img) = T.lines txt
                Right (val, _) = T.decimal $ T.drop 5 tile
            in  (val, img)

-- | By treating each side as a 10-bit number (i.e. converting #->1 and .->0) we can generate a unique hash for each side which is 
-- easier to compare
hash :: T.Text -> Int
hash = T.foldl' combine 0
    where combine acc '#' = acc * 2 + 1
          combine acc '.' = acc * 2

-- | Gets all possible side values for a image by flipping/rotating
-- Order of hashes is top, left, down, right, flipped top, flipped left, flipped down, flipped right
getSides :: [T.Text] -> [Int]
getSides img = map hash 
    [head img, T.pack $ reverse $ map T.head img, T.reverse $ last img, T.pack $ map T.last img, 
     T.reverse $ head img, T.pack $ reverse $ map T.last img, last img, T.pack $ map T.head img]

getCorners :: IM.IntMap [T.Text] -> [Int]
getCorners puzzle = 
    let sides = IM.map getSides puzzle
        uniques = map head $ filter (\x -> length x == 1) $ group $ sort $ concat $ IM.elems sides
    -- corner pieces will have 4 unique values (2 edges, each edge produces 2 unique values in getSides (normal + flipped))
    in IM.keys $ IM.filter (\s -> length (s `intersect` uniques) == 4) sides 

part1 :: T.Text -> Int
part1 input = 
    let corners = getCorners $ parseInput input
    in  product corners

-- | Part 2 | --------------------------------------------------------------------------------------
-- Part 2 requires actually assembling the picture, then searching for sea monsters

-- | A type to denote a piece with fixed orientation, with 4 fields representing hashes of each side
data FixedPiece = FixedPiece 
    { _up :: Int
    , _down :: Int
    , _left :: Int
    , _right :: Int
    , _img :: [T.Text]
    } 
    deriving Show

printFixedPiece :: FixedPiece -> IO ()
printFixedPiece fp = do
    let width = 20
    putStrLn ""
    TIO.putStrLn $ T.center width ' ' (T.pack $ show $ _up fp)
    forM_ (zip [1..] $ _img fp) $ \(row, img) -> do 
        let toPrint = if row == 4 then T.pack (show $ _left fp) <> "  " <> img <> "  " <> T.pack (show $ _right fp) else img
        TIO.putStrLn $ T.center width ' ' toPrint
    TIO.putStrLn $ T.center width ' ' (T.pack $ show $ _down fp)

rotate :: Int -> [a] -> [a]
rotate = drop <> take

rotateImgClockwise :: [T.Text] -> [T.Text]
rotateImgClockwise = map (T.reverse . T.pack) . transpose . map T.unpack

getInteriorImg :: [T.Text] -> [T.Text]
getInteriorImg = map (T.init . T.tail) . init . tail

flipImg :: [T.Text] -> [T.Text]
flipImg = map T.reverse

mkFixedPieceFromLeft :: Int -> [Int] -> [T.Text] -> FixedPiece
mkFixedPieceFromLeft hash hashes puzzle = 
    let leftFirstHashes = uncurry (++) $ bimap (rotate 1) (rotate 1) $ splitAt 4 hashes
        Just ix = elemIndex hash leftFirstHashes
        (normal, flipped) = splitAt 4 leftFirstHashes
        isFlipped = ix > 3
        rotates = ix `mod` 4
        [l, d, r, u] = rotate rotates (if isFlipped then flipped else normal)
        image = iterate rotateImgClockwise (if isFlipped then flipImg puzzle else puzzle) !! rotates
    in FixedPiece {
        _left = l,
        _up = u,
        _down = d,
        _right = r,
        _img = getInteriorImg image
       }

mkFixedPieceFromTop :: Int -> [Int] -> [T.Text] -> FixedPiece
mkFixedPieceFromTop hash hashes puzzle = 
    let Just ix = elemIndex hash hashes
        (normal, flipped) = splitAt 4 hashes
        isFlipped = ix > 3
        rotates = ix `mod` 4
        [l, d, r, u] = rotate rotates (if isFlipped then flipped else normal)
        image = iterate rotateImgClockwise (if isFlipped then flipImg puzzle else puzzle) !! rotates
    in FixedPiece {
        _left = l,
        _up = u,
        _down = d,
        _right = r,
        _img = getInteriorImg image
       }


data PuzzleState = PuzzleState 
    { _corners :: IM.IntMap [Int]
    , _edges :: IM.IntMap [Int]
    , _interiors :: IM.IntMap [Int]
    , _picture :: M.Map (Int, Int) FixedPiece
    }

makeLenses ''PuzzleState
makeLenses ''FixedPiece

data PicturePart = Corner | Edge | Interior

getPicPart :: (Int, Int) -> PicturePart
getPicPart (row, col)
    | isTopBot && isLeftRight = Corner
    | isTopBot || isLeftRight = Edge
    | otherwise = Interior
    where isTopBot = row == 0 || row == 11
          isLeftRight = col == 0 || col == 11


formImage :: IM.IntMap [T.Text] -> State PuzzleState (M.Map (Int, Int) FixedPiece)
formImage puzzle = do 
    firstCorner <- use $ corners.ix 1321 -- arbitrary first corner chosen
    corners.at 1321 .= Nothing -- delete this corner
    let topleftCorner = mkFixedPieceFromLeft 892 firstCorner (puzzle IM.! 1321) 
    picture.at (0,0) ?= topleftCorner
    forM_ [0..11] $ \row -> do
        forM_ [0..11] $ \col -> 
            if (row, col) == (0, 0) 
                then return ()
                else do
                currPic <- use picture 
                let sides = catMaybes [currPic^?ix (row, col-1).right, currPic^? ix (row-1, col).down]
                case getPicPart (row, col) of 
                    Corner -> do
                        fitting <- IM.filter (\hs -> all (`elem` hs) sides) <$> use corners
                        when (IM.size fitting > 1) $ trace ("More than 1 possibility for " ++ show sides) return ()
                        when (IM.null fitting) $ trace ("No possibilities for " ++ show (row, col) ++ show sides) return ()
                        let (uid, nextPiece) = IM.findMin fitting
                            next = case currPic M.!? (row, col-1) of
                                    Just fp -> mkFixedPieceFromLeft (fp^.right) nextPiece (puzzle IM.! uid)
                                    Nothing -> mkFixedPieceFromTop ((currPic M.! (row-1, col))^.down) nextPiece (puzzle IM.! uid)
                        corners.at uid .= Nothing -- remove
                        picture.at (row, col) ?= next
                    Edge -> do
                        fitting <- IM.filter (\hs -> all (`elem` hs) sides) <$> use edges
                        when (IM.size fitting > 1) $ trace ("More than 1 possibility for " ++ show sides) return ()
                        when (IM.null fitting) $ trace ("No possibilities for " ++ show (row, col) ++ show sides) return ()
                        let (uid, nextPiece) = IM.findMin fitting
                            next = case currPic M.!? (row, col-1) of
                                    Just fp -> mkFixedPieceFromLeft (fp^.right) nextPiece (puzzle IM.! uid)
                                    Nothing -> mkFixedPieceFromTop ((currPic M.! (row-1, col))^.down) nextPiece (puzzle IM.! uid)
                        edges.at uid .= Nothing -- remove
                        picture.at (row, col) ?= next
                    Interior -> do
                        fitting <- IM.filter (\hs -> all (`elem` hs) sides) <$> use interiors
                        when (IM.size fitting > 1) $ trace ("More than 1 possibility for " ++ show sides) return ()
                        when (IM.null fitting) $ trace ("No possibilities for " ++ show (row, col) ++ show sides) return ()
                        let (uid, nextPiece) = IM.findMin fitting
                            next = case currPic M.!? (row, col-1) of
                                    Just fp -> mkFixedPieceFromLeft (fp^.right) nextPiece (puzzle IM.! uid)
                                    Nothing -> mkFixedPieceFromTop ((currPic M.! (row-1, col))^.down) nextPiece (puzzle IM.! uid)
                        interiors.at uid .= Nothing -- remove
                        picture.at (row, col) ?= next
    use picture

runFormImage :: IM.IntMap [T.Text] -> M.Map (Int, Int) FixedPiece
runFormImage puzzle = 
    let sides = IM.map getSides puzzle
        uniques = map head $ filter (\x -> length x == 1) $ group $ sort $ concat $ IM.elems sides
        corners = IM.filter (\s -> length (s `intersect` uniques) == 4) sides
        edges = IM.filter (\s -> length (s `intersect` uniques) == 2) sides
        interiors = IM.filter (\s -> null (s `intersect` uniques)) sides
    in evalState (formImage puzzle) (PuzzleState corners edges interiors M.empty)

piecePictureTogether :: M.Map (Int, Int) FixedPiece -> [T.Text]
piecePictureTogether picture = 
    flip map [0..11] (\row ->
        flip map [0..11] (\col -> 
            (picture M.! (row, col)) ^. img
        ) & 
        foldl1' (zipWith (<>))
    ) &
    concat

main :: IO ()
main = TIO.readFile "input/20.txt" >>= print . part1