module Day17 where

import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (group, foldl', sort, iterate')

class Ord a => Vec a where
    getNeighbours :: a -> [a]
    from2D :: Int -> Int -> a

newtype Vec3D = Vec3D (Int, Int, Int) deriving (Eq, Ord)

instance Vec Vec3D where
    getNeighbours = get3DNeighbours
    from2D x y = Vec3D (x, y, 0)

get3DNeighbours :: Vec3D -> [Vec3D]
get3DNeighbours (Vec3D (x,y,z)) = do 
    xs <- [x-1, x, x+1]
    ys <- [y-1, y, y+1]
    zs <- [z-1, z, z+1]
    guard $ not (xs == x && ys == y && zs == z)
    return $ Vec3D (xs, ys, zs)

parseInitialMap :: Vec a => String -> S.Set a
parseInitialMap puzzle = 
    foldl' (\actives (row, line) ->
        foldl' (\actives' (col, cube) -> 
            if cube == '#' 
                then S.insert (from2D col row) actives'
                else actives'
        ) actives $ zip [0..] line
    ) S.empty $ zip [0..] (lines puzzle)
    
singleCycle :: Vec a => S.Set a -> S.Set a
singleCycle actives = 
    let newActivated = S.fromList
                    $ map head
                    $ filter ((== 3) . length) 
                    $ group 
                    $ sort 
                    $ filter (\coord -> not $ S.member coord actives) 
                    $ concatMap getNeighbours $ S.elems actives
        stillActive = S.filter (\coord -> 
                        let activeNeighbours = length $ filter (`S.member` actives) $ getNeighbours coord
                        in activeNeighbours == 2 || activeNeighbours == 3) actives
    in newActivated `S.union` stillActive

solution :: String -> Int
solution input = let initial = parseInitialMap input :: S.Set Vec4D -- Vec3D
                     finalState = iterate' singleCycle initial !! 6
                 in S.size finalState

-- | Part 2 | -------------------------------------------------------
-- we generify our Vec3D into a Vec class and make parseInitialMap and singleCycle generic, so extending 
-- to 4D is as simple as writing instance for Vec specific to 4D. Then no other implementation is necessary :)

newtype Vec4D = Vec4D (Int, Int, Int, Int) deriving (Eq, Ord)

instance Vec Vec4D where
    getNeighbours (Vec4D (x,y,z,w)) = do
        xs <- [x-1, x, x+1]
        ys <- [y-1, y, y+1]
        zs <- [z-1, z, z+1]
        ws <- [w-1, w, w+1]
        guard $ not (xs == x && ys == y && zs == z && ws == w)
        return $ Vec4D (xs, ys, zs, ws)
    from2D x y = Vec4D (x, y, 0, 0)

main :: IO ()
main = readFile "input/17.txt" >>= print . solution