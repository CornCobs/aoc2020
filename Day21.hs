{-# LANGUAGE OverloadedStrings #-}

module Day21 where

import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable 
import Data.Bifunctor ( Bifunctor(bimap) )

type Food = T.Text
type Allergen = T.Text

parseAllergens :: T.Text -> (S.Set Food, [Allergen])
parseAllergens = bimap (S.fromList . T.words) (T.splitOn ", " . T.init) . toTuple .  T.splitOn " (contains "
    where toTuple [x,y] = (x, y)

part1 :: T.Text -> Int
part1 input = 
    let allergenList = map parseAllergens $ T.lines input
        ingredients = S.unions $ map fst allergenList
        updatePossibilities possibles (ingredients, allergens) = 
            foldl' (\possibles' allergen -> M.insertWith S.intersection allergen ingredients possibles') possibles allergens
        possibleIngredients = foldl' updatePossibilities M.empty allergenList
        susIngredients = fold possibleIngredients
        clearIngredients = ingredients S.\\ susIngredients
    in sum $ map (S.size . S.intersection clearIngredients . fst) allergenList

part2 :: T.Text -> T.Text
part2 input = 
    let allergenList = map parseAllergens $ T.lines input
        ingredients = S.unions $ map fst allergenList
        updatePossibilities possibles (ingredients, allergens) = 
            foldl' (\possibles' allergen -> M.insertWith S.intersection allergen ingredients possibles') possibles allergens
        possibleIngredients = foldl' updatePossibilities M.empty allergenList
    in T.intercalate "," $ map (S.findMax.snd) $ M.toAscList $ identifyAllergens possibleIngredients

identifyAllergens :: M.Map Allergen (S.Set Food) -> M.Map Allergen (S.Set Food)
identifyAllergens possibles = go (M.empty, possibles)
    where go (singles, ambiguous)
              | M.null ambiguous = singles
              | otherwise = 
                    let (singles', ambiguous') = M.partition ((== 1).S.size) ambiguous
                        ambiguous'' = M.foldr (\ingredient amb -> M.map (S.\\ ingredient) amb) ambiguous' singles'
                    in go (M.union singles singles', ambiguous'')