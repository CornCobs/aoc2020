{-# LANGUAGE RecordWildCards #-}

module Day8 where
 
import Data.Char ( isSpace )
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict ((!))
import qualified Data.Set as S
import Data.Maybe (mapMaybe)

data Op = Nop | Jmp | Acc deriving (Eq, Show)

data RunState = RunState { instructions :: M.IntMap (Op, Int), accumulator :: Int, position :: Int }

readOp :: String -> Op
readOp "nop" = Nop
readOp "jmp" = Jmp
readOp "acc" = Acc

instruction :: String -> (Op, Int)
instruction instr = let (op, ' ':sign:valStr) = break isSpace instr
                        val = if sign == '-' then - (read valStr) else read valStr
                    in  (readOp op, val)

parseInstr :: String -> M.IntMap (Op, Int)
parseInstr = M.fromDistinctAscList . zip [0..] . map instruction . lines

runOp :: (Op, Int) -> RunState -> RunState
runOp (op, val) rs@RunState {..} = case op of 
    Nop -> rs { position = position + 1}
    Jmp -> rs { position = position + val}
    Acc -> rs { position = position + 1, accumulator = accumulator + val}

runInfLoop :: RunState -> S.Set Int -> Int
runInfLoop rs@RunState {..} ran
    | S.member position ran = accumulator
    | otherwise = 
        let ran' = S.insert position ran
            rs' = runOp (instructions ! position) rs
        in  runInfLoop rs' ran'

part1 :: String -> Int
part1 txt = 
    let instr = parseInstr txt
        init = RunState 
            { instructions = instr
            , accumulator = 0
            , position = 0
            }
    in runInfLoop init S.empty

runTerminatingProg :: RunState -> S.Set Int -> Maybe Int
runTerminatingProg rs@RunState {..} ran
    | S.member position ran = Nothing
    | position == M.size instructions = Just accumulator
    | otherwise =
        let ran' = S.insert position ran
            rs' = runOp (instructions ! position) rs
        in runTerminatingProg rs' ran'

part2 :: String -> Int
part2 txt = 
    let instr = parseInstr txt
        switch Jmp = Just Nop
        switch Nop = Just Jmp
        switch Acc = Nothing
    in head $ flip mapMaybe [0..M.size instr - 1] $ \key ->
        let (op, val) = instr ! key
        in switch op >>= \op' -> 
            runTerminatingProg (RunState (M.insert key (op', val) instr) 0 0) S.empty


main :: IO ()
main = readFile "input/8.txt" >>= print . part2