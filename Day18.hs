module Day18 where

import Data.Char ( digitToInt, isDigit )

data Math = Lit Int | Add Int | Mult Int | Blank deriving (Eq, Show)

interpret :: Math -> Int -> Math
interpret (Add x) y = Lit (x+y)
interpret (Mult x) y = Lit (x*y)
interpret _ y = Lit y 

-- | A really shitty interpreter that only can evaluate well-formed expressions and has no error handling
evalMath :: Bool -> String -> Math -> (Math, String)
evalMath inParen [] val = if inParen then error "No closing parenthesis" else (val, [])
evalMath inParen (x:xs) val 
    | x == ' ' = evalMath inParen xs val
    | isDigit x = evalMath inParen xs (interpret val (digitToInt x))
    | x == '+' = let Lit num = val in evalMath inParen xs (Add num)
    | x == '*' = let Lit num = val in evalMath inParen xs (Mult num)
    | x == '(' = let (Lit inner, rest) = evalMath True xs Blank
                 in evalMath inParen rest (interpret val inner)
    | x == ')' = if inParen then (val, xs) else error "mismatched closing brace"

part1 :: String -> Int
part1 input = let getAns str = let (Lit ans, []) = evalMath False str Blank in ans
                  vals = map getAns $ lines input
              in sum vals

main :: IO ()
main = readFile "input/18.txt" >>= print . part2

-- | Part 2 | --------------------------------------------------------------------

data Op = Subexpr | Times | Plus deriving (Eq, Ord, Show)
data Stack = Val Int | Op Op deriving Show

op :: Char -> Op
op '+' = Plus
op '*' = Times

evalOp :: Op -> (Int -> Int -> Int)
evalOp Plus = (+)
evalOp Times = (*)

shuntingYard :: String -> [Stack]
shuntingYard = go [] [] 
    where go stack output "" = reverse output ++ map Op stack
          go stack output (x:xs)
            | x == ' ' = go stack output xs 
            | isDigit x = go stack (Val (digitToInt x) : output) xs
            | x == '(' = go (Subexpr:stack) output xs
            | x == ')' = let (stack', output') = popSubexpr stack output in go stack' output' xs
            | otherwise = let (stack', output') = pushOperator (op x) stack output in go stack' output' xs
          popSubexpr (Subexpr:stack) output = (stack, output)
          popSubexpr (x:xs) output = popSubexpr xs (Op x:output)
          pushOperator op [] output = ([op], output)
          pushOperator op (x:xs) output = if x >= op then pushOperator op xs (Op x:output) else (op:x:xs, output)


evalRPN :: [Stack] -> Int
evalRPN = head . foldl eval []
    where eval acc (Val x) = x:acc
          eval (x1:x2:acc) (Op o) = evalOp o x1 x2 : acc

evalMath2 :: String -> Int
evalMath2 = evalRPN . shuntingYard

part2 :: String -> Int
part2 = sum . map evalMath2 . lines