import Data.List (map, permutations)

-- Rule 1: All numbers must be positive naturals
-- Rule 2: All source numbers can be used once only

-- Arithmetic operators
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add a b = a + b
apply Sub a b = a - b
apply Mul a b = a * b
apply Div a b = a `div` b

valid :: Op -> Int -> Int -> Bool
valid Sub a b = a > b
valid Div a b = a `mod` b == 0
valid _ _ _ = True

-- Numeric expressions

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ e1 e2) = values e1 ++ values e2

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o e1 e2) = [apply o x y | x <- eval e1, y <- eval e2, valid o x y]

-- Combinatorial functions

comb :: [a] -> [[a]]
comb [] = [[]]
comb (x : xs) = yss ++ map (x :) yss where yss = comb xs

choices :: [a] -> [[a]]
choices xs = [p | c <- comb xs, p <- permutations c]

-- Formalising the problem

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = values e `elem` choices ns && eval e == [n]
