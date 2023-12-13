import Data.List (permutations)

-- Rule 1: All numbers must be positive naturals
-- Rule 2: All source numbers can be used once only

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
valid Div x y = x `mod` y == 0
valid _ _ _ = True

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

comb :: [Int] -> Int -> [[Int]]
comb [] _ = []
comb _ 0 = [[]]
comb xs 1 = [[x] | x <- xs]
comb (x : xs) r = [x : xs' | xs' <- comb xs (r - 1)] ++ comb xs r

combTillLength :: [Int] -> Int -> [[Int]]
combTillLength [] _ = []
combTillLength _ 0 = [[]]
combTillLength xs l = combTillLength xs (l - 1) ++ comb xs l

choices :: [Int] -> [[Int]]
choices [] = [[]]
choices xs = [p | c <- combTillLength xs (length xs), p <- permutations c]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = values e `elem` choices ns && eval e == [n]