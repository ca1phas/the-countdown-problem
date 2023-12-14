import Data.List (map, permutations)

-- Arithmetic operators
data Op = Add | Sub | Mul | Div

ops :: [Op]
ops = [Add, Sub, Mul, Div]

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
eval (App o e1 e2) = [apply o a b | a <- eval e1, b <- eval e2, valid o a b]

-- Combinatorial functions
comb :: [a] -> [[a]]
comb [] = [[]]
comb (x : xs) = yss ++ map (x :) yss where yss = comb xs

choices :: [a] -> [[a]]
choices xs = [p | c <- comb xs, p <- permutations c]

-- Formalising the problem
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = values e `elem` choices ns && eval e == [n]

-- Brute Force Search
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : as, bs) | (as, bs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [x] = [Val x]
exprs xs =
  [ e
    | (as, bs) <- split xs,
      a <- exprs as,
      b <- exprs bs,
      e <- combine a b
  ]

combine :: Expr -> Expr -> [Expr]
combine a b = [App o a b | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | c <- choices ns, e <- exprs c, eval e == [n]]

-- Combining generation and evaluation
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ r
    | (as, bs) <- split ns,
      ra <- results as,
      rb <- results bs,
      r <- combine' ra rb
  ]

combine' :: Result -> Result -> [Result]
combine' (e1, a) (e2, b) =
  [ (App o e1 e2, apply o a b)
    | o <- ops,
      valid o a b
  ]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | c <- choices ns, (e, v) <- results c, v == n]

-- Exploiting algebraic properties
valid' :: Op -> Int -> Int -> Bool
valid' Sub a b = a > b
valid' Add a b = a <= b
valid' Div a b = b /= 1 && a `mod` b == 0
valid' Mul a b = a <= b && a /= 1 && b /= 1

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns =
  [ r
    | (as, bs) <- split ns,
      ra <- results' as,
      rb <- results' bs,
      r <- combine'' ra rb
  ]

combine'' :: Result -> Result -> [Result]
combine'' (e1, a) (e2, b) =
  [ (App o e1 e2, apply o a b)
    | o <- ops,
      valid' o a b
  ]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | c <- choices ns, (e, v) <- results' c, v == n]
