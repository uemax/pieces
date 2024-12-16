-- This program is written by Professor Graham Hutton from the University of Nottingham,
-- and the original copyright is retained and respceted.

-- | Operators
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- | Apply an operator
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- | Validate operations
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- | Expressions
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e       = "(" ++ show e ++ ")"

-- | Return the overall value of an expression
eval :: Expr -> [Int]
-- Either succeeds and returns a singleton list,
-- or fails and return the empty list
eval (Val n)     = [n | n > 0]
-- l & r can be a Val or an App
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- | Formalising the problem

-- | Return a list of all possible ways of choosing zere or more elements from a list
--
-- > choices [1,2] -> [[], [1], [2], [1,2], [2,1]]
choices = permutations

permutations :: [a] -> [[a]]
permutations xs0 = xs0 : perms xs0 []
  where
    perms [] _ = []
    perms (t : ts) is = foldr interleave (perms ts (t : is)) (permutations is)
      where
        interleave xs r = let (_, zs) = interleave' id xs r in zs
        interleave' _ [] r = (ts, r)
        interleave' f (y : ys) r =
          let (us, zs) = interleave' (f . (y :)) ys r
           in (y : us, f (t : y : us) : zs)

-- | Return a list of all the values in an expression
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- | Decide if an expression is a solution for a given list of source numbers and
-- | a target number
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns)
    && eval e == [n]

-- | Brute force solution

-- | Return a list of all possible ways of splitting a list into two non-empty list
split :: [a] -> [([a], [a])]
split []       = []
split [_]      = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

-- | Return a list of all possible expressions whose values are precisely a given
-- | list of numbers
--
-- The key function in this lecture
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns
              , l <- exprs ls
              , r <- exprs rs
              , e <- combine l r]

-- | Combine two expressions using each operator
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

-- | Return a list of all possible expressions that solve an instance of the
-- | countdown probelm
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns -- find all number combinations
                    , e <- exprs ns'    -- find all expression combinations
                    , eval e == [n]]    -- test expression combination

main :: IO ()
main = print (solutions [1, 3, 7, 10, 25, 50] 765)
