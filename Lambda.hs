module Lambda where

import Data.List

-- Lambda Term

data Var' = Var0 | Var1 | Var2 | Var3 | Var4 

data Var = Var Int deriving Eq

instance Show Var where
  show (Var x) = "v" ++ show x

vs :: [Var]
vs = map Var [0..]

data LT =
  A Var |
  App LT LT |
  Abs Var LT deriving Eq

ws :: [LT]
ws = map A vs

instance Show LT where
  show (A x) = show x
  show (App m n) = "(" ++ show m ++ show n ++ ")"
  show (Abs x m) = "(λ" ++ show x ++ "." ++ show m ++ ")"
  
occur :: LT -> LT -> Bool
occur p (A x) = p == (A x)
occur p (App m n) = p == (App m n) || (occur p m) || (occur p n)
occur p (Abs x m) = p == (Abs x m) || (p == A x) || (occur p m)

-- Get all variables in term M.
vars :: LT -> [Var]
vars m = (nub . vars') m
  where
    vars' :: LT -> [Var]
    vars' (A x) = [x]
    vars' (App m n) = (vars' m) ++ (vars' n)
    vars' (Abs x m) = x : (vars' m)

fv :: LT -> [Var]
fv t = (nub . fv') t

fv' :: LT -> [Var]
fv' (A x) = [x]
fv' (App m n) = (fv m) ++ (fv n)
fv' (Abs x m) = del x (fv m)

del :: Eq a => a -> [a] -> [a]
del _ [] = []
del x (a:as)
  | x == a = del x as
  | otherwise = a : (del x as)


-- first variable not in the given term
fstvnin :: LT -> Var
fstvnin t = head [x | x <- map Var [0..], (not (elem x (fv t)))]

-- substitution
-- [N/x]M
sub :: LT -> LT -> Var -> LT 
sub (A y) n x
  | y /= x = (A y)
  | otherwise = n
sub (App p q) n x = App (sub p n x) (sub q n x)
sub (Abs y p) n x
  | y == x = Abs y p
  | y /= x && (not (elem x (fv p))) = Abs y p
  | y /= x && (elem x (fv p)) && (not (elem y (fv n))) = Abs y (sub p n x)
  | otherwise = Abs z (sub (sub p (A z) y) n x)
  where z = fstvnin (App n p)

getBetaRedex :: LT -> [LT]
getBetaRedex (A v) = []
getBetaRedex (App (A v) n) = getBetaRedex n
getBetaRedex (App (Abs x p) n) = (App (Abs x p) n) : (getBetaRedex p) ++ (getBetaRedex n)
getBetaRedex (App m n) = (getBetaRedex m) ++ getBetaRedex n
getBetaRedex (Abs x p) = getBetaRedex p

-- alphaConv :: LT -> Var -> LT
-- alphaConv (A v) _ = A v
-- alphaConv (App m n) x = App (alphaConv m x) (alphaConv n x)
-- alphaConv (Abs x m) y
--   | not (elem y (fv m)) = Abs y (sub m (A y) x)
--   | otherwise = Abs x m

-- beta contracts the left most beta redex
lmbContruct :: LT -> LT
lmbContruct (A x) = A x
lmbContruct (Abs x m) = Abs x (lmbContruct m)
lmbContruct (App (A x) m) = (App (A x) (lmbContruct m))
lmbContruct (App (Abs x m) n) = sub m n x
lmbContruct (App m n)
  | [] /= getBetaRedex m = App (lmbContruct m) n
  | otherwise = App m (lmbContruct n)

-- WARNING!! The following function may cause an infinite loop.
lmbReduce :: LT -> LT
lmbReduce m
  | [] /= getBetaRedex m = lmbReduce (lmbContruct m)
  | otherwise = m
