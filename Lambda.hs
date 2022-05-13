-- Lambda Term

data Var' = Var0 | Var1 | Var2 | Var3 | Var4 

data Var = Var Int deriving Eq

instance Show Var where
  show (Var x) = "v" ++ show x

data LT =
  A Var |
  App LT LT |
  Abs Var LT deriving Eq

instance Show LT where
  show (A x) = show x
  show (App m n) = "(" ++ show m ++ show n ++ ")"
  show (Abs x m) = "(λ" ++ show x ++ "." ++ show m ++ ")"
  
vs = map Var [0..]
ws = map A vs

m0 = App (ws !! 0) (ws !! 5)
m1 = Abs (vs !! 0) m0
m2 = App m1 m0

n0 = ws !! 5
n1 = ws !! 3

occur :: LT -> LT -> Bool
occur p (A x) = p == (A x)
occur p (App m n) = p == (App m n) || (occur p m) || (occur p n)
occur p (Abs x m) = p == (Abs x m) || (p == A x) || (occur p m)
