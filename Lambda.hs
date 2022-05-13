import Data.List

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
  
occur :: LT -> LT -> Bool
occur p (A x) = p == (A x)
occur p (App m n) = p == (App m n) || (occur p m) || (occur p n)
occur p (Abs x m) = p == (Abs x m) || (p == A x) || (occur p m)

-- Get all variables in term M.
vars :: LT -> [Var]
vars m = (nub . vars') m

vars' :: LT -> [Var]
vars' (A x) = [x]
vars' (App m n) = (vars' m) ++ (vars' n)
vars' (Abs x m) = x : (vars' m)

-- free variables in M
fv :: LT -> [Var]
fv m = nub (fv' m [])

fv' :: LT -> [Var] -> [Var]
fv' (A x) zs
  | elem x zs = []
  | otherwise = [x]
fv' (App m n) zs = (fv' m zs) ++ (fv' n zs)
fv' (Abs x m) zs = (fv' m (x:zs)) 






vs = map Var [0..]
ws = map A vs

m0 = App (ws !! 0) (ws !! 5)
m1 = Abs (vs !! 0) m0
m2 = App m1 m0

n0 = ws !! 5
n1 = ws !! 3

-- u = v0, v = v1, w = v2, x = v3, y = v4, z = v5
-- xv(λyz.yv)w = (((v3v1)(λv4.(λv5.(v4v1))))v2)
-- (λy.yx(λx.y(λy.z)x))vw = (((λv4.((v4v3)(λv3.((v4(λv4.v5))v3) )))v1)v2)

m3 = App (ws !! 3) (ws !! 1)
m4 = App (ws !! 4) (ws !! 1)
m5 = Abs (vs !! 5) m4
m6 = Abs (vs !! 4) m5
m7 = App m3 m6
m8 = App m7 (ws !! 2)

m9 = Abs (vs !! 4) (ws !! 5)
m10 = App (ws !! 4) m9
m11 = App m10 (ws !! 3)
m12 = Abs (vs !! 3) m11
m13 = App (ws !! 4) (ws !! 3)
m14 = App m13 m12
m15 = Abs (vs !! 4) m14
m16 = App m15 (ws !! 1)
m17 = App m16 (ws !! 2)
