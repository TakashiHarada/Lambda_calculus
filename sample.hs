-- Put sample.hs and Lambda.hs in a same direcoty.
import Lambda

f = vs !! 5
m = vs !! 12
n = vs !! 13
u = vs !! 20
v = vs !! 21
w = vs !! 22
x = vs !! 23
y = vs !! 24
z = vs !! 25

-- Question 3 in Homework 8
xx = App (A x) (A x)
lx_xx = Abs x xx
ly_y = Abs y (A y)
ly_y'w = App ly_y (A w)
lz_lx_xx'ly_y'w = Abs z (App lx_xx ly_y'w)
lz_lx_xx'ly_y'w'v = App lz_lx_xx'ly_y'w (A v)

lu_lv_u = Abs u (Abs v (A u))
lu_lv_u'xyx = App (App (App lu_lv_u (A x)) (A y)) (A x)
lx_lu_lv_u'xyx = Abs x lu_lv_u'xyx
lx_lu_lv_u'xyx'w = App lx_lu_lv_u'xyx (A w)

-- λ> lmReduce lz_lx_xx'ly_y'w'v
-- λ> lmReduce lx_lu_lv_u'xyx'w

cI = Abs x (A x)
cS = Abs x (Abs y (Abs z (App xz yz)))
  where
    yz = App (A y) (A z)
    xz = App (A x) (A z)
cK = Abs x (Abs y (A x))
skk = App (App cS cK) cK
-- λ> lmReduce skk


-- Fixed point combinator Y
cY = App (Abs u (Abs x x'uux)) (Abs u (Abs x x'uux))
  where
    uux = App (App (A u) (A u)) (A x)
    x'uux = App (A x) uux

-- Number and operations
suc = Abs u (Abs x (Abs y x'uxy))
  where
    uxy = App (App (A u) (A x)) (A y)
    x'uxy = App (A x) uxy

pred' = Abs x (App (App x'uv_v'usuc kzero) cI)
  where
    usuc = App (A u) suc
    v'usuc = App (A v) usuc
    uv_v'usuc = Abs u (Abs v v'usuc)
    x'uv_v'usuc = App (A x) (Abs u (Abs v v'usuc))
    kzero = App cK zero

zero = Abs f (Abs x (A x))
one = App suc zero
two = App suc one
-- lmReduce two

cnum :: Int -> LT
cnum 0 = zero
cnum n = App suc (cnum (n-1))
-- lmReduce (cnum 5)

-- Pairing combinator D
cD = Abs x (Abs y (Abs z zkyx))
  where
    ky = App cK (A y)
    zky = App (A z) ky
    zkyx = App zky (A x)

mul = Abs m (Abs n (Abs f (Abs x nmfx)))
  where
    mf = App (A m) (A f)
    nmf = App (A n) mf
    nmfx = App nmf (A x)
-- lmReduce (App (App mul (cnum 5)) (cnum 2))

fac = App cY body
  where
    pm = App pred' (A m)
    fpm = App (A f) pm
    re = App (App mul (A m)) fpm
    d1rem = App (App (App cD one) re) (A m)
    body = Abs f (Abs m d1rem)
-- lmReduce $ App fac (cnum 4)
