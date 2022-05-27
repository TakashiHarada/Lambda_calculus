-- Put sample.hs and Lambda.hs in a same direcoty.
import Lambda

f = vs !! 5
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

yz = App (A y) (A z)
xz = App (A x) (A z)
cs = Abs x (Abs y (Abs z (App xz yz)))
ck = Abs x (Abs y (A x))
skk = App (App cs ck) ck

-- λ> lmReduce skk

uux = App (App (A u) (A u)) (A x)
x'uux = App (A x) uux

-- Fixed point combinator Y
cy = App (Abs u (Abs x x'uux)) (Abs u (Abs x x'uux))
