{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified Foreign.C.String as FC
import Prelude (Int)

c1 :: FC.CInt
c1 = (97 :: FC.CInt)

c2 :: FC.CInt
c2 = (34 :: FC.CInt)

c3 :: FC.CInt
c3 = (9 :: FC.CInt)

c4 :: FC.CInt
c4 = (0 :: FC.CInt)

c5 :: FC.CInt
c5 = (39 :: FC.CInt)

c6 :: FC.CInt
c6 = (63 :: FC.CInt)

c7 :: FC.CInt
c7 = (83 :: FC.CInt)

c8 :: FC.CInt
c8 = (83 :: FC.CInt)

d :: FC.CInt
d = (511 :: FC.CInt)

j1 :: FC.CInt
j1 = (14909826 :: FC.CInt)

j2 :: FC.CInt
j2 = (14909826 :: FC.CInt)

j3 :: FC.CInt
j3 = (14909826 :: FC.CInt)

s1 :: ((,,) (F.Ptr FC.CChar)) Int
s1 = ((F.Ptr "a"#, 1) :: FC.CStringLen)

s2 :: ((,,) (F.Ptr FC.CChar)) Int
s2 = ((F.Ptr "\'"#, 1) :: FC.CStringLen)

s3 :: ((,,) (F.Ptr FC.CChar)) Int
s3 = ((F.Ptr "\t"#, 1) :: FC.CStringLen)

s4 :: ((,,) (F.Ptr FC.CChar)) Int
s4 = ((F.Ptr "\0"#, 1) :: FC.CStringLen)

s5 :: ((,,) (F.Ptr FC.CChar)) Int
s5 = ((F.Ptr "\'"#, 1) :: FC.CStringLen)

s6 :: ((,,) (F.Ptr FC.CChar)) Int
s6 = ((F.Ptr "?"#, 1) :: FC.CStringLen)

s7 :: ((,,) (F.Ptr FC.CChar)) Int
s7 = ((F.Ptr "S"#, 1) :: FC.CStringLen)

s8 :: ((,,) (F.Ptr FC.CChar)) Int
s8 = ((F.Ptr "S"#, 1) :: FC.CStringLen)

t1 :: ((,,) (F.Ptr FC.CChar)) Int
t1 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

t2 :: ((,,) (F.Ptr FC.CChar)) Int
t2 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

t3 :: ((,,) (F.Ptr FC.CChar)) Int
t3 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

u :: ((,,) (F.Ptr FC.CChar)) Int
u = ((F.Ptr "\x1\xFF\x1\xFF\x1\xFF\x1\xFF"#, 8) :: FC.CStringLen)

v :: ((,,) (F.Ptr FC.CChar)) Int
v = ((F.Ptr "\x1\x2\x3\x4\x5\x6"#, 6) :: FC.CStringLen)

w1 :: ((,,) (F.Ptr FC.CChar)) Int
w1 = ((F.Ptr "hij\0"#, 4) :: FC.CStringLen)

w2 :: ((,,) (F.Ptr FC.CChar)) Int
w2 = ((F.Ptr "abc\0def\0g"#, 9) :: FC.CStringLen)
