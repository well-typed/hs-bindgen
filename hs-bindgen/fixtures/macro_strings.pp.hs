{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified C.Char
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified Foreign.C.String as FC
import Prelude (Int)

c1 :: C.Char.CharValue
c1 = C.Char.charValueFromAddr "a"# 1 (Just 'a')

c2 :: C.Char.CharValue
c2 = C.Char.charValueFromAddr "\""# 1 (Just '"')

c3 :: C.Char.CharValue
c3 = C.Char.charValueFromAddr "\t"# 1 (Just '\t')

c4 :: C.Char.CharValue
c4 = C.Char.charValueFromAddr "\0"# 1 (Just '\NUL')

c5 :: C.Char.CharValue
c5 = C.Char.charValueFromAddr "\'"# 1 (Just '\'')

c6 :: C.Char.CharValue
c6 = C.Char.charValueFromAddr "?"# 1 (Just '?')

c7 :: C.Char.CharValue
c7 = C.Char.charValueFromAddr "S"# 1 Nothing

c8 :: C.Char.CharValue
c8 = C.Char.charValueFromAddr "S"# 1 Nothing

d :: C.Char.CharValue
d = C.Char.charValueFromAddr "\x1\xFF"# 2 Nothing

j1 :: C.Char.CharValue
j1 = C.Char.charValueFromAddr "\xE3\x81\x82"# 3 (Just '\12354')

j2 :: C.Char.CharValue
j2 = C.Char.charValueFromAddr "\xE3\x81\x82"# 3 (Just '\12354')

j3 :: C.Char.CharValue
j3 = C.Char.charValueFromAddr "\xE3\x81\x82"# 3 Nothing

s1 :: ((,) (F.Ptr FC.CChar)) Int
s1 = ((F.Ptr "a"#, 1) :: FC.CStringLen)

s2 :: ((,) (F.Ptr FC.CChar)) Int
s2 = ((F.Ptr "\'"#, 1) :: FC.CStringLen)

s3 :: ((,) (F.Ptr FC.CChar)) Int
s3 = ((F.Ptr "\t"#, 1) :: FC.CStringLen)

s4 :: ((,) (F.Ptr FC.CChar)) Int
s4 = ((F.Ptr "\0"#, 1) :: FC.CStringLen)

s5 :: ((,) (F.Ptr FC.CChar)) Int
s5 = ((F.Ptr "\'"#, 1) :: FC.CStringLen)

s6 :: ((,) (F.Ptr FC.CChar)) Int
s6 = ((F.Ptr "?"#, 1) :: FC.CStringLen)

s7 :: ((,) (F.Ptr FC.CChar)) Int
s7 = ((F.Ptr "S"#, 1) :: FC.CStringLen)

s8 :: ((,) (F.Ptr FC.CChar)) Int
s8 = ((F.Ptr "S"#, 1) :: FC.CStringLen)

t1 :: ((,) (F.Ptr FC.CChar)) Int
t1 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

t2 :: ((,) (F.Ptr FC.CChar)) Int
t2 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

t3 :: ((,) (F.Ptr FC.CChar)) Int
t3 = ((F.Ptr "\xE3\x81\x82"#, 3) :: FC.CStringLen)

u :: ((,) (F.Ptr FC.CChar)) Int
u = ((F.Ptr "\x1\xFF\x1\xFF\x1\xFF\x1\xFF"#, 8) :: FC.CStringLen)

v :: ((,) (F.Ptr FC.CChar)) Int
v = ((F.Ptr "\x1\x2\x3\x4\x5\x6"#, 6) :: FC.CStringLen)

w1 :: ((,) (F.Ptr FC.CChar)) Int
w1 = ((F.Ptr "hij\0"#, 4) :: FC.CStringLen)

w2 :: ((,) (F.Ptr FC.CChar)) Int
w2 = ((F.Ptr "abc\0def\0g"#, 9) :: FC.CStringLen)
