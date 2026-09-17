module Example5.A (A, fromCInt) where

import Foreign.C.Types

import HsBindgen.Runtime.HasFFIType

data A = Neg Word | Zero | Pos Word
  deriving stock Show

fromCInt :: CInt -> A
fromCInt x
  | x < 0 = Neg (fromIntegral (abs x))
  | x == 0 = Zero
  | otherwise -- x > 0
  =  Pos (fromIntegral x)

toCInt :: A -> CInt
toCInt = \case
    Neg x -> - fromIntegral x
    Zero -> 0
    Pos x -> fromIntegral x

instance HasFFIType A where
  type FFIType A = CInt
  toFFIType = toCInt
  fromFFIType = fromCInt
