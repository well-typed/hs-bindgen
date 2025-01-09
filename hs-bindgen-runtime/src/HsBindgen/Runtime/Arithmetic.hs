{-# LANGUAGE DerivingVia #-}

module HsBindgen.Runtime.Arithmetic
  ( Div(..) ) where

import Data.Kind
import Foreign.C.Types
import Data.Int
import Data.Word

--------------------------------------------------------------------------------

infixl 7 /
class Div a where
  (/) :: a -> a -> a

type Via :: ( Type -> Constraint ) -> Type -> Type
newtype Via c a = Via a
instance Integral a => Div (Via Integral a) where
  Via a / Via b = Via $ a `div` b
  {-# INLINEABLE (/) #-}
instance Fractional a => Div (Via Fractional a) where
  Via a / Via b = Via $ a Prelude./ b
  {-# INLINEABLE (/) #-}

deriving via Via Integral Int8   instance Div Int8
deriving via Via Integral Int16  instance Div Int16
deriving via Via Integral Int32  instance Div Int32
deriving via Via Integral Int64  instance Div Int64
deriving via Via Integral Word8  instance Div Word8
deriving via Via Integral Word16 instance Div Word16
deriving via Via Integral Word32 instance Div Word32
deriving via Via Integral Word64 instance Div Word64

deriving via Via Fractional Float  instance Div Float
deriving via Via Fractional Double instance Div Double

deriving newtype instance Div CChar
deriving newtype instance Div CSChar
deriving newtype instance Div CUChar
deriving newtype instance Div CInt
deriving newtype instance Div CUInt
deriving newtype instance Div CShort
deriving newtype instance Div CUShort
deriving newtype instance Div CLong
deriving newtype instance Div CULong
deriving newtype instance Div CLLong
deriving newtype instance Div CULLong

deriving newtype instance Div CFloat
deriving newtype instance Div CDouble
