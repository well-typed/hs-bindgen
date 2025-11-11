{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__defined at:__ @fun_attributes.h:7:9@

__exported by:__ @fun_attributes.h@
-}
data FILE = FILE
  {}
  deriving stock (Eq, Show)

instance F.Storable FILE where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure FILE

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE -> return ()

{-| __C declaration:__ @size_t@

    __defined at:__ @fun_attributes.h:8:13@

    __exported by:__ @fun_attributes.h@
-}
newtype Size_t = Size_t
  { un_Size_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
