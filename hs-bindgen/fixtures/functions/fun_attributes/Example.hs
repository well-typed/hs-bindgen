{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @FILE@

__defined at:__ @functions\/fun_attributes.h:7:9@

__exported by:__ @functions\/fun_attributes.h@
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

    __defined at:__ @functions\/fun_attributes.h:8:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
newtype Size_t = Size_t
  { un_Size_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Size_t) "un_Size_t")
         ) => GHC.Records.HasField "un_Size_t" (Ptr.Ptr Size_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Size_t")

instance HsBindgen.Runtime.HasCField.HasCField Size_t "un_Size_t" where

  type CFieldType Size_t "un_Size_t" = FC.CInt

  offset# = \_ -> \_ -> 0
