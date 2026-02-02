{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @struct FILE@

__defined at:__ @functions\/fun_attributes.h 7:9@

__exported by:__ @functions\/fun_attributes.h@
-}
data FILE = FILE
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize FILE where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw FILE where

  readRaw = \ptr0 -> pure FILE

instance HsBindgen.Runtime.Marshal.WriteRaw FILE where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable FILE instance F.Storable FILE

instance Data.Primitive.Types.Prim FILE where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> FILE

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, FILE #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              FILE -> s3

  indexOffAddr# = \addr0 -> \i1 -> FILE

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, FILE #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              FILE -> s3

{-| __C declaration:__ @size_t@

    __defined at:__ @functions\/fun_attributes.h 8:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
newtype Size_t = Size_t
  { unwrapSize_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Size_t) "unwrapSize_t")
         ) => GHC.Records.HasField "unwrapSize_t" (Ptr.Ptr Size_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSize_t")

instance HsBindgen.Runtime.HasCField.HasCField Size_t "unwrapSize_t" where

  type CFieldType Size_t "unwrapSize_t" = FC.CInt

  offset# = \_ -> \_ -> 0
