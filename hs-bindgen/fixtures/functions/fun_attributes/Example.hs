{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @struct FILE@

__defined at:__ @functions\/fun_attributes.h 7:9@

__exported by:__ @functions\/fun_attributes.h@
-}
data FILE = FILE
  {}
  deriving stock (GHC.Generics.Generic, Eq, Show)

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

{-| __C declaration:__ @size_t@

    __defined at:__ @functions\/fun_attributes.h 8:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
newtype Size_t = Size_t
  { unwrapSize_t :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance GHC.Records.HasField "unwrapSize_t" (Ptr.Ptr Size_t) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSize_t")

instance HsBindgen.Runtime.HasCField.HasCField Size_t "unwrapSize_t" where

  type CFieldType Size_t "unwrapSize_t" = FC.CInt

  offset# = \_ -> \_ -> 0
