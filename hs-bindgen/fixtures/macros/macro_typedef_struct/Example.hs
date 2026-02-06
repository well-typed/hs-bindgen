{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @MY_TYPE@

    __defined at:__ @macros\/macro_typedef_struct.h 1:9@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
newtype MY_TYPE = MY_TYPE
  { unwrapMY_TYPE :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord, Read, Show)
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MY_TYPE) "unwrapMY_TYPE")
         ) => GHC.Records.HasField "unwrapMY_TYPE" (Ptr.Ptr MY_TYPE) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMY_TYPE")

instance HsBindgen.Runtime.HasCField.HasCField MY_TYPE "unwrapMY_TYPE" where

  type CFieldType MY_TYPE "unwrapMY_TYPE" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bar@

    __defined at:__ @macros\/macro_typedef_struct.h 3:9@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
data Bar = Bar
  { bar_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/macro_typedef_struct.h 4:7@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  , bar_y :: MY_TYPE
    {- ^ __C declaration:__ @y@

         __defined at:__ @macros\/macro_typedef_struct.h 5:11@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"bar_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"bar_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"bar_x") ptr0 bar_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"bar_y") ptr0 bar_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Bar instance F.Storable Bar

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_x" where

  type CFieldType Bar "bar_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_x")
         ) => GHC.Records.HasField "bar_x" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bar_x")

instance HsBindgen.Runtime.HasCField.HasCField Bar "bar_y" where

  type CFieldType Bar "bar_y" = MY_TYPE

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bar) "bar_y")
         ) => GHC.Records.HasField "bar_y" (Ptr.Ptr Bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bar_y")
