{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module M.B
    ( M.B.Foo(..)
    , M.B.Bar(..)
    , M.B.bar_x
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.A
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @struct foo@

    __defined at:__ @b.h 6:8@

    __exported by:__ @b.h@
-}
data Foo = Foo
  { foo_next :: M.A.Foo
    {- ^ __C declaration:__ @next@

         __defined at:__ @b.h 6:18@

         __exported by:__ @b.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (BG.Proxy @"foo_next") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_next2 ->
            HasCField.writeRaw (BG.Proxy @"foo_next") ptr0 foo_next2

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

deriving via Struct.IsStructViaReadRaw Foo instance Struct.IsStruct Foo

{-| __C declaration:__ @next@

    __defined at:__ @b.h 6:18@

    __exported by:__ @b.h@
-}
instance (ty ~ M.A.Foo) => BG.CompatHasField.HasField "foo_next" Foo ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {foo_next = y1}, BG.getField @"foo_next" x0)

instance (ty ~ M.A.Foo) => BG.HasField "foo_next" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_next")

instance HasCField.HasCField Foo "foo_next" where

  type CFieldType Foo "foo_next" = M.A.Foo

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @bar@

    __defined at:__ @b.h 9:13@

    __exported by:__ @b.h@
-}
newtype Bar = Bar
  { unwrapBar :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapBar" Bar ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bar {unwrapBar = y1}, BG.getField @"unwrapBar" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapBar" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapBar")

instance HasCField.HasCField Bar "unwrapBar" where

  type CFieldType Bar "unwrapBar" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro bar_x@

    __defined at:__ @b.h 10:9@

    __exported by:__ @b.h@
-}
bar_x :: BG.CInt
bar_x = (1 :: BG.CInt)
