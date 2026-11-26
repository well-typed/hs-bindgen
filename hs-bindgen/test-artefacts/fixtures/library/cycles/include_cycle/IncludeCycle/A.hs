{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module IncludeCycle.A
    ( IncludeCycle.A.A(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct a@

    __defined at:__ @a.h 12:8@

    __exported by:__ @include_cycle.h@
-}
data A = A
  { a_value :: BG.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @a.h 13:7@

         __exported by:__ @include_cycle.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (BG.Proxy @"a_value") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_value2 ->
            HasCField.writeRaw (BG.Proxy @"a_value") ptr0 a_value2

deriving via Marshal.EquivStorable A instance BG.Storable A

deriving via Struct.IsStructViaReadRaw A instance Struct.IsStruct A

{-| __C declaration:__ @value@

    __defined at:__ @a.h 13:7@

    __exported by:__ @include_cycle.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "a_value" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {a_value = y1}, BG.getField @"a_value" x0)

instance (ty ~ BG.CInt) => BG.HasField "a_value" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_value")

instance HasCField.HasCField A "a_value" where

  type CFieldType A "a_value" = BG.CInt

  offset# = \_ -> \_ -> 0
