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

module IncludeCycle.B
    ( IncludeCycle.B.B(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified IncludeCycle.A
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct b@

    __defined at:__ @b.h 6:8@

    __exported by:__ @include_cycle.h@
-}
data B = B
  { b_a :: BG.Ptr IncludeCycle.A.A
    {- ^ __C declaration:__ @a@

         __defined at:__ @b.h 7:13@

         __exported by:__ @include_cycle.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HasCField.readRaw (BG.Proxy @"b_a") ptr0

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_a2 ->
            HasCField.writeRaw (BG.Proxy @"b_a") ptr0 b_a2

deriving via Marshal.EquivStorable B instance BG.Storable B

deriving via Struct.IsStructViaReadRaw B instance Struct.IsStruct B

{-| __C declaration:__ @a@

    __defined at:__ @b.h 7:13@

    __exported by:__ @include_cycle.h@
-}
instance ( ty ~ BG.Ptr IncludeCycle.A.A
         ) => BG.CompatHasField.HasField "b_a" B ty where

  hasField =
    \x0 -> (\y1 -> B {b_a = y1}, BG.getField @"b_a" x0)

instance ( ty ~ BG.Ptr IncludeCycle.A.A
         ) => BG.HasField "b_a" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b_a")

instance HasCField.HasCField B "b_a" where

  type CFieldType B "b_a" = BG.Ptr IncludeCycle.A.A

  offset# = \_ -> \_ -> 0
