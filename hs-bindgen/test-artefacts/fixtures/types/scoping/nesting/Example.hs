{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Bar(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct bar@

    __defined at:__ @types\/scoping\/nesting.h 14:10@

    __exported by:__ @types\/scoping\/nesting.h@
-}
data Bar = Bar
  { bar_x1_1 :: BG.CInt
    {- ^ Comment attached to @foo::bar::x1_1@ .

         This comment must be preserved, even though the enclosing declaration uses an unsupported feature.

         __C declaration:__ @x1_1@

         __defined at:__ @types\/scoping\/nesting.h 21:9@

         __exported by:__ @types\/scoping\/nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (BG.Proxy @"bar_x1_1") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x1_12 ->
            HasCField.writeRaw (BG.Proxy @"bar_x1_1") ptr0 bar_x1_12

deriving via Marshal.EquivStorable Bar instance BG.Storable Bar

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "bar_x1_1" Bar ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bar {bar_x1_1 = y1}, BG.getField @"bar_x1_1" x0)

instance (ty ~ BG.CInt) => BG.HasField "bar_x1_1" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_x1_1")

instance HasCField.HasCField Bar "bar_x1_1" where

  type CFieldType Bar "bar_x1_1" = BG.CInt

  offset# = \_ -> \_ -> 0
