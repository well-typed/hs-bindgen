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
    ( Example.Baz(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct baz@

    __defined at:__ @types\/scoping\/wide_nesting.h 21:10@

    __exported by:__ @types\/scoping\/wide_nesting.h@
-}
data Baz = Baz
  { baz_x3_1 :: BG.CInt
    {- ^ __C declaration:__ @x3_1@

         __defined at:__ @types\/scoping\/wide_nesting.h 22:9@

         __exported by:__ @types\/scoping\/wide_nesting.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Baz where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Baz where

  readRaw =
    \ptr0 ->
          pure Baz
      <*> HasCField.readRaw (BG.Proxy @"baz_x3_1") ptr0

instance Marshal.WriteRaw Baz where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_x3_12 ->
            HasCField.writeRaw (BG.Proxy @"baz_x3_1") ptr0 baz_x3_12

deriving via Marshal.EquivStorable Baz instance BG.Storable Baz

deriving via Struct.IsStructViaStorable Baz instance Struct.IsStruct Baz

{-| __C declaration:__ @x3_1@

    __defined at:__ @types\/scoping\/wide_nesting.h 22:9@

    __exported by:__ @types\/scoping\/wide_nesting.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "baz_x3_1" Baz ty where

  hasField =
    \x0 ->
      (\y1 ->
         Baz {baz_x3_1 = y1}, BG.getField @"baz_x3_1" x0)

instance (ty ~ BG.CInt) => BG.HasField "baz_x3_1" (BG.Ptr Baz) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"baz_x3_1")

instance HasCField.HasCField Baz "baz_x3_1" where

  type CFieldType Baz "baz_x3_1" = BG.CInt

  offset# = \_ -> \_ -> 0
