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
    ( Example.Thing(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct thing@

    __defined at:__ @types\/structs\/struct_arg.h 2:8@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
data Thing = Thing
  { thing_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/structs\/struct_arg.h 3:9@

         __exported by:__ @types\/structs\/struct_arg.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Thing where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Thing where

  readRaw =
    \ptr0 ->
          pure Thing
      <*> HasCField.readRaw (BG.Proxy @"thing_x") ptr0

instance Marshal.WriteRaw Thing where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Thing thing_x2 ->
            HasCField.writeRaw (BG.Proxy @"thing_x") ptr0 thing_x2

deriving via Marshal.EquivStorable Thing instance BG.Storable Thing

{-| __C declaration:__ @x@

    __defined at:__ @types\/structs\/struct_arg.h 3:9@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "thing_x" Thing ty where

  hasField =
    \x0 ->
      (\y1 ->
         Thing {thing_x = y1}, BG.getField @"thing_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "thing_x" (BG.Ptr Thing) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"thing_x")

instance HasCField.HasCField Thing "thing_x" where

  type CFieldType Thing "thing_x" = BG.CInt

  offset# = \_ -> \_ -> 0
