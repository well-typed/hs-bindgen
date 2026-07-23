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
    ( Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified OtherExample

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 3:8@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
data S = S
  { s_anon'anon'x :: OtherExample.Point3D
    {- ^ __C declaration:__ @anon\'anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 4:3@

         __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_anon'anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'anon'x") ptr0 s_anon'anon'x2

deriving via Marshal.EquivStorable S instance BG.Storable S

{-| __C declaration:__ @anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 4:3@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
instance ( ty ~ OtherExample.Point3D
         ) => BG.CompatHasField.HasField "s_anon'anon'x" S ty where

  hasField =
    \x0 ->
      (\y1 ->
         S {s_anon'anon'x = y1}, BG.getField @"s_anon'anon'x" x0)

instance ( ty ~ OtherExample.Point3D
         ) => BG.HasField "s_anon'anon'x" (BG.Ptr S) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x")

instance HasCField.HasCField S "s_anon'anon'x" where

  type CFieldType S "s_anon'anon'x" =
    OtherExample.Point3D

  offset# = \_ -> \_ -> 0
