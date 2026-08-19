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
    ( Example.S_anon'x_x(..)
    , Example.S_anon'x(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union \@S_anon\'x_x@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
newtype S_anon'x_x = S_anon'x_x
  { unwrapS_anon'x_x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize S_anon'x_x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw S_anon'x_x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw S_anon'x_x

deriving via Marshal.EquivStorable S_anon'x_x instance BG.Storable S_anon'x_x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion S_anon'x_x

{-| __C declaration:__ @a@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "s_anon'x_x_a" S_anon'x_x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "s_anon'x_x_a" S_anon'x_x ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"s_anon'x_x_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "s_anon'x_x_a" (BG.Ptr S_anon'x_x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'x_x_a")

instance HasCField.HasCField S_anon'x_x "s_anon'x_x_a" where

  type CFieldType S_anon'x_x "s_anon'x_x_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@S_anon\'x@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
data S_anon'x = S_anon'x
  { s_anon'x_x :: S_anon'x_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

         __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize S_anon'x where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_anon'x where

  readRaw =
    \ptr0 ->
          pure S_anon'x
      <*> HasCField.readRaw (BG.Proxy @"s_anon'x_x") ptr0

instance Marshal.WriteRaw S_anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_anon'x s_anon'x_x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'x_x") ptr0 s_anon'x_x2

deriving via Marshal.EquivStorable S_anon'x instance BG.Storable S_anon'x

deriving via Struct.IsStructViaReadRaw S_anon'x instance Struct.IsStruct S_anon'x

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
instance ( ty ~ S_anon'x_x
         ) => BG.CompatHasField.HasField "s_anon'x_x" S_anon'x ty where

  hasField =
    \x0 ->
      (\y1 ->
         S_anon'x {s_anon'x_x = y1}, BG.getField @"s_anon'x_x" x0)

instance ( ty ~ S_anon'x_x
         ) => BG.HasField "s_anon'x_x" (BG.Ptr S_anon'x) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_anon'x_x")

instance HasCField.HasCField S_anon'x "s_anon'x_x" where

  type CFieldType S_anon'x "s_anon'x_x" = S_anon'x_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
data S = S
  { s_anon'x :: S_anon'x
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

         __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize S where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S where

  readRaw =
    \ptr0 ->
          pure S
      <*> HasCField.readRaw (BG.Proxy @"s_anon'x") ptr0

instance Marshal.WriteRaw S where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_anon'x2 ->
            HasCField.writeRaw (BG.Proxy @"s_anon'x") ptr0 s_anon'x2

deriving via Marshal.EquivStorable S instance BG.Storable S

deriving via Struct.IsStructViaReadRaw S instance Struct.IsStruct S

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
instance (ty ~ S_anon'x) => BG.CompatHasField.HasField "s_anon'x" S ty where

  hasField =
    \x0 ->
      (\y1 ->
         S {s_anon'x = y1}, BG.getField @"s_anon'x" x0)

instance (ty ~ S_anon'x) => BG.HasField "s_anon'x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_anon'x")

instance HasCField.HasCField S "s_anon'x" where

  type CFieldType S "s_anon'x" = S_anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
instance (ty ~ S_anon'x_x) => BG.HasField "s_x" S ty where

  getField =
    \x0 ->
      BG.getField @"s_anon'x_x" (BG.getField @"s_anon'x" x0)

{-| __C declaration:__ @x@

    __defined at:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h 3:1@

    __exported by:__ @types\/anonymous\/indirect-fields\/single_nesting_union_untagged.h@
-}
instance (ty ~ S_anon'x_x) => BG.CompatHasField.HasField "s_x" S ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"s_anon'x" x0 (\z2 ->
                                                          BG.CompatHasField.setField @"s_anon'x_x" z2 y1)
      , BG.getField @"s_x" x0
      )

instance (ty ~ S_anon'x_x) => BG.HasField "s_x" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_x")

instance HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = S_anon'x_x

  offset# = \_ -> \_ -> 0
