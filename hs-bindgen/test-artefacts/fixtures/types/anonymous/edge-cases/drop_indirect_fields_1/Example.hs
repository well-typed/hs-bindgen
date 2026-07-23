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
    ( Example.S_anon'anon'x(..)
    , Example.S(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified OtherExample

{-| __C declaration:__ @struct \@S_anon\'anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 4:3@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
data S_anon'anon'x = S_anon'anon'x
  { s_anon'anon'x_Point2D :: OtherExample.Point2D
    {- ^ __C declaration:__ @anon\'x@

         __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 5:5@

         __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
    -}
  , s_anon'anon'x_z :: BG.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 9:9@

         __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S_anon'anon'x where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S_anon'anon'x where

  readRaw =
    \ptr0 ->
          pure S_anon'anon'x
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x_Point2D") ptr0
      <*> HasCField.readRaw (BG.Proxy @"s_anon'anon'x_z") ptr0

instance Marshal.WriteRaw S_anon'anon'x where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S_anon'anon'x s_anon'anon'x_Point2D2 s_anon'anon'x_z3 ->
               HasCField.writeRaw (BG.Proxy @"s_anon'anon'x_Point2D") ptr0 s_anon'anon'x_Point2D2
            >> HasCField.writeRaw (BG.Proxy @"s_anon'anon'x_z") ptr0 s_anon'anon'x_z3

deriving via Marshal.EquivStorable S_anon'anon'x instance BG.Storable S_anon'anon'x

{-| __C declaration:__ @anon\'x@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 5:5@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
instance ( ty ~ OtherExample.Point2D
         ) => BG.CompatHasField.HasField "s_anon'anon'x_Point2D" S_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S_anon'anon'x { s_anon'anon'x_Point2D = y1
                        , s_anon'anon'x_z = BG.getField @"s_anon'anon'x_z" x0
                        }
      , BG.getField @"s_anon'anon'x_Point2D" x0
      )

instance ( ty ~ OtherExample.Point2D
         ) => BG.HasField "s_anon'anon'x_Point2D" (BG.Ptr S_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_Point2D")

instance HasCField.HasCField S_anon'anon'x "s_anon'anon'x_Point2D" where

  type CFieldType S_anon'anon'x "s_anon'anon'x_Point2D" =
    OtherExample.Point2D

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "s_anon'anon'x_z" S_anon'anon'x ty where

  hasField =
    \x0 ->
      ( \y1 ->
          S_anon'anon'x { s_anon'anon'x_z = y1
                        , s_anon'anon'x_Point2D = BG.getField @"s_anon'anon'x_Point2D" x0
                        }
      , BG.getField @"s_anon'anon'x_z" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "s_anon'anon'x_z" (BG.Ptr S_anon'anon'x) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x_z")

instance HasCField.HasCField S_anon'anon'x "s_anon'anon'x_z" where

  type CFieldType S_anon'anon'x "s_anon'anon'x_z" =
    BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct S@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 3:8@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
data S = S
  { s_anon'anon'x :: S_anon'anon'x
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
instance ( ty ~ S_anon'anon'x
         ) => BG.CompatHasField.HasField "s_anon'anon'x" S ty where

  hasField =
    \x0 ->
      (\y1 ->
         S {s_anon'anon'x = y1}, BG.getField @"s_anon'anon'x" x0)

instance ( ty ~ S_anon'anon'x
         ) => BG.HasField "s_anon'anon'x" (BG.Ptr S) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"s_anon'anon'x")

instance HasCField.HasCField S "s_anon'anon'x" where

  type CFieldType S "s_anon'anon'x" = S_anon'anon'x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "s_z" S ty where

  getField =
    \x0 ->
      BG.getField @"s_anon'anon'x_z" (BG.getField @"s_anon'anon'x" x0)

{-| __C declaration:__ @z@

    __defined at:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h 9:9@

    __exported by:__ @types\/anonymous\/edge-cases\/drop_indirect_fields.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s_z" S ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"s_anon'anon'x" x0 (\z2 ->
                                                               BG.CompatHasField.setField @"s_anon'anon'x_z" z2 y1)
      , BG.getField @"s_z" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "s_z" (BG.Ptr S) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s_z")

instance HasCField.HasCField S "s_z" where

  type CFieldType S "s_z" = BG.CInt

  offset# = \_ -> \_ -> 8
