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
    ( Example.Some_struct_field1(..)
    , Example.Some_struct(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct \@some_struct_field1@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:3@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
data Some_struct_field1 = Some_struct_field1
  { some_struct_field1_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:16@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field1_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:23@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Some_struct_field1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Some_struct_field1 where

  readRaw =
    \ptr0 ->
          pure Some_struct_field1
      <*> HasCField.readRaw (BG.Proxy @"some_struct_field1_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"some_struct_field1_y") ptr0

instance Marshal.WriteRaw Some_struct_field1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct_field1 some_struct_field1_x2 some_struct_field1_y3 ->
               HasCField.writeRaw (BG.Proxy @"some_struct_field1_x") ptr0 some_struct_field1_x2
            >> HasCField.writeRaw (BG.Proxy @"some_struct_field1_y") ptr0 some_struct_field1_y3

deriving via Marshal.EquivStorable Some_struct_field1 instance BG.Storable Some_struct_field1

{-| __C declaration:__ @x@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:16@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "some_struct_field1_x" Some_struct_field1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Some_struct_field1 { some_struct_field1_x = y1
                             , some_struct_field1_y = BG.getField @"some_struct_field1_y" x0
                             }
      , BG.getField @"some_struct_field1_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "some_struct_field1_x" (BG.Ptr Some_struct_field1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"some_struct_field1_x")

instance HasCField.HasCField Some_struct_field1 "some_struct_field1_x" where

  type CFieldType Some_struct_field1 "some_struct_field1_x" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:23@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "some_struct_field1_y" Some_struct_field1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Some_struct_field1 { some_struct_field1_y = y1
                             , some_struct_field1_x = BG.getField @"some_struct_field1_x" x0
                             }
      , BG.getField @"some_struct_field1_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "some_struct_field1_y" (BG.Ptr Some_struct_field1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"some_struct_field1_y")

instance HasCField.HasCField Some_struct_field1 "some_struct_field1_y" where

  type CFieldType Some_struct_field1 "some_struct_field1_y" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct some_struct@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 4:8@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
data Some_struct = Some_struct
  { some_struct_field1 :: Some_struct_field1
    {- ^ __C declaration:__ @field1@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:28@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field2 :: Some_struct_field1
    {- ^ __C declaration:__ @field2@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:36@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field3 :: Some_struct_field1
    {- ^ __C declaration:__ @field3@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:44@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Some_struct where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Some_struct where

  readRaw =
    \ptr0 ->
          pure Some_struct
      <*> HasCField.readRaw (BG.Proxy @"some_struct_field1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"some_struct_field2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"some_struct_field3") ptr0

instance Marshal.WriteRaw Some_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct some_struct_field12 some_struct_field23 some_struct_field34 ->
               HasCField.writeRaw (BG.Proxy @"some_struct_field1") ptr0 some_struct_field12
            >> HasCField.writeRaw (BG.Proxy @"some_struct_field2") ptr0 some_struct_field23
            >> HasCField.writeRaw (BG.Proxy @"some_struct_field3") ptr0 some_struct_field34

deriving via Marshal.EquivStorable Some_struct instance BG.Storable Some_struct

{-| __C declaration:__ @field1@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:28@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
instance ( ty ~ Some_struct_field1
         ) => BG.CompatHasField.HasField "some_struct_field1" Some_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Some_struct { some_struct_field1 = y1
                      , some_struct_field2 = BG.getField @"some_struct_field2" x0
                      , some_struct_field3 = BG.getField @"some_struct_field3" x0
                      }
      , BG.getField @"some_struct_field1" x0
      )

instance ( ty ~ Some_struct_field1
         ) => BG.HasField "some_struct_field1" (BG.Ptr Some_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"some_struct_field1")

instance HasCField.HasCField Some_struct "some_struct_field1" where

  type CFieldType Some_struct "some_struct_field1" =
    Some_struct_field1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @field2@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:36@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
instance ( ty ~ Some_struct_field1
         ) => BG.CompatHasField.HasField "some_struct_field2" Some_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Some_struct { some_struct_field2 = y1
                      , some_struct_field1 = BG.getField @"some_struct_field1" x0
                      , some_struct_field3 = BG.getField @"some_struct_field3" x0
                      }
      , BG.getField @"some_struct_field2" x0
      )

instance ( ty ~ Some_struct_field1
         ) => BG.HasField "some_struct_field2" (BG.Ptr Some_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"some_struct_field2")

instance HasCField.HasCField Some_struct "some_struct_field2" where

  type CFieldType Some_struct "some_struct_field2" =
    Some_struct_field1

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @field3@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:44@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
instance ( ty ~ Some_struct_field1
         ) => BG.CompatHasField.HasField "some_struct_field3" Some_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Some_struct { some_struct_field3 = y1
                      , some_struct_field1 = BG.getField @"some_struct_field1" x0
                      , some_struct_field2 = BG.getField @"some_struct_field2" x0
                      }
      , BG.getField @"some_struct_field3" x0
      )

instance ( ty ~ Some_struct_field1
         ) => BG.HasField "some_struct_field3" (BG.Ptr Some_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"some_struct_field3")

instance HasCField.HasCField Some_struct "some_struct_field3" where

  type CFieldType Some_struct "some_struct_field3" =
    Some_struct_field1

  offset# = \_ -> \_ -> 16
