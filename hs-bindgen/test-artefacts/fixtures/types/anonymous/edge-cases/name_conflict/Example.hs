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
    ( Example.Rectangle_anon'x1(..)
    , Example.Rectangle_anon'x2(..)
    , Example.Rectangle(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct \@Rectangle_anon\'x1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 2:3@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
data Rectangle_anon'x1 = Rectangle_anon'x1
  { rectangle_anon'x1_x1 :: BG.CInt
    {- ^ __C declaration:__ @x1@

         __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 3:11@

         __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
    -}
  , rectangle_anon'x1_y1 :: BG.CInt
    {- ^ __C declaration:__ @y1@

         __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 4:11@

         __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Rectangle_anon'x1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Rectangle_anon'x1 where

  readRaw =
    \ptr0 ->
          pure Rectangle_anon'x1
      <*> HasCField.readRaw (BG.Proxy @"rectangle_anon'x1_x1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"rectangle_anon'x1_y1") ptr0

instance Marshal.WriteRaw Rectangle_anon'x1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rectangle_anon'x1 rectangle_anon'x1_x12 rectangle_anon'x1_y13 ->
               HasCField.writeRaw (BG.Proxy @"rectangle_anon'x1_x1") ptr0 rectangle_anon'x1_x12
            >> HasCField.writeRaw (BG.Proxy @"rectangle_anon'x1_y1") ptr0 rectangle_anon'x1_y13

deriving via Marshal.EquivStorable Rectangle_anon'x1 instance BG.Storable Rectangle_anon'x1

{-| __C declaration:__ @x1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 3:11@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_anon'x1_x1" Rectangle_anon'x1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rectangle_anon'x1 { rectangle_anon'x1_x1 = y1
                            , rectangle_anon'x1_y1 = BG.getField @"rectangle_anon'x1_y1" x0
                            }
      , BG.getField @"rectangle_anon'x1_x1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_anon'x1_x1" (BG.Ptr Rectangle_anon'x1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_anon'x1_x1")

instance HasCField.HasCField Rectangle_anon'x1 "rectangle_anon'x1_x1" where

  type CFieldType Rectangle_anon'x1 "rectangle_anon'x1_x1" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 4:11@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_anon'x1_y1" Rectangle_anon'x1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rectangle_anon'x1 { rectangle_anon'x1_y1 = y1
                            , rectangle_anon'x1_x1 = BG.getField @"rectangle_anon'x1_x1" x0
                            }
      , BG.getField @"rectangle_anon'x1_y1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_anon'x1_y1" (BG.Ptr Rectangle_anon'x1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_anon'x1_y1")

instance HasCField.HasCField Rectangle_anon'x1 "rectangle_anon'x1_y1" where

  type CFieldType Rectangle_anon'x1 "rectangle_anon'x1_y1" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@Rectangle_anon\'x2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 6:3@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
data Rectangle_anon'x2 = Rectangle_anon'x2
  { rectangle_anon'x2_x2 :: BG.CInt
    {- ^ __C declaration:__ @x2@

         __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 7:9@

         __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
    -}
  , rectangle_anon'x2_y2 :: BG.CInt
    {- ^ __C declaration:__ @y2@

         __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 8:9@

         __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Rectangle_anon'x2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Rectangle_anon'x2 where

  readRaw =
    \ptr0 ->
          pure Rectangle_anon'x2
      <*> HasCField.readRaw (BG.Proxy @"rectangle_anon'x2_x2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"rectangle_anon'x2_y2") ptr0

instance Marshal.WriteRaw Rectangle_anon'x2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rectangle_anon'x2 rectangle_anon'x2_x22 rectangle_anon'x2_y23 ->
               HasCField.writeRaw (BG.Proxy @"rectangle_anon'x2_x2") ptr0 rectangle_anon'x2_x22
            >> HasCField.writeRaw (BG.Proxy @"rectangle_anon'x2_y2") ptr0 rectangle_anon'x2_y23

deriving via Marshal.EquivStorable Rectangle_anon'x2 instance BG.Storable Rectangle_anon'x2

{-| __C declaration:__ @x2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 7:9@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_anon'x2_x2" Rectangle_anon'x2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rectangle_anon'x2 { rectangle_anon'x2_x2 = y1
                            , rectangle_anon'x2_y2 = BG.getField @"rectangle_anon'x2_y2" x0
                            }
      , BG.getField @"rectangle_anon'x2_x2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_anon'x2_x2" (BG.Ptr Rectangle_anon'x2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_anon'x2_x2")

instance HasCField.HasCField Rectangle_anon'x2 "rectangle_anon'x2_x2" where

  type CFieldType Rectangle_anon'x2 "rectangle_anon'x2_x2" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 8:9@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_anon'x2_y2" Rectangle_anon'x2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rectangle_anon'x2 { rectangle_anon'x2_y2 = y1
                            , rectangle_anon'x2_x2 = BG.getField @"rectangle_anon'x2_x2" x0
                            }
      , BG.getField @"rectangle_anon'x2_y2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_anon'x2_y2" (BG.Ptr Rectangle_anon'x2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_anon'x2_y2")

instance HasCField.HasCField Rectangle_anon'x2 "rectangle_anon'x2_y2" where

  type CFieldType Rectangle_anon'x2 "rectangle_anon'x2_y2" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct Rectangle@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 1:8@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
data Rectangle = Rectangle
  { rectangle_anon'x1 :: Rectangle_anon'x1
    {- ^ __C declaration:__ @anon\'x1@

         __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 2:3@

         __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
    -}
  , rectangle_anon'x2 :: Rectangle_anon'x2
    {- ^ __C declaration:__ @anon\'x2@

         __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 6:3@

         __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Rectangle where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Rectangle where

  readRaw =
    \ptr0 ->
          pure Rectangle
      <*> HasCField.readRaw (BG.Proxy @"rectangle_anon'x1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"rectangle_anon'x2") ptr0

instance Marshal.WriteRaw Rectangle where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rectangle rectangle_anon'x12 rectangle_anon'x23 ->
               HasCField.writeRaw (BG.Proxy @"rectangle_anon'x1") ptr0 rectangle_anon'x12
            >> HasCField.writeRaw (BG.Proxy @"rectangle_anon'x2") ptr0 rectangle_anon'x23

deriving via Marshal.EquivStorable Rectangle instance BG.Storable Rectangle

{-| __C declaration:__ @anon\'x1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 2:3@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ Rectangle_anon'x1
         ) => BG.CompatHasField.HasField "rectangle_anon'x1" Rectangle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rectangle { rectangle_anon'x1 = y1
                    , rectangle_anon'x2 = BG.getField @"rectangle_anon'x2" x0
                    }
      , BG.getField @"rectangle_anon'x1" x0
      )

instance ( ty ~ Rectangle_anon'x1
         ) => BG.HasField "rectangle_anon'x1" (BG.Ptr Rectangle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_anon'x1")

instance HasCField.HasCField Rectangle "rectangle_anon'x1" where

  type CFieldType Rectangle "rectangle_anon'x1" =
    Rectangle_anon'x1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 3:11@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "rectangle_x1" Rectangle ty where

  getField =
    \x0 ->
      BG.getField @"rectangle_anon'x1_x1" (BG.getField @"rectangle_anon'x1" x0)

{-| __C declaration:__ @x1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 3:11@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_x1" Rectangle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"rectangle_anon'x1" x0 (\z2 ->
                                                                   BG.CompatHasField.setField @"rectangle_anon'x1_x1" z2 y1)
      , BG.getField @"rectangle_x1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_x1" (BG.Ptr Rectangle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_x1")

instance HasCField.HasCField Rectangle "rectangle_x1" where

  type CFieldType Rectangle "rectangle_x1" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 4:11@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "rectangle_y1" Rectangle ty where

  getField =
    \x0 ->
      BG.getField @"rectangle_anon'x1_y1" (BG.getField @"rectangle_anon'x1" x0)

{-| __C declaration:__ @y1@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 4:11@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_y1" Rectangle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"rectangle_anon'x1" x0 (\z2 ->
                                                                   BG.CompatHasField.setField @"rectangle_anon'x1_y1" z2 y1)
      , BG.getField @"rectangle_y1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_y1" (BG.Ptr Rectangle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_y1")

instance HasCField.HasCField Rectangle "rectangle_y1" where

  type CFieldType Rectangle "rectangle_y1" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @anon\'x2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 6:3@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ Rectangle_anon'x2
         ) => BG.CompatHasField.HasField "rectangle_anon'x2" Rectangle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rectangle { rectangle_anon'x2 = y1
                    , rectangle_anon'x1 = BG.getField @"rectangle_anon'x1" x0
                    }
      , BG.getField @"rectangle_anon'x2" x0
      )

instance ( ty ~ Rectangle_anon'x2
         ) => BG.HasField "rectangle_anon'x2" (BG.Ptr Rectangle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_anon'x2")

instance HasCField.HasCField Rectangle "rectangle_anon'x2" where

  type CFieldType Rectangle "rectangle_anon'x2" =
    Rectangle_anon'x2

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @x2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 7:9@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "rectangle_x2" Rectangle ty where

  getField =
    \x0 ->
      BG.getField @"rectangle_anon'x2_x2" (BG.getField @"rectangle_anon'x2" x0)

{-| __C declaration:__ @x2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 7:9@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_x2" Rectangle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"rectangle_anon'x2" x0 (\z2 ->
                                                                   BG.CompatHasField.setField @"rectangle_anon'x2_x2" z2 y1)
      , BG.getField @"rectangle_x2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_x2" (BG.Ptr Rectangle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_x2")

instance HasCField.HasCField Rectangle "rectangle_x2" where

  type CFieldType Rectangle "rectangle_x2" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @y2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 8:9@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "rectangle_y2" Rectangle ty where

  getField =
    \x0 ->
      BG.getField @"rectangle_anon'x2_y2" (BG.getField @"rectangle_anon'x2" x0)

{-| __C declaration:__ @y2@

    __defined at:__ @types\/anonymous\/edge-cases\/name_conflict.h 8:9@

    __exported by:__ @types\/anonymous\/edge-cases\/name_conflict.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rectangle_y2" Rectangle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"rectangle_anon'x2" x0 (\z2 ->
                                                                   BG.CompatHasField.setField @"rectangle_anon'x2_y2" z2 y1)
      , BG.getField @"rectangle_y2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rectangle_y2" (BG.Ptr Rectangle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rectangle_y2")

instance HasCField.HasCField Rectangle "rectangle_y2" where

  type CFieldType Rectangle "rectangle_y2" = BG.CInt

  offset# = \_ -> \_ -> 12
