{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Point1a(..)
    , Example.Point1b(..)
    , Example.Point2a(..)
    , Example.Point2b(..)
    , Example.Point3a_Aux(..)
    , Example.Point3a(..)
    , Example.Point3b(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct point1a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point1a = Point1a
  { point1a_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point1a_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Point1a where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point1a where

  readRaw =
    \ptr0 ->
          pure Point1a
      <*> HasCField.readRaw (BG.Proxy @"point1a_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point1a_y") ptr0

instance Marshal.WriteRaw Point1a where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point1a point1a_x2 point1a_y3 ->
               HasCField.writeRaw (BG.Proxy @"point1a_x") ptr0 point1a_x2
            >> HasCField.writeRaw (BG.Proxy @"point1a_y") ptr0 point1a_y3

deriving via Marshal.EquivStorable Point1a instance BG.Storable Point1a

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "point1a_x" Point1a ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point1a {point1a_x = y1, point1a_y = BG.getField @"point1a_y" x0}
      , BG.getField @"point1a_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point1a_x" (BG.Ptr Point1a) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point1a_x")

instance HasCField.HasCField Point1a "point1a_x" where

  type CFieldType Point1a "point1a_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "point1a_y" Point1a ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point1a {point1a_y = y1, point1a_x = BG.getField @"point1a_x" x0}
      , BG.getField @"point1a_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point1a_y" (BG.Ptr Point1a) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point1a_y")

instance HasCField.HasCField Point1a "point1a_y" where

  type CFieldType Point1a "point1a_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @point1b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:43@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point1b = Point1b
  { unwrapPoint1b :: Point1a
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ Point1a
         ) => BG.CompatHasField.HasField "unwrapPoint1b" Point1b ty where

  hasField =
    \x0 ->
      (\y1 ->
         Point1b {unwrapPoint1b = y1}, BG.getField @"unwrapPoint1b" x0)

instance ( ty ~ Point1a
         ) => BG.HasField "unwrapPoint1b" (BG.Ptr Point1b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPoint1b")

instance HasCField.HasCField Point1b "unwrapPoint1b" where

  type CFieldType Point1b "unwrapPoint1b" = Point1a

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct point2a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point2a = Point2a
  { point2a_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point2a_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Point2a where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point2a where

  readRaw =
    \ptr0 ->
          pure Point2a
      <*> HasCField.readRaw (BG.Proxy @"point2a_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point2a_y") ptr0

instance Marshal.WriteRaw Point2a where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point2a point2a_x2 point2a_y3 ->
               HasCField.writeRaw (BG.Proxy @"point2a_x") ptr0 point2a_x2
            >> HasCField.writeRaw (BG.Proxy @"point2a_y") ptr0 point2a_y3

deriving via Marshal.EquivStorable Point2a instance BG.Storable Point2a

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "point2a_x" Point2a ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point2a {point2a_x = y1, point2a_y = BG.getField @"point2a_y" x0}
      , BG.getField @"point2a_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point2a_x" (BG.Ptr Point2a) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point2a_x")

instance HasCField.HasCField Point2a "point2a_x" where

  type CFieldType Point2a "point2a_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "point2a_y" Point2a ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point2a {point2a_y = y1, point2a_x = BG.getField @"point2a_x" x0}
      , BG.getField @"point2a_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point2a_y" (BG.Ptr Point2a) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point2a_y")

instance HasCField.HasCField Point2a "point2a_y" where

  type CFieldType Point2a "point2a_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @point2b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:44@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point2b = Point2b
  { unwrapPoint2b :: BG.Ptr Point2a
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr Point2a
         ) => BG.CompatHasField.HasField "unwrapPoint2b" Point2b ty where

  hasField =
    \x0 ->
      (\y1 ->
         Point2b {unwrapPoint2b = y1}, BG.getField @"unwrapPoint2b" x0)

instance ( ty ~ BG.Ptr Point2a
         ) => BG.HasField "unwrapPoint2b" (BG.Ptr Point2b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPoint2b")

instance HasCField.HasCField Point2b "unwrapPoint2b" where

  type CFieldType Point2b "unwrapPoint2b" =
    BG.Ptr Point2a

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@point3a_Aux@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point3a_Aux = Point3a_Aux
  { point3a_Aux_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point3a_Aux_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Point3a_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point3a_Aux where

  readRaw =
    \ptr0 ->
          pure Point3a_Aux
      <*> HasCField.readRaw (BG.Proxy @"point3a_Aux_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point3a_Aux_y") ptr0

instance Marshal.WriteRaw Point3a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point3a_Aux point3a_Aux_x2 point3a_Aux_y3 ->
               HasCField.writeRaw (BG.Proxy @"point3a_Aux_x") ptr0 point3a_Aux_x2
            >> HasCField.writeRaw (BG.Proxy @"point3a_Aux_y") ptr0 point3a_Aux_y3

deriving via Marshal.EquivStorable Point3a_Aux instance BG.Storable Point3a_Aux

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "point3a_Aux_x" Point3a_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point3a_Aux {point3a_Aux_x = y1, point3a_Aux_y = BG.getField @"point3a_Aux_y" x0}
      , BG.getField @"point3a_Aux_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point3a_Aux_x" (BG.Ptr Point3a_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"point3a_Aux_x")

instance HasCField.HasCField Point3a_Aux "point3a_Aux_x" where

  type CFieldType Point3a_Aux "point3a_Aux_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "point3a_Aux_y" Point3a_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point3a_Aux {point3a_Aux_y = y1, point3a_Aux_x = BG.getField @"point3a_Aux_x" x0}
      , BG.getField @"point3a_Aux_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point3a_Aux_y" (BG.Ptr Point3a_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"point3a_Aux_y")

instance HasCField.HasCField Point3a_Aux "point3a_Aux_y" where

  type CFieldType Point3a_Aux "point3a_Aux_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @point3a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:35@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point3a = Point3a
  { unwrapPoint3a :: BG.Ptr Point3a_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr Point3a_Aux
         ) => BG.CompatHasField.HasField "unwrapPoint3a" Point3a ty where

  hasField =
    \x0 ->
      (\y1 ->
         Point3a {unwrapPoint3a = y1}, BG.getField @"unwrapPoint3a" x0)

instance ( ty ~ BG.Ptr Point3a_Aux
         ) => BG.HasField "unwrapPoint3a" (BG.Ptr Point3a) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPoint3a")

instance HasCField.HasCField Point3a "unwrapPoint3a" where

  type CFieldType Point3a "unwrapPoint3a" =
    BG.Ptr Point3a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @point3b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:45@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point3b = Point3b
  { unwrapPoint3b :: BG.Ptr Point3a_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr Point3a_Aux
         ) => BG.CompatHasField.HasField "unwrapPoint3b" Point3b ty where

  hasField =
    \x0 ->
      (\y1 ->
         Point3b {unwrapPoint3b = y1}, BG.getField @"unwrapPoint3b" x0)

instance ( ty ~ BG.Ptr Point3a_Aux
         ) => BG.HasField "unwrapPoint3b" (BG.Ptr Point3b) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPoint3b")

instance HasCField.HasCField Point3b "unwrapPoint3b" where

  type CFieldType Point3b "unwrapPoint3b" =
    BG.Ptr Point3a_Aux

  offset# = \_ -> \_ -> 0
