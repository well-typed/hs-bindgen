{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct point1a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point1a = Point1a
  { point1a_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point1a_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Point1a where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point1a where

  readRaw =
    \ptr0 ->
          pure Point1a
      <*> HasCField.readRaw (RIP.Proxy @"point1a_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"point1a_y") ptr0

instance Marshal.WriteRaw Point1a where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point1a point1a_x2 point1a_y3 ->
               HasCField.writeRaw (RIP.Proxy @"point1a_x") ptr0 point1a_x2
            >> HasCField.writeRaw (RIP.Proxy @"point1a_y") ptr0 point1a_y3

deriving via Marshal.EquivStorable Point1a instance RIP.Storable Point1a

instance HasCField.HasCField Point1a "point1a_x" where

  type CFieldType Point1a "point1a_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point1a_x" (RIP.Ptr Point1a) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"point1a_x")

instance HasCField.HasCField Point1a "point1a_y" where

  type CFieldType Point1a "point1a_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point1a_y" (RIP.Ptr Point1a) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"point1a_y")

{-| __C declaration:__ @point1b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:43@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point1b = Point1b
  { unwrapPoint1b :: Point1a
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) Point1a
         ) => RIP.HasField "unwrapPoint1b" (RIP.Ptr Point1b) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPoint1b")

instance HasCField.HasCField Point1b "unwrapPoint1b" where

  type CFieldType Point1b "unwrapPoint1b" = Point1a

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct point2a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point2a = Point2a
  { point2a_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point2a_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Point2a where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point2a where

  readRaw =
    \ptr0 ->
          pure Point2a
      <*> HasCField.readRaw (RIP.Proxy @"point2a_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"point2a_y") ptr0

instance Marshal.WriteRaw Point2a where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point2a point2a_x2 point2a_y3 ->
               HasCField.writeRaw (RIP.Proxy @"point2a_x") ptr0 point2a_x2
            >> HasCField.writeRaw (RIP.Proxy @"point2a_y") ptr0 point2a_y3

deriving via Marshal.EquivStorable Point2a instance RIP.Storable Point2a

instance HasCField.HasCField Point2a "point2a_x" where

  type CFieldType Point2a "point2a_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point2a_x" (RIP.Ptr Point2a) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"point2a_x")

instance HasCField.HasCField Point2a "point2a_y" where

  type CFieldType Point2a "point2a_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point2a_y" (RIP.Ptr Point2a) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"point2a_y")

{-| __C declaration:__ @point2b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:44@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point2b = Point2b
  { unwrapPoint2b :: RIP.Ptr Point2a
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr Point2a)
         ) => RIP.HasField "unwrapPoint2b" (RIP.Ptr Point2b) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPoint2b")

instance HasCField.HasCField Point2b "unwrapPoint2b" where

  type CFieldType Point2b "unwrapPoint2b" =
    RIP.Ptr Point2a

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@point3a_Aux@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point3a_Aux = Point3a_Aux
  { point3a_Aux_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point3a_Aux_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Point3a_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point3a_Aux where

  readRaw =
    \ptr0 ->
          pure Point3a_Aux
      <*> HasCField.readRaw (RIP.Proxy @"point3a_Aux_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"point3a_Aux_y") ptr0

instance Marshal.WriteRaw Point3a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point3a_Aux point3a_Aux_x2 point3a_Aux_y3 ->
               HasCField.writeRaw (RIP.Proxy @"point3a_Aux_x") ptr0 point3a_Aux_x2
            >> HasCField.writeRaw (RIP.Proxy @"point3a_Aux_y") ptr0 point3a_Aux_y3

deriving via Marshal.EquivStorable Point3a_Aux instance RIP.Storable Point3a_Aux

instance HasCField.HasCField Point3a_Aux "point3a_Aux_x" where

  type CFieldType Point3a_Aux "point3a_Aux_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point3a_Aux_x" (RIP.Ptr Point3a_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"point3a_Aux_x")

instance HasCField.HasCField Point3a_Aux "point3a_Aux_y" where

  type CFieldType Point3a_Aux "point3a_Aux_y" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point3a_Aux_y" (RIP.Ptr Point3a_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"point3a_Aux_y")

{-| __C declaration:__ @point3a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:35@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point3a = Point3a
  { unwrapPoint3a :: RIP.Ptr Point3a_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr Point3a_Aux)
         ) => RIP.HasField "unwrapPoint3a" (RIP.Ptr Point3a) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPoint3a")

instance HasCField.HasCField Point3a "unwrapPoint3a" where

  type CFieldType Point3a "unwrapPoint3a" =
    RIP.Ptr Point3a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @point3b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:45@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point3b = Point3b
  { unwrapPoint3b :: RIP.Ptr Point3a_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr Point3a_Aux)
         ) => RIP.HasField "unwrapPoint3b" (RIP.Ptr Point3b) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapPoint3b")

instance HasCField.HasCField Point3b "unwrapPoint3b" where

  type CFieldType Point3b "unwrapPoint3b" =
    RIP.Ptr Point3a_Aux

  offset# = \_ -> \_ -> 0
