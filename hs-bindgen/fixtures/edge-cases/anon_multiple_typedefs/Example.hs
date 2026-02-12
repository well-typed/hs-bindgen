{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure)

{-| __C declaration:__ @struct point1a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point1a = Point1a
  { point1a_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point1a_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Point1a where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Point1a where

  readRaw =
    \ptr0 ->
          pure Point1a
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point1a_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point1a_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Point1a where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point1a point1a_x2 point1a_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point1a_x") ptr0 point1a_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point1a_y") ptr0 point1a_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Point1a instance F.Storable Point1a

instance HsBindgen.Runtime.HasCField.HasCField Point1a "point1a_x" where

  type CFieldType Point1a "point1a_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "point1a_x" (Ptr.Ptr Point1a) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point1a_x")

instance HsBindgen.Runtime.HasCField.HasCField Point1a "point1a_y" where

  type CFieldType Point1a "point1a_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "point1a_y" (Ptr.Ptr Point1a) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point1a_y")

{-| __C declaration:__ @point1b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 5:43@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point1b = Point1b
  { unwrapPoint1b :: Point1a
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapPoint1b" (Ptr.Ptr Point1b) (Ptr.Ptr Point1a) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPoint1b")

instance HsBindgen.Runtime.HasCField.HasCField Point1b "unwrapPoint1b" where

  type CFieldType Point1b "unwrapPoint1b" = Point1a

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct point2a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point2a = Point2a
  { point2a_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point2a_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Point2a where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Point2a where

  readRaw =
    \ptr0 ->
          pure Point2a
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point2a_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point2a_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Point2a where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point2a point2a_x2 point2a_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point2a_x") ptr0 point2a_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point2a_y") ptr0 point2a_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Point2a instance F.Storable Point2a

instance HsBindgen.Runtime.HasCField.HasCField Point2a "point2a_x" where

  type CFieldType Point2a "point2a_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "point2a_x" (Ptr.Ptr Point2a) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point2a_x")

instance HsBindgen.Runtime.HasCField.HasCField Point2a "point2a_y" where

  type CFieldType Point2a "point2a_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "point2a_y" (Ptr.Ptr Point2a) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point2a_y")

{-| __C declaration:__ @point2b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 8:44@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point2b = Point2b
  { unwrapPoint2b :: Ptr.Ptr Point2a
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance GHC.Records.HasField "unwrapPoint2b" (Ptr.Ptr Point2b) (Ptr.Ptr (Ptr.Ptr Point2a)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPoint2b")

instance HsBindgen.Runtime.HasCField.HasCField Point2b "unwrapPoint2b" where

  type CFieldType Point2b "unwrapPoint2b" =
    Ptr.Ptr Point2a

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@point3a_Aux@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:9@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
data Point3a_Aux = Point3a_Aux
  { point3a_Aux_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:22@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  , point3a_Aux_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:29@

         __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Point3a_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Point3a_Aux where

  readRaw =
    \ptr0 ->
          pure Point3a_Aux
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point3a_Aux_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point3a_Aux_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Point3a_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point3a_Aux point3a_Aux_x2 point3a_Aux_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point3a_Aux_x") ptr0 point3a_Aux_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point3a_Aux_y") ptr0 point3a_Aux_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Point3a_Aux instance F.Storable Point3a_Aux

instance HsBindgen.Runtime.HasCField.HasCField Point3a_Aux "point3a_Aux_x" where

  type CFieldType Point3a_Aux "point3a_Aux_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "point3a_Aux_x" (Ptr.Ptr Point3a_Aux) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point3a_Aux_x")

instance HsBindgen.Runtime.HasCField.HasCField Point3a_Aux "point3a_Aux_y" where

  type CFieldType Point3a_Aux "point3a_Aux_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "point3a_Aux_y" (Ptr.Ptr Point3a_Aux) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point3a_Aux_y")

{-| __C declaration:__ @point3a@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:35@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point3a = Point3a
  { unwrapPoint3a :: Ptr.Ptr Point3a_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance GHC.Records.HasField "unwrapPoint3a" (Ptr.Ptr Point3a) (Ptr.Ptr (Ptr.Ptr Point3a_Aux)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPoint3a")

instance HsBindgen.Runtime.HasCField.HasCField Point3a "unwrapPoint3a" where

  type CFieldType Point3a "unwrapPoint3a" =
    Ptr.Ptr Point3a_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @point3b@

    __defined at:__ @edge-cases\/anon_multiple_typedefs.h 11:45@

    __exported by:__ @edge-cases\/anon_multiple_typedefs.h@
-}
newtype Point3b = Point3b
  { unwrapPoint3b :: Ptr.Ptr Point3a_Aux
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance GHC.Records.HasField "unwrapPoint3b" (Ptr.Ptr Point3b) (Ptr.Ptr (Ptr.Ptr Point3a_Aux)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapPoint3b")

instance HsBindgen.Runtime.HasCField.HasCField Point3b "unwrapPoint3b" where

  type CFieldType Point3b "unwrapPoint3b" =
    Ptr.Ptr Point3a_Aux

  offset# = \_ -> \_ -> 0
