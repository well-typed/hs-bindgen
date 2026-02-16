{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @struct globalConfig@

    __defined at:__ @manual\/globals.h 7:9@

    __exported by:__ @manual\/globals.h@
-}
data GlobalConfig = GlobalConfig
  { globalConfig_numThreads :: FC.CInt
    {- ^ __C declaration:__ @numThreads@

         __defined at:__ @manual\/globals.h 8:7@

         __exported by:__ @manual\/globals.h@
    -}
  , globalConfig_numWorkers :: FC.CInt
    {- ^ __C declaration:__ @numWorkers@

         __defined at:__ @manual\/globals.h 9:7@

         __exported by:__ @manual\/globals.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize GlobalConfig where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw GlobalConfig where

  readRaw =
    \ptr0 ->
          pure GlobalConfig
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"globalConfig_numThreads") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"globalConfig_numWorkers") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw GlobalConfig where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          GlobalConfig globalConfig_numThreads2 globalConfig_numWorkers3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"globalConfig_numThreads") ptr0 globalConfig_numThreads2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"globalConfig_numWorkers") ptr0 globalConfig_numWorkers3

deriving via HsBindgen.Runtime.Marshal.EquivStorable GlobalConfig instance F.Storable GlobalConfig

instance HsBindgen.Runtime.HasCField.HasCField GlobalConfig "globalConfig_numThreads" where

  type CFieldType GlobalConfig "globalConfig_numThreads" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "globalConfig_numThreads" (Ptr.Ptr GlobalConfig) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"globalConfig_numThreads")

instance HsBindgen.Runtime.HasCField.HasCField GlobalConfig "globalConfig_numWorkers" where

  type CFieldType GlobalConfig "globalConfig_numWorkers" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "globalConfig_numWorkers" (Ptr.Ptr GlobalConfig) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"globalConfig_numWorkers")

{-| __C declaration:__ @ConstInt@

    __defined at:__ @manual\/globals.h 40:19@

    __exported by:__ @manual\/globals.h@
-}
newtype ConstInt = ConstInt
  { unwrapConstInt :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "unwrapConstInt" (Ptr.Ptr ConstInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConstInt")

instance HsBindgen.Runtime.HasCField.HasCField ConstInt "unwrapConstInt" where

  type CFieldType ConstInt "unwrapConstInt" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct tuple@

    __defined at:__ @manual\/globals.h 52:8@

    __exported by:__ @manual\/globals.h@
-}
data Tuple = Tuple
  { tuple_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/globals.h 52:20@

         __exported by:__ @manual\/globals.h@
    -}
  , tuple_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/globals.h 52:33@

         __exported by:__ @manual\/globals.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Tuple where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Tuple where

  readRaw =
    \ptr0 ->
          pure Tuple
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"tuple_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"tuple_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Tuple where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tuple tuple_x2 tuple_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"tuple_x") ptr0 tuple_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"tuple_y") ptr0 tuple_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Tuple instance F.Storable Tuple

instance HsBindgen.Runtime.HasCField.HasCField Tuple "tuple_x" where

  type CFieldType Tuple "tuple_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "tuple_x" (Ptr.Ptr Tuple) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"tuple_x")

instance HsBindgen.Runtime.HasCField.HasCField Tuple "tuple_y" where

  type CFieldType Tuple "tuple_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty FC.CInt
         ) => GHC.Records.HasField "tuple_y" (Ptr.Ptr Tuple) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"tuple_y")

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/globals.h 81:13@

    __exported by:__ @manual\/globals.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
         ) => GHC.Records.HasField "unwrapTriplet" (Ptr.Ptr Triplet) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTriplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @list@

    __defined at:__ @manual\/globals.h 85:13@

    __exported by:__ @manual\/globals.h@
-}
newtype List = List
  { unwrapList :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance ( TyEq ty (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
         ) => GHC.Records.HasField "unwrapList" (Ptr.Ptr List) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapList")

instance HsBindgen.Runtime.HasCField.HasCField List "unwrapList" where

  type CFieldType List "unwrapList" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt

  offset# = \_ -> \_ -> 0
