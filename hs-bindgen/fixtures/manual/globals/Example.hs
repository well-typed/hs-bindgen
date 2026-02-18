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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct globalConfig@

    __defined at:__ @manual\/globals.h 7:9@

    __exported by:__ @manual\/globals.h@
-}
data GlobalConfig = GlobalConfig
  { globalConfig_numThreads :: RIP.CInt
    {- ^ __C declaration:__ @numThreads@

         __defined at:__ @manual\/globals.h 8:7@

         __exported by:__ @manual\/globals.h@
    -}
  , globalConfig_numWorkers :: RIP.CInt
    {- ^ __C declaration:__ @numWorkers@

         __defined at:__ @manual\/globals.h 9:7@

         __exported by:__ @manual\/globals.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize GlobalConfig where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw GlobalConfig where

  readRaw =
    \ptr0 ->
          pure GlobalConfig
      <*> HasCField.readRaw (RIP.Proxy @"globalConfig_numThreads") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"globalConfig_numWorkers") ptr0

instance Marshal.WriteRaw GlobalConfig where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          GlobalConfig globalConfig_numThreads2 globalConfig_numWorkers3 ->
               HasCField.writeRaw (RIP.Proxy @"globalConfig_numThreads") ptr0 globalConfig_numThreads2
            >> HasCField.writeRaw (RIP.Proxy @"globalConfig_numWorkers") ptr0 globalConfig_numWorkers3

deriving via Marshal.EquivStorable GlobalConfig instance RIP.Storable GlobalConfig

instance HasCField.HasCField GlobalConfig "globalConfig_numThreads" where

  type CFieldType GlobalConfig "globalConfig_numThreads" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "globalConfig_numThreads" (RIP.Ptr GlobalConfig) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"globalConfig_numThreads")

instance HasCField.HasCField GlobalConfig "globalConfig_numWorkers" where

  type CFieldType GlobalConfig "globalConfig_numWorkers" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "globalConfig_numWorkers" (RIP.Ptr GlobalConfig) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"globalConfig_numWorkers")

{-| __C declaration:__ @ConstInt@

    __defined at:__ @manual\/globals.h 40:19@

    __exported by:__ @manual\/globals.h@
-}
newtype ConstInt = ConstInt
  { unwrapConstInt :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapConstInt" (RIP.Ptr ConstInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConstInt")

instance HasCField.HasCField ConstInt "unwrapConstInt" where

  type CFieldType ConstInt "unwrapConstInt" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct tuple@

    __defined at:__ @manual\/globals.h 52:8@

    __exported by:__ @manual\/globals.h@
-}
data Tuple = Tuple
  { tuple_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/globals.h 52:20@

         __exported by:__ @manual\/globals.h@
    -}
  , tuple_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/globals.h 52:33@

         __exported by:__ @manual\/globals.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Tuple where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Tuple where

  readRaw =
    \ptr0 ->
          pure Tuple
      <*> HasCField.readRaw (RIP.Proxy @"tuple_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"tuple_y") ptr0

instance Marshal.WriteRaw Tuple where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tuple tuple_x2 tuple_y3 ->
               HasCField.writeRaw (RIP.Proxy @"tuple_x") ptr0 tuple_x2
            >> HasCField.writeRaw (RIP.Proxy @"tuple_y") ptr0 tuple_y3

deriving via Marshal.EquivStorable Tuple instance RIP.Storable Tuple

instance HasCField.HasCField Tuple "tuple_x" where

  type CFieldType Tuple "tuple_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "tuple_x" (RIP.Ptr Tuple) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"tuple_x")

instance HasCField.HasCField Tuple "tuple_y" where

  type CFieldType Tuple "tuple_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "tuple_y" (RIP.Ptr Tuple) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"tuple_y")

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/globals.h 81:13@

    __exported by:__ @manual\/globals.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapTriplet" (RIP.Ptr Triplet) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTriplet")

instance HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @list@

    __defined at:__ @manual\/globals.h 85:13@

    __exported by:__ @manual\/globals.h@
-}
newtype List = List
  { unwrapList :: IA.IncompleteArray RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)

instance ( ((~) ty) (IA.IncompleteArray RIP.CInt)
         ) => RIP.HasField "unwrapList" (RIP.Ptr List) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapList")

instance HasCField.HasCField List "unwrapList" where

  type CFieldType List "unwrapList" =
    IA.IncompleteArray RIP.CInt

  offset# = \_ -> \_ -> 0
