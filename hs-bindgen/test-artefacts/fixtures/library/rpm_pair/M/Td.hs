{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module M.Td
    ( M.Td.Rpm_tag(..)
    , M.Td.Rpmtd(..)
    , M.Td.Rpmtd_s(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @rpm_tag@

    __defined at:__ @types.h 6:13@

    __exported by:__ @rpm_pair\/td.h@
-}
newtype Rpm_tag = Rpm_tag
  { unwrapRpm_tag :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapRpm_tag" Rpm_tag ty where

  hasField =
    \x0 ->
      (\y1 ->
         Rpm_tag {unwrapRpm_tag = y1}, BG.getField @"unwrapRpm_tag" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapRpm_tag" (BG.Ptr Rpm_tag) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapRpm_tag")

instance HasCField.HasCField Rpm_tag "unwrapRpm_tag" where

  type CFieldType Rpm_tag "unwrapRpm_tag" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @rpmtd@

    __defined at:__ @types.h 8:25@

    __exported by:__ @rpm_pair\/td.h@
-}
newtype Rpmtd = Rpmtd
  { unwrapRpmtd :: BG.Ptr Rpmtd_s
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr Rpmtd_s
         ) => BG.CompatHasField.HasField "unwrapRpmtd" Rpmtd ty where

  hasField =
    \x0 ->
      (\y1 ->
         Rpmtd {unwrapRpmtd = y1}, BG.getField @"unwrapRpmtd" x0)

instance ( ty ~ BG.Ptr Rpmtd_s
         ) => BG.HasField "unwrapRpmtd" (BG.Ptr Rpmtd) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapRpmtd")

instance HasCField.HasCField Rpmtd "unwrapRpmtd" where

  type CFieldType Rpmtd "unwrapRpmtd" = BG.Ptr Rpmtd_s

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct rpmtd_s@

    __defined at:__ @rpm_pair\/td.h 8:8@

    __exported by:__ @rpm_pair\/td.h@
-}
data Rpmtd_s = Rpmtd_s
  { rpmtd_s_tag :: Rpm_tag
    {- ^ __C declaration:__ @tag@

         __defined at:__ @rpm_pair\/td.h 9:11@

         __exported by:__ @rpm_pair\/td.h@
    -}
  , rpmtd_s_count :: BG.CInt
    {- ^ __C declaration:__ @count@

         __defined at:__ @rpm_pair\/td.h 10:7@

         __exported by:__ @rpm_pair\/td.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Rpmtd_s where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Rpmtd_s where

  readRaw =
    \ptr0 ->
          pure Rpmtd_s
      <*> HasCField.readRaw (BG.Proxy @"rpmtd_s_tag") ptr0
      <*> HasCField.readRaw (BG.Proxy @"rpmtd_s_count") ptr0

instance Marshal.WriteRaw Rpmtd_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rpmtd_s rpmtd_s_tag2 rpmtd_s_count3 ->
               HasCField.writeRaw (BG.Proxy @"rpmtd_s_tag") ptr0 rpmtd_s_tag2
            >> HasCField.writeRaw (BG.Proxy @"rpmtd_s_count") ptr0 rpmtd_s_count3

deriving via Marshal.EquivStorable Rpmtd_s instance BG.Storable Rpmtd_s

deriving via Struct.IsStructViaReadRaw Rpmtd_s instance Struct.IsStruct Rpmtd_s

{-| __C declaration:__ @tag@

    __defined at:__ @rpm_pair\/td.h 9:11@

    __exported by:__ @rpm_pair\/td.h@
-}
instance ( ty ~ Rpm_tag
         ) => BG.CompatHasField.HasField "rpmtd_s_tag" Rpmtd_s ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rpmtd_s {rpmtd_s_tag = y1, rpmtd_s_count = BG.getField @"rpmtd_s_count" x0}
      , BG.getField @"rpmtd_s_tag" x0
      )

instance ( ty ~ Rpm_tag
         ) => BG.HasField "rpmtd_s_tag" (BG.Ptr Rpmtd_s) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rpmtd_s_tag")

instance HasCField.HasCField Rpmtd_s "rpmtd_s_tag" where

  type CFieldType Rpmtd_s "rpmtd_s_tag" = Rpm_tag

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @count@

    __defined at:__ @rpm_pair\/td.h 10:7@

    __exported by:__ @rpm_pair\/td.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "rpmtd_s_count" Rpmtd_s ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rpmtd_s {rpmtd_s_count = y1, rpmtd_s_tag = BG.getField @"rpmtd_s_tag" x0}
      , BG.getField @"rpmtd_s_count" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "rpmtd_s_count" (BG.Ptr Rpmtd_s) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rpmtd_s_count")

instance HasCField.HasCField Rpmtd_s "rpmtd_s_count" where

  type CFieldType Rpmtd_s "rpmtd_s_count" = BG.CInt

  offset# = \_ -> \_ -> 4
