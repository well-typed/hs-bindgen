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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.FILE(..)
    , Example.Size_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct FILE@

    __defined at:__ @functions\/fun_attributes.h 7:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
data FILE = FILE
  {}
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize FILE where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw FILE where

  readRaw = \ptr0 -> pure FILE

instance Marshal.WriteRaw FILE where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE -> return ()

deriving via Marshal.EquivStorable FILE instance BG.Storable FILE

{-| __C declaration:__ @size_t@

    __defined at:__ @functions\/fun_attributes.h 8:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
newtype Size_t = Size_t
  { unwrapSize_t :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapSize_t" Size_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Size_t {unwrapSize_t = y1}, BG.getField @"unwrapSize_t" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapSize_t" (BG.Ptr Size_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSize_t")

instance HasCField.HasCField Size_t "unwrapSize_t" where

  type CFieldType Size_t "unwrapSize_t" = BG.CInt

  offset# = \_ -> \_ -> 0
