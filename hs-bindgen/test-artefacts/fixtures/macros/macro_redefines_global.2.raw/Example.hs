{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.FILE(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @FILE@

    __defined at:__ @macros\/macro_redefines_global.h 7:13@

    __exported by:__ @macros\/macro_redefines_global.h@
-}
newtype FILE = FILE
  { unwrapFILE :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapFILE" FILE ty where

  hasField =
    \x0 ->
      (\y1 ->
         FILE {unwrapFILE = y1}, BG.getField @"unwrapFILE" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapFILE" (BG.Ptr FILE) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapFILE")

instance HasCField.HasCField FILE "unwrapFILE" where

  type CFieldType FILE "unwrapFILE" = BG.CInt

  offset# = \_ -> \_ -> 0
