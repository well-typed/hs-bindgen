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
    ( Example.U(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @macro U@

    __defined at:__ @program-analysis\/program-slicing\/typedef_unselected.h 11:9@

    __exported by:__ @program-analysis\/program-slicing\/typedef_unselected.h@
-}
newtype U = U
  { unwrapU :: BG.CInt
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

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unwrapU" U ty where

  hasField =
    \x0 ->
      (\y1 -> U {unwrapU = y1}, BG.getField @"unwrapU" x0)

instance (ty ~ BG.CInt) => BG.HasField "unwrapU" (BG.Ptr U) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapU")

instance HasCField.HasCField U "unwrapU" where

  type CFieldType U "unwrapU" = BG.CInt

  offset# = \_ -> \_ -> 0
