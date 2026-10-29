{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module M.Shape
    ( M.Shape.Shape_status(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.Point
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @shape_status@

    __defined at:__ @shape.h 5:16@

    __exported by:__ @draw.h@
-}
newtype Shape_status = Shape_status
  { unwrapShape_status :: M.Point.Status
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

instance ( ty ~ M.Point.Status
         ) => BG.CompatHasField.HasField "unwrapShape_status" Shape_status ty where

  hasField =
    \x0 ->
      ( \y1 -> Shape_status {unwrapShape_status = y1}
      , BG.getField @"unwrapShape_status" x0
      )

instance ( ty ~ M.Point.Status
         ) => BG.HasField "unwrapShape_status" (BG.Ptr Shape_status) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapShape_status")

instance HasCField.HasCField Shape_status "unwrapShape_status" where

  type CFieldType Shape_status "unwrapShape_status" =
    M.Point.Status

  offset# = \_ -> \_ -> 0
