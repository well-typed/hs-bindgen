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

module Lib.Ui.Point
    ( Lib.Ui.Point.Point(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Enum, Eq, Floating, Fractional, Num, Ord, Read, Real, RealFloat, RealFrac, Show, type (~))

{-| __C declaration:__ @Point@

    __defined at:__ @ui\/point.h 4:16@

    __exported by:__ @root.h@
-}
newtype Point = Point
  { unwrapPoint :: BG.CDouble
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( Enum
    , Floating
    , Fractional
    , BG.HasFFIType
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , RealFloat
    , RealFrac
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "unwrapPoint" Point ty where

  hasField =
    \x0 ->
      (\y1 ->
         Point {unwrapPoint = y1}, BG.getField @"unwrapPoint" x0)

instance ( ty ~ BG.CDouble
         ) => BG.HasField "unwrapPoint" (BG.Ptr Point) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPoint")

instance HasCField.HasCField Point "unwrapPoint" where

  type CFieldType Point "unwrapPoint" = BG.CDouble

  offset# = \_ -> \_ -> 0
