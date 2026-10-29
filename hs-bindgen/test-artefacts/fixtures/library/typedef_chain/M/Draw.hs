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

module M.Draw
    ( M.Draw.Draw_status(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.Shape
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @draw_status@

    __defined at:__ @draw.h 5:22@

    __exported by:__ @draw.h@
-}
newtype Draw_status = Draw_status
  { unwrapDraw_status :: M.Shape.Shape_status
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

instance ( ty ~ M.Shape.Shape_status
         ) => BG.CompatHasField.HasField "unwrapDraw_status" Draw_status ty where

  hasField =
    \x0 ->
      ( \y1 -> Draw_status {unwrapDraw_status = y1}
      , BG.getField @"unwrapDraw_status" x0
      )

instance ( ty ~ M.Shape.Shape_status
         ) => BG.HasField "unwrapDraw_status" (BG.Ptr Draw_status) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapDraw_status")

instance HasCField.HasCField Draw_status "unwrapDraw_status" where

  type CFieldType Draw_status "unwrapDraw_status" =
    M.Shape.Shape_status

  offset# = \_ -> \_ -> 0
