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

module Lib.Stopwatch
    ( Lib.Stopwatch.Stopwatch_time(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, type (~))

{-| __C declaration:__ @stopwatch_time@

    __defined at:__ @stopwatch.h 6:23@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
newtype Stopwatch_time = Stopwatch_time
  { unwrapStopwatch_time :: BG.CULong
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

instance ( ty ~ BG.CULong
         ) => BG.CompatHasField.HasField "unwrapStopwatch_time" Stopwatch_time ty where

  hasField =
    \x0 ->
      ( \y1 -> Stopwatch_time {unwrapStopwatch_time = y1}
      , BG.getField @"unwrapStopwatch_time" x0
      )

instance ( ty ~ BG.CULong
         ) => BG.HasField "unwrapStopwatch_time" (BG.Ptr Stopwatch_time) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStopwatch_time")

instance HasCField.HasCField Stopwatch_time "unwrapStopwatch_time" where

  type CFieldType Stopwatch_time "unwrapStopwatch_time" =
    BG.CULong

  offset# = \_ -> \_ -> 0
