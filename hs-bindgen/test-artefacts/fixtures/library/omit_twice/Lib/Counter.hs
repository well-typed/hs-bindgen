{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Counter
    ( Lib.Counter.Counter(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified Lib.Stopwatch
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct counter@

    __defined at:__ @counter.h 7:8@

    __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
-}
data Counter = Counter
  { counter_count :: BG.CInt
    {- ^ __C declaration:__ @count@

         __defined at:__ @counter.h 8:7@

         __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
    -}
  , counter_total :: Lib.Stopwatch.Stopwatch_time
    {- ^ __C declaration:__ @total@

         __defined at:__ @counter.h 9:18@

         __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Counter where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Counter where

  readRaw =
    \ptr0 ->
          pure Counter
      <*> HasCField.readRaw (BG.Proxy @"counter_count") ptr0
      <*> HasCField.readRaw (BG.Proxy @"counter_total") ptr0

instance Marshal.WriteRaw Counter where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Counter counter_count2 counter_total3 ->
               HasCField.writeRaw (BG.Proxy @"counter_count") ptr0 counter_count2
            >> HasCField.writeRaw (BG.Proxy @"counter_total") ptr0 counter_total3

deriving via Marshal.EquivStorable Counter instance BG.Storable Counter

deriving via Struct.IsStructViaReadRaw Counter instance Struct.IsStruct Counter

{-| __C declaration:__ @count@

    __defined at:__ @counter.h 8:7@

    __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "counter_count" Counter ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Counter {counter_count = y1, counter_total = BG.getField @"counter_total" x0}
      , BG.getField @"counter_count" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "counter_count" (BG.Ptr Counter) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"counter_count")

instance HasCField.HasCField Counter "counter_count" where

  type CFieldType Counter "counter_count" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @total@

    __defined at:__ @counter.h 9:18@

    __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
-}
instance ( ty ~ Lib.Stopwatch.Stopwatch_time
         ) => BG.CompatHasField.HasField "counter_total" Counter ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Counter {counter_total = y1, counter_count = BG.getField @"counter_count" x0}
      , BG.getField @"counter_total" x0
      )

instance ( ty ~ Lib.Stopwatch.Stopwatch_time
         ) => BG.HasField "counter_total" (BG.Ptr Counter) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"counter_total")

instance HasCField.HasCField Counter "counter_total" where

  type CFieldType Counter "counter_total" =
    Lib.Stopwatch.Stopwatch_time

  offset# = \_ -> \_ -> 8
