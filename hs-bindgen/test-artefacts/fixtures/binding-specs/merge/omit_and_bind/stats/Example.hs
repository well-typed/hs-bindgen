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

module Example
    ( Example.Stats(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified Lib.Operation
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct stats@

    __defined at:__ @binding-specs\/merge\/omit_and_bind\/stats.h 13:8@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
data Stats = Stats
  { stats_started :: Lib.Operation.Stopwatch
    {- ^ __C declaration:__ @started@

         __defined at:__ @binding-specs\/merge\/omit_and_bind\/stats.h 14:20@

         __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
    -}
  , stats_reads :: Lib.Operation.Operation
    {- ^ __C declaration:__ @reads@

         __defined at:__ @binding-specs\/merge\/omit_and_bind\/stats.h 15:20@

         __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
    -}
  , stats_writes :: Lib.Operation.Operation
    {- ^ __C declaration:__ @writes@

         __defined at:__ @binding-specs\/merge\/omit_and_bind\/stats.h 16:20@

         __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Stats where

  staticSizeOf = \_ -> (56 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Stats where

  readRaw =
    \ptr0 ->
          pure Stats
      <*> HasCField.readRaw (BG.Proxy @"stats_started") ptr0
      <*> HasCField.readRaw (BG.Proxy @"stats_reads") ptr0
      <*> HasCField.readRaw (BG.Proxy @"stats_writes") ptr0

instance Marshal.WriteRaw Stats where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Stats stats_started2 stats_reads3 stats_writes4 ->
               HasCField.writeRaw (BG.Proxy @"stats_started") ptr0 stats_started2
            >> HasCField.writeRaw (BG.Proxy @"stats_reads") ptr0 stats_reads3
            >> HasCField.writeRaw (BG.Proxy @"stats_writes") ptr0 stats_writes4

deriving via Marshal.EquivStorable Stats instance BG.Storable Stats

deriving via Struct.IsStructViaReadRaw Stats instance Struct.IsStruct Stats

{-| __C declaration:__ @started@

    __defined at:__ @binding-specs\/merge\/omit_and_bind\/stats.h 14:20@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
instance ( ty ~ Lib.Operation.Stopwatch
         ) => BG.CompatHasField.HasField "stats_started" Stats ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Stats { stats_started = y1
                , stats_reads = BG.getField @"stats_reads" x0
                , stats_writes = BG.getField @"stats_writes" x0
                }
      , BG.getField @"stats_started" x0
      )

instance ( ty ~ Lib.Operation.Stopwatch
         ) => BG.HasField "stats_started" (BG.Ptr Stats) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"stats_started")

instance HasCField.HasCField Stats "stats_started" where

  type CFieldType Stats "stats_started" =
    Lib.Operation.Stopwatch

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @reads@

    __defined at:__ @binding-specs\/merge\/omit_and_bind\/stats.h 15:20@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
instance ( ty ~ Lib.Operation.Operation
         ) => BG.CompatHasField.HasField "stats_reads" Stats ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Stats { stats_reads = y1
                , stats_started = BG.getField @"stats_started" x0
                , stats_writes = BG.getField @"stats_writes" x0
                }
      , BG.getField @"stats_reads" x0
      )

instance ( ty ~ Lib.Operation.Operation
         ) => BG.HasField "stats_reads" (BG.Ptr Stats) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"stats_reads")

instance HasCField.HasCField Stats "stats_reads" where

  type CFieldType Stats "stats_reads" =
    Lib.Operation.Operation

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @writes@

    __defined at:__ @binding-specs\/merge\/omit_and_bind\/stats.h 16:20@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
instance ( ty ~ Lib.Operation.Operation
         ) => BG.CompatHasField.HasField "stats_writes" Stats ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Stats { stats_writes = y1
                , stats_started = BG.getField @"stats_started" x0
                , stats_reads = BG.getField @"stats_reads" x0
                }
      , BG.getField @"stats_writes" x0
      )

instance ( ty ~ Lib.Operation.Operation
         ) => BG.HasField "stats_writes" (BG.Ptr Stats) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"stats_writes")

instance HasCField.HasCField Stats "stats_writes" where

  type CFieldType Stats "stats_writes" =
    Lib.Operation.Operation

  offset# = \_ -> \_ -> 32
