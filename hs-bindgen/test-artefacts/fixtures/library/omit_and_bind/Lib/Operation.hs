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

module Lib.Operation
    ( Lib.Operation.Stopwatch(..)
    , Lib.Operation.Operation(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified Lib.Stopwatch
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct stopwatch@

    __defined at:__ @stopwatch.h 10:8@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
data Stopwatch = Stopwatch
  { stopwatch_ticks :: BG.CULLong
    {- ^ __C declaration:__ @ticks@

         __defined at:__ @stopwatch.h 11:22@

         __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Stopwatch where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Stopwatch where

  readRaw =
    \ptr0 ->
          pure Stopwatch
      <*> HasCField.readRaw (BG.Proxy @"stopwatch_ticks") ptr0

instance Marshal.WriteRaw Stopwatch where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Stopwatch stopwatch_ticks2 ->
            HasCField.writeRaw (BG.Proxy @"stopwatch_ticks") ptr0 stopwatch_ticks2

deriving via Marshal.EquivStorable Stopwatch instance BG.Storable Stopwatch

deriving via Struct.IsStructViaReadRaw Stopwatch instance Struct.IsStruct Stopwatch

{-| __C declaration:__ @ticks@

    __defined at:__ @stopwatch.h 11:22@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
instance ( ty ~ BG.CULLong
         ) => BG.CompatHasField.HasField "stopwatch_ticks" Stopwatch ty where

  hasField =
    \x0 ->
      (\y1 ->
         Stopwatch {stopwatch_ticks = y1}, BG.getField @"stopwatch_ticks" x0)

instance ( ty ~ BG.CULLong
         ) => BG.HasField "stopwatch_ticks" (BG.Ptr Stopwatch) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"stopwatch_ticks")

instance HasCField.HasCField Stopwatch "stopwatch_ticks" where

  type CFieldType Stopwatch "stopwatch_ticks" =
    BG.CULLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct operation@

    __defined at:__ @operation.h 7:8@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
data Operation = Operation
  { operation_begin :: Stopwatch
    {- ^ __C declaration:__ @begin@

         __defined at:__ @operation.h 8:20@

         __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
    -}
  , operation_count :: BG.CInt
    {- ^ __C declaration:__ @count@

         __defined at:__ @operation.h 9:7@

         __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
    -}
  , operation_total :: Lib.Stopwatch.Stopwatch_time
    {- ^ __C declaration:__ @total@

         __defined at:__ @operation.h 10:18@

         __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Operation where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Operation where

  readRaw =
    \ptr0 ->
          pure Operation
      <*> HasCField.readRaw (BG.Proxy @"operation_begin") ptr0
      <*> HasCField.readRaw (BG.Proxy @"operation_count") ptr0
      <*> HasCField.readRaw (BG.Proxy @"operation_total") ptr0

instance Marshal.WriteRaw Operation where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Operation operation_begin2 operation_count3 operation_total4 ->
               HasCField.writeRaw (BG.Proxy @"operation_begin") ptr0 operation_begin2
            >> HasCField.writeRaw (BG.Proxy @"operation_count") ptr0 operation_count3
            >> HasCField.writeRaw (BG.Proxy @"operation_total") ptr0 operation_total4

deriving via Marshal.EquivStorable Operation instance BG.Storable Operation

deriving via Struct.IsStructViaReadRaw Operation instance Struct.IsStruct Operation

{-| __C declaration:__ @begin@

    __defined at:__ @operation.h 8:20@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
instance ( ty ~ Stopwatch
         ) => BG.CompatHasField.HasField "operation_begin" Operation ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Operation { operation_begin = y1
                    , operation_count = BG.getField @"operation_count" x0
                    , operation_total = BG.getField @"operation_total" x0
                    }
      , BG.getField @"operation_begin" x0
      )

instance ( ty ~ Stopwatch
         ) => BG.HasField "operation_begin" (BG.Ptr Operation) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"operation_begin")

instance HasCField.HasCField Operation "operation_begin" where

  type CFieldType Operation "operation_begin" =
    Stopwatch

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @count@

    __defined at:__ @operation.h 9:7@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "operation_count" Operation ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Operation { operation_count = y1
                    , operation_begin = BG.getField @"operation_begin" x0
                    , operation_total = BG.getField @"operation_total" x0
                    }
      , BG.getField @"operation_count" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "operation_count" (BG.Ptr Operation) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"operation_count")

instance HasCField.HasCField Operation "operation_count" where

  type CFieldType Operation "operation_count" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @total@

    __defined at:__ @operation.h 10:18@

    __exported by:__ @binding-specs\/merge\/omit_and_bind\/stats.h@
-}
instance ( ty ~ Lib.Stopwatch.Stopwatch_time
         ) => BG.CompatHasField.HasField "operation_total" Operation ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Operation { operation_total = y1
                    , operation_begin = BG.getField @"operation_begin" x0
                    , operation_count = BG.getField @"operation_count" x0
                    }
      , BG.getField @"operation_total" x0
      )

instance ( ty ~ Lib.Stopwatch.Stopwatch_time
         ) => BG.HasField "operation_total" (BG.Ptr Operation) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"operation_total")

instance HasCField.HasCField Operation "operation_total" where

  type CFieldType Operation "operation_total" =
    Lib.Stopwatch.Stopwatch_time

  offset# = \_ -> \_ -> 16
