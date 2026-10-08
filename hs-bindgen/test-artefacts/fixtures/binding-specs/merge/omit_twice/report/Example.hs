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
    ( Example.Report(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified Lib.Counter
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct report@

    __defined at:__ @binding-specs\/merge\/omit_twice\/report.h 11:8@

    __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
-}
data Report = Report
  { report_reads :: Lib.Counter.Counter
    {- ^ __C declaration:__ @reads@

         __defined at:__ @binding-specs\/merge\/omit_twice\/report.h 12:18@

         __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
    -}
  , report_writes :: Lib.Counter.Counter
    {- ^ __C declaration:__ @writes@

         __defined at:__ @binding-specs\/merge\/omit_twice\/report.h 13:18@

         __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Report where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Report where

  readRaw =
    \ptr0 ->
          pure Report
      <*> HasCField.readRaw (BG.Proxy @"report_reads") ptr0
      <*> HasCField.readRaw (BG.Proxy @"report_writes") ptr0

instance Marshal.WriteRaw Report where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Report report_reads2 report_writes3 ->
               HasCField.writeRaw (BG.Proxy @"report_reads") ptr0 report_reads2
            >> HasCField.writeRaw (BG.Proxy @"report_writes") ptr0 report_writes3

deriving via Marshal.EquivStorable Report instance BG.Storable Report

deriving via Struct.IsStructViaReadRaw Report instance Struct.IsStruct Report

{-| __C declaration:__ @reads@

    __defined at:__ @binding-specs\/merge\/omit_twice\/report.h 12:18@

    __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
-}
instance ( ty ~ Lib.Counter.Counter
         ) => BG.CompatHasField.HasField "report_reads" Report ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Report {report_reads = y1, report_writes = BG.getField @"report_writes" x0}
      , BG.getField @"report_reads" x0
      )

instance ( ty ~ Lib.Counter.Counter
         ) => BG.HasField "report_reads" (BG.Ptr Report) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"report_reads")

instance HasCField.HasCField Report "report_reads" where

  type CFieldType Report "report_reads" =
    Lib.Counter.Counter

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @writes@

    __defined at:__ @binding-specs\/merge\/omit_twice\/report.h 13:18@

    __exported by:__ @binding-specs\/merge\/omit_twice\/report.h@
-}
instance ( ty ~ Lib.Counter.Counter
         ) => BG.CompatHasField.HasField "report_writes" Report ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Report {report_writes = y1, report_reads = BG.getField @"report_reads" x0}
      , BG.getField @"report_writes" x0
      )

instance ( ty ~ Lib.Counter.Counter
         ) => BG.HasField "report_writes" (BG.Ptr Report) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"report_writes")

instance HasCField.HasCField Report "report_writes" where

  type CFieldType Report "report_writes" =
    Lib.Counter.Counter

  offset# = \_ -> \_ -> 16
