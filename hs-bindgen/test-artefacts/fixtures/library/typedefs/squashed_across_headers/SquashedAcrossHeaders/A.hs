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

module SquashedAcrossHeaders.A
    ( SquashedAcrossHeaders.A.A(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified SquashedAcrossHeaders.B
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct a@

    __defined at:__ @squashed_across_headers\/a.h 8:8@

    __exported by:__ @squashed_across_headers.h@
-}
data A = A
  { a_peer :: BG.Ptr SquashedAcrossHeaders.B.B
    {- ^ __C declaration:__ @peer@

         __defined at:__ @squashed_across_headers\/a.h 9:6@

         __exported by:__ @squashed_across_headers.h@
    -}
  , a_value :: BG.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @squashed_across_headers\/a.h 10:7@

         __exported by:__ @squashed_across_headers.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (BG.Proxy @"a_peer") ptr0
      <*> HasCField.readRaw (BG.Proxy @"a_value") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_peer2 a_value3 ->
               HasCField.writeRaw (BG.Proxy @"a_peer") ptr0 a_peer2
            >> HasCField.writeRaw (BG.Proxy @"a_value") ptr0 a_value3

deriving via Marshal.EquivStorable A instance BG.Storable A

deriving via Struct.IsStructViaReadRaw A instance Struct.IsStruct A

{-| __C declaration:__ @peer@

    __defined at:__ @squashed_across_headers\/a.h 9:6@

    __exported by:__ @squashed_across_headers.h@
-}
instance ( ty ~ BG.Ptr SquashedAcrossHeaders.B.B
         ) => BG.CompatHasField.HasField "a_peer" A ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A {a_peer = y1, a_value = BG.getField @"a_value" x0}
      , BG.getField @"a_peer" x0
      )

instance ( ty ~ BG.Ptr SquashedAcrossHeaders.B.B
         ) => BG.HasField "a_peer" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_peer")

instance HasCField.HasCField A "a_peer" where

  type CFieldType A "a_peer" =
    BG.Ptr SquashedAcrossHeaders.B.B

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @value@

    __defined at:__ @squashed_across_headers\/a.h 10:7@

    __exported by:__ @squashed_across_headers.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "a_value" A ty where

  hasField =
    \x0 ->
      ( \y1 ->
          A {a_value = y1, a_peer = BG.getField @"a_peer" x0}
      , BG.getField @"a_value" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "a_value" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_value")

instance HasCField.HasCField A "a_value" where

  type CFieldType A "a_value" = BG.CInt

  offset# = \_ -> \_ -> 8
