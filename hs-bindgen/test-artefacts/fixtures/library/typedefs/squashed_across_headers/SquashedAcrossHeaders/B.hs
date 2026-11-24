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

module SquashedAcrossHeaders.B
    ( SquashedAcrossHeaders.B.B(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified SquashedAcrossHeaders.A
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct b@

    __defined at:__ @squashed_across_headers\/b.h 6:8@

    __exported by:__ @squashed_across_headers.h@
-}
data B = B
  { b_peer :: BG.Ptr SquashedAcrossHeaders.A.A
    {- ^ __C declaration:__ @peer@

         __defined at:__ @squashed_across_headers\/b.h 7:6@

         __exported by:__ @squashed_across_headers.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HasCField.readRaw (BG.Proxy @"b_peer") ptr0

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_peer2 ->
            HasCField.writeRaw (BG.Proxy @"b_peer") ptr0 b_peer2

deriving via Marshal.EquivStorable B instance BG.Storable B

deriving via Struct.IsStructViaReadRaw B instance Struct.IsStruct B

{-| __C declaration:__ @peer@

    __defined at:__ @squashed_across_headers\/b.h 7:6@

    __exported by:__ @squashed_across_headers.h@
-}
instance ( ty ~ BG.Ptr SquashedAcrossHeaders.A.A
         ) => BG.CompatHasField.HasField "b_peer" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {b_peer = y1}, BG.getField @"b_peer" x0)

instance ( ty ~ BG.Ptr SquashedAcrossHeaders.A.A
         ) => BG.HasField "b_peer" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b_peer")

instance HasCField.HasCField B "b_peer" where

  type CFieldType B "b_peer" =
    BG.Ptr SquashedAcrossHeaders.A.A

  offset# = \_ -> \_ -> 0
