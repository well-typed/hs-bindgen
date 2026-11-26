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

module SquashedIntoUnusable.Unusable
    ( SquashedIntoUnusable.Unusable.Kept(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct kept@

    __defined at:__ @squashed_into_unusable\/unusable.h 12:8@

    __exported by:__ @squashed_into_unusable.h@
-}
data Kept = Kept
  { kept_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @squashed_into_unusable\/unusable.h 13:7@

         __exported by:__ @squashed_into_unusable.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Kept where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Kept where

  readRaw =
    \ptr0 ->
          pure Kept
      <*> HasCField.readRaw (BG.Proxy @"kept_x") ptr0

instance Marshal.WriteRaw Kept where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Kept kept_x2 ->
            HasCField.writeRaw (BG.Proxy @"kept_x") ptr0 kept_x2

deriving via Marshal.EquivStorable Kept instance BG.Storable Kept

deriving via Struct.IsStructViaReadRaw Kept instance Struct.IsStruct Kept

{-| __C declaration:__ @x@

    __defined at:__ @squashed_into_unusable\/unusable.h 13:7@

    __exported by:__ @squashed_into_unusable.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "kept_x" Kept ty where

  hasField =
    \x0 ->
      (\y1 -> Kept {kept_x = y1}, BG.getField @"kept_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "kept_x" (BG.Ptr Kept) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"kept_x")

instance HasCField.HasCField Kept "kept_x" where

  type CFieldType Kept "kept_x" = BG.CInt

  offset# = \_ -> \_ -> 0
