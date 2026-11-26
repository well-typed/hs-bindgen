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

module ExternalSpecs.Uses_external
    ( ExternalSpecs.Uses_external.Uses_external(..)
    )
  where

import qualified GeneratedSpecs.Other_header
import qualified GeneratedSpecs.Same_header
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct uses_external@

    __defined at:__ @external_specs\/uses_external.h 10:8@

    __exported by:__ @external_specs.h@
-}
data Uses_external = Uses_external
  { uses_external_a :: GeneratedSpecs.Same_header.Same_header
    {- ^ __C declaration:__ @a@

         __defined at:__ @external_specs\/uses_external.h 11:15@

         __exported by:__ @external_specs.h@
    -}
  , uses_external_b :: GeneratedSpecs.Other_header.Other_header
    {- ^ __C declaration:__ @b@

         __defined at:__ @external_specs\/uses_external.h 12:16@

         __exported by:__ @external_specs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Uses_external where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Uses_external where

  readRaw =
    \ptr0 ->
          pure Uses_external
      <*> HasCField.readRaw (BG.Proxy @"uses_external_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"uses_external_b") ptr0

instance Marshal.WriteRaw Uses_external where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Uses_external uses_external_a2 uses_external_b3 ->
               HasCField.writeRaw (BG.Proxy @"uses_external_a") ptr0 uses_external_a2
            >> HasCField.writeRaw (BG.Proxy @"uses_external_b") ptr0 uses_external_b3

deriving via Marshal.EquivStorable Uses_external instance BG.Storable Uses_external

deriving via Struct.IsStructViaReadRaw Uses_external instance Struct.IsStruct Uses_external

{-| __C declaration:__ @a@

    __defined at:__ @external_specs\/uses_external.h 11:15@

    __exported by:__ @external_specs.h@
-}
instance ( ty ~ GeneratedSpecs.Same_header.Same_header
         ) => BG.CompatHasField.HasField "uses_external_a" Uses_external ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Uses_external {uses_external_a = y1, uses_external_b = BG.getField @"uses_external_b" x0}
      , BG.getField @"uses_external_a" x0
      )

instance ( ty ~ GeneratedSpecs.Same_header.Same_header
         ) => BG.HasField "uses_external_a" (BG.Ptr Uses_external) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uses_external_a")

instance HasCField.HasCField Uses_external "uses_external_a" where

  type CFieldType Uses_external "uses_external_a" =
    GeneratedSpecs.Same_header.Same_header

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @external_specs\/uses_external.h 12:16@

    __exported by:__ @external_specs.h@
-}
instance ( ty ~ GeneratedSpecs.Other_header.Other_header
         ) => BG.CompatHasField.HasField "uses_external_b" Uses_external ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Uses_external {uses_external_b = y1, uses_external_a = BG.getField @"uses_external_a" x0}
      , BG.getField @"uses_external_b" x0
      )

instance ( ty ~ GeneratedSpecs.Other_header.Other_header
         ) => BG.HasField "uses_external_b" (BG.Ptr Uses_external) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uses_external_b")

instance HasCField.HasCField Uses_external "uses_external_b" where

  type CFieldType Uses_external "uses_external_b" =
    GeneratedSpecs.Other_header.Other_header

  offset# = \_ -> \_ -> 8
