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

module GeneratedSpecs.Same_header
    ( GeneratedSpecs.Same_header.Same_header(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct same_header@

    __defined at:__ @generated_specs\/same_header.h 5:16@

    __exported by:__ @generated_specs.h@
-}
data Same_header = Same_header
  { same_header_value :: BG.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @generated_specs\/same_header.h 6:10@

         __exported by:__ @generated_specs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Same_header where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Same_header where

  readRaw =
    \ptr0 ->
          pure Same_header
      <*> HasCField.readRaw (BG.Proxy @"same_header_value") ptr0

instance Marshal.WriteRaw Same_header where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Same_header same_header_value2 ->
            HasCField.writeRaw (BG.Proxy @"same_header_value") ptr0 same_header_value2

deriving via Marshal.EquivStorable Same_header instance BG.Storable Same_header

deriving via Struct.IsStructViaReadRaw Same_header instance Struct.IsStruct Same_header

{-| __C declaration:__ @value@

    __defined at:__ @generated_specs\/same_header.h 6:10@

    __exported by:__ @generated_specs.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "same_header_value" Same_header ty where

  hasField =
    \x0 ->
      ( \y1 -> Same_header {same_header_value = y1}
      , BG.getField @"same_header_value" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "same_header_value" (BG.Ptr Same_header) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"same_header_value")

instance HasCField.HasCField Same_header "same_header_value" where

  type CFieldType Same_header "same_header_value" =
    BG.CDouble

  offset# = \_ -> \_ -> 0
