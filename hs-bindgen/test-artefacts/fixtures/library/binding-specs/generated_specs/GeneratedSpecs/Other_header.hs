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

module GeneratedSpecs.Other_header
    ( GeneratedSpecs.Other_header.Other_header(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct other_header@

    __defined at:__ @generated_specs\/other_header.h 6:8@

    __exported by:__ @generated_specs.h@
-}
data Other_header = Other_header
  { other_header_value :: BG.CLong
    {- ^ __C declaration:__ @value@

         __defined at:__ @generated_specs\/other_header.h 7:8@

         __exported by:__ @generated_specs.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Other_header where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Other_header where

  readRaw =
    \ptr0 ->
          pure Other_header
      <*> HasCField.readRaw (BG.Proxy @"other_header_value") ptr0

instance Marshal.WriteRaw Other_header where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Other_header other_header_value2 ->
            HasCField.writeRaw (BG.Proxy @"other_header_value") ptr0 other_header_value2

deriving via Marshal.EquivStorable Other_header instance BG.Storable Other_header

deriving via Struct.IsStructViaReadRaw Other_header instance Struct.IsStruct Other_header

{-| __C declaration:__ @value@

    __defined at:__ @generated_specs\/other_header.h 7:8@

    __exported by:__ @generated_specs.h@
-}
instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "other_header_value" Other_header ty where

  hasField =
    \x0 ->
      ( \y1 -> Other_header {other_header_value = y1}
      , BG.getField @"other_header_value" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "other_header_value" (BG.Ptr Other_header) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"other_header_value")

instance HasCField.HasCField Other_header "other_header_value" where

  type CFieldType Other_header "other_header_value" =
    BG.CLong

  offset# = \_ -> \_ -> 0
