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

module ModuleNames.Dash_name
    ( ModuleNames.Dash_name.Dash_name(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct dash_name@

    __defined at:__ @..\/dash-name.h 5:8@

    __exported by:__ @module_names.h@
-}
data Dash_name = Dash_name
  { dash_name_value :: BG.CUChar
    {- ^ __C declaration:__ @value@

         __defined at:__ @..\/dash-name.h 6:17@

         __exported by:__ @module_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Dash_name where

  staticSizeOf = \_ -> (1 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Dash_name where

  readRaw =
    \ptr0 ->
          pure Dash_name
      <*> HasCField.readRaw (BG.Proxy @"dash_name_value") ptr0

instance Marshal.WriteRaw Dash_name where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dash_name dash_name_value2 ->
            HasCField.writeRaw (BG.Proxy @"dash_name_value") ptr0 dash_name_value2

deriving via Marshal.EquivStorable Dash_name instance BG.Storable Dash_name

deriving via Struct.IsStructViaReadRaw Dash_name instance Struct.IsStruct Dash_name

{-| __C declaration:__ @value@

    __defined at:__ @..\/dash-name.h 6:17@

    __exported by:__ @module_names.h@
-}
instance ( ty ~ BG.CUChar
         ) => BG.CompatHasField.HasField "dash_name_value" Dash_name ty where

  hasField =
    \x0 ->
      (\y1 ->
         Dash_name {dash_name_value = y1}, BG.getField @"dash_name_value" x0)

instance ( ty ~ BG.CUChar
         ) => BG.HasField "dash_name_value" (BG.Ptr Dash_name) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dash_name_value")

instance HasCField.HasCField Dash_name "dash_name_value" where

  type CFieldType Dash_name "dash_name_value" =
    BG.CUChar

  offset# = \_ -> \_ -> 0
