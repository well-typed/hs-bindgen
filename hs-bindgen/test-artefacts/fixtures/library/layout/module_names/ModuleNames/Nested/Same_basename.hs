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

module ModuleNames.Nested.Same_basename
    ( ModuleNames.Nested.Same_basename.Same_basename(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified ModuleNames.Dash_name
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct same_basename@

    __defined at:__ @module_names\/nested\/same_basename.h 8:8@

    __exported by:__ @module_names.h@
-}
data Same_basename = Same_basename
  { same_basename_width :: BG.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @module_names\/nested\/same_basename.h 9:7@

         __exported by:__ @module_names.h@
    -}
  , same_basename_colour :: ModuleNames.Dash_name.Dash_name
    {- ^ __C declaration:__ @colour@

         __defined at:__ @module_names\/nested\/same_basename.h 10:20@

         __exported by:__ @module_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Same_basename where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Same_basename where

  readRaw =
    \ptr0 ->
          pure Same_basename
      <*> HasCField.readRaw (BG.Proxy @"same_basename_width") ptr0
      <*> HasCField.readRaw (BG.Proxy @"same_basename_colour") ptr0

instance Marshal.WriteRaw Same_basename where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Same_basename same_basename_width2 same_basename_colour3 ->
               HasCField.writeRaw (BG.Proxy @"same_basename_width") ptr0 same_basename_width2
            >> HasCField.writeRaw (BG.Proxy @"same_basename_colour") ptr0 same_basename_colour3

deriving via Marshal.EquivStorable Same_basename instance BG.Storable Same_basename

deriving via Struct.IsStructViaReadRaw Same_basename instance Struct.IsStruct Same_basename

{-| __C declaration:__ @width@

    __defined at:__ @module_names\/nested\/same_basename.h 9:7@

    __exported by:__ @module_names.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "same_basename_width" Same_basename ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Same_basename { same_basename_width = y1
                        , same_basename_colour = BG.getField @"same_basename_colour" x0
                        }
      , BG.getField @"same_basename_width" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "same_basename_width" (BG.Ptr Same_basename) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"same_basename_width")

instance HasCField.HasCField Same_basename "same_basename_width" where

  type CFieldType Same_basename "same_basename_width" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @colour@

    __defined at:__ @module_names\/nested\/same_basename.h 10:20@

    __exported by:__ @module_names.h@
-}
instance ( ty ~ ModuleNames.Dash_name.Dash_name
         ) => BG.CompatHasField.HasField "same_basename_colour" Same_basename ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Same_basename { same_basename_colour = y1
                        , same_basename_width = BG.getField @"same_basename_width" x0
                        }
      , BG.getField @"same_basename_colour" x0
      )

instance ( ty ~ ModuleNames.Dash_name.Dash_name
         ) => BG.HasField "same_basename_colour" (BG.Ptr Same_basename) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"same_basename_colour")

instance HasCField.HasCField Same_basename "same_basename_colour" where

  type CFieldType Same_basename "same_basename_colour" =
    ModuleNames.Dash_name.Dash_name

  offset# = \_ -> \_ -> 4
