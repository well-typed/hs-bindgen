{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
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

module Geometry.Scene
    ( Geometry.Scene.sCENE_MAX_SHAPES
    , Geometry.Scene.Scene(..)
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified Geometry.Layer
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @macro SCENE_MAX_SHAPES@

    __defined at:__ @geometry\/scene.h 7:9@

    __exported by:__ @import_cycle.h@
-}
sCENE_MAX_SHAPES :: BG.CInt
sCENE_MAX_SHAPES =
  (C.Expr.HostPlatform.*) Geometry.Layer.lAYER_MAX_SHAPES (8 :: BG.CInt)

{-| __C declaration:__ @struct scene@

    __defined at:__ @geometry\/scene.h 9:8@

    __exported by:__ @import_cycle.h@
-}
data Scene = Scene
  { scene_layers :: BG.Ptr Geometry.Layer.Layer
    {- ^ __C declaration:__ @layers@

         __defined at:__ @geometry\/scene.h 10:17@

         __exported by:__ @import_cycle.h@
    -}
  , scene_count :: BG.CInt
    {- ^ __C declaration:__ @count@

         __defined at:__ @geometry\/scene.h 11:7@

         __exported by:__ @import_cycle.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Scene where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Scene where

  readRaw =
    \ptr0 ->
          pure Scene
      <*> HasCField.readRaw (BG.Proxy @"scene_layers") ptr0
      <*> HasCField.readRaw (BG.Proxy @"scene_count") ptr0

instance Marshal.WriteRaw Scene where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Scene scene_layers2 scene_count3 ->
               HasCField.writeRaw (BG.Proxy @"scene_layers") ptr0 scene_layers2
            >> HasCField.writeRaw (BG.Proxy @"scene_count") ptr0 scene_count3

deriving via Marshal.EquivStorable Scene instance BG.Storable Scene

deriving via Struct.IsStructViaReadRaw Scene instance Struct.IsStruct Scene

{-| __C declaration:__ @layers@

    __defined at:__ @geometry\/scene.h 10:17@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.Ptr Geometry.Layer.Layer
         ) => BG.CompatHasField.HasField "scene_layers" Scene ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Scene {scene_layers = y1, scene_count = BG.getField @"scene_count" x0}
      , BG.getField @"scene_layers" x0
      )

instance ( ty ~ BG.Ptr Geometry.Layer.Layer
         ) => BG.HasField "scene_layers" (BG.Ptr Scene) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"scene_layers")

instance HasCField.HasCField Scene "scene_layers" where

  type CFieldType Scene "scene_layers" =
    BG.Ptr Geometry.Layer.Layer

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @count@

    __defined at:__ @geometry\/scene.h 11:7@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "scene_count" Scene ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Scene {scene_count = y1, scene_layers = BG.getField @"scene_layers" x0}
      , BG.getField @"scene_count" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "scene_count" (BG.Ptr Scene) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"scene_count")

instance HasCField.HasCField Scene "scene_count" where

  type CFieldType Scene "scene_count" = BG.CInt

  offset# = \_ -> \_ -> 8
