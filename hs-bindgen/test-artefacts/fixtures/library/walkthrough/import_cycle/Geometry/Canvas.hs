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

module Geometry.Canvas
    ( Geometry.Canvas.Canvas(..)
    )
  where

import qualified Geometry.Shape
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct canvas@

    __defined at:__ @geometry\/canvas.h 9:8@

    __exported by:__ @import_cycle.h@
-}
data Canvas = Canvas
  { canvas_width :: BG.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @geometry\/canvas.h 10:7@

         __exported by:__ @import_cycle.h@
    -}
  , canvas_height :: BG.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @geometry\/canvas.h 11:7@

         __exported by:__ @import_cycle.h@
    -}
  , canvas_selection :: BG.Ptr Geometry.Shape.Shape
    {- ^ __C declaration:__ @selection@

         __defined at:__ @geometry\/canvas.h 12:17@

         __exported by:__ @import_cycle.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Canvas where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Canvas where

  readRaw =
    \ptr0 ->
          pure Canvas
      <*> HasCField.readRaw (BG.Proxy @"canvas_width") ptr0
      <*> HasCField.readRaw (BG.Proxy @"canvas_height") ptr0
      <*> HasCField.readRaw (BG.Proxy @"canvas_selection") ptr0

instance Marshal.WriteRaw Canvas where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Canvas canvas_width2 canvas_height3 canvas_selection4 ->
               HasCField.writeRaw (BG.Proxy @"canvas_width") ptr0 canvas_width2
            >> HasCField.writeRaw (BG.Proxy @"canvas_height") ptr0 canvas_height3
            >> HasCField.writeRaw (BG.Proxy @"canvas_selection") ptr0 canvas_selection4

deriving via Marshal.EquivStorable Canvas instance BG.Storable Canvas

deriving via Struct.IsStructViaReadRaw Canvas instance Struct.IsStruct Canvas

{-| __C declaration:__ @width@

    __defined at:__ @geometry\/canvas.h 10:7@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "canvas_width" Canvas ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Canvas { canvas_width = y1
                 , canvas_height = BG.getField @"canvas_height" x0
                 , canvas_selection = BG.getField @"canvas_selection" x0
                 }
      , BG.getField @"canvas_width" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "canvas_width" (BG.Ptr Canvas) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"canvas_width")

instance HasCField.HasCField Canvas "canvas_width" where

  type CFieldType Canvas "canvas_width" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @height@

    __defined at:__ @geometry\/canvas.h 11:7@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "canvas_height" Canvas ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Canvas { canvas_height = y1
                 , canvas_width = BG.getField @"canvas_width" x0
                 , canvas_selection = BG.getField @"canvas_selection" x0
                 }
      , BG.getField @"canvas_height" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "canvas_height" (BG.Ptr Canvas) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"canvas_height")

instance HasCField.HasCField Canvas "canvas_height" where

  type CFieldType Canvas "canvas_height" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @selection@

    __defined at:__ @geometry\/canvas.h 12:17@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.Ptr Geometry.Shape.Shape
         ) => BG.CompatHasField.HasField "canvas_selection" Canvas ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Canvas { canvas_selection = y1
                 , canvas_width = BG.getField @"canvas_width" x0
                 , canvas_height = BG.getField @"canvas_height" x0
                 }
      , BG.getField @"canvas_selection" x0
      )

instance ( ty ~ BG.Ptr Geometry.Shape.Shape
         ) => BG.HasField "canvas_selection" (BG.Ptr Canvas) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"canvas_selection")

instance HasCField.HasCField Canvas "canvas_selection" where

  type CFieldType Canvas "canvas_selection" =
    BG.Ptr Geometry.Shape.Shape

  offset# = \_ -> \_ -> 8
