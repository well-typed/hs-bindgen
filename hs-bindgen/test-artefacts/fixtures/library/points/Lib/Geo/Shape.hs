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

module Lib.Geo.Shape
    ( Lib.Geo.Shape.Shape(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified Lib.Geo.Point
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct shape@

    __defined at:__ @geo\/shape.h 5:8@

    __exported by:__ @root.h@
-}
data Shape = Shape
  { shape_origin :: Lib.Geo.Point.Point
    {- ^ __C declaration:__ @origin@

         __defined at:__ @geo\/shape.h 5:29@

         __exported by:__ @root.h@
    -}
  , shape_next :: BG.Ptr Shape
    {- ^ __C declaration:__ @next@

         __defined at:__ @geo\/shape.h 5:51@

         __exported by:__ @root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Shape where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Shape where

  readRaw =
    \ptr0 ->
          pure Shape
      <*> HasCField.readRaw (BG.Proxy @"shape_origin") ptr0
      <*> HasCField.readRaw (BG.Proxy @"shape_next") ptr0

instance Marshal.WriteRaw Shape where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Shape shape_origin2 shape_next3 ->
               HasCField.writeRaw (BG.Proxy @"shape_origin") ptr0 shape_origin2
            >> HasCField.writeRaw (BG.Proxy @"shape_next") ptr0 shape_next3

deriving via Marshal.EquivStorable Shape instance BG.Storable Shape

deriving via Struct.IsStructViaReadRaw Shape instance Struct.IsStruct Shape

{-| __C declaration:__ @origin@

    __defined at:__ @geo\/shape.h 5:29@

    __exported by:__ @root.h@
-}
instance ( ty ~ Lib.Geo.Point.Point
         ) => BG.CompatHasField.HasField "shape_origin" Shape ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Shape {shape_origin = y1, shape_next = BG.getField @"shape_next" x0}
      , BG.getField @"shape_origin" x0
      )

instance ( ty ~ Lib.Geo.Point.Point
         ) => BG.HasField "shape_origin" (BG.Ptr Shape) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"shape_origin")

instance HasCField.HasCField Shape "shape_origin" where

  type CFieldType Shape "shape_origin" =
    Lib.Geo.Point.Point

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @next@

    __defined at:__ @geo\/shape.h 5:51@

    __exported by:__ @root.h@
-}
instance ( ty ~ BG.Ptr Shape
         ) => BG.CompatHasField.HasField "shape_next" Shape ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Shape {shape_next = y1, shape_origin = BG.getField @"shape_origin" x0}
      , BG.getField @"shape_next" x0
      )

instance ( ty ~ BG.Ptr Shape
         ) => BG.HasField "shape_next" (BG.Ptr Shape) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"shape_next")

instance HasCField.HasCField Shape "shape_next" where

  type CFieldType Shape "shape_next" = BG.Ptr Shape

  offset# = \_ -> \_ -> 16
