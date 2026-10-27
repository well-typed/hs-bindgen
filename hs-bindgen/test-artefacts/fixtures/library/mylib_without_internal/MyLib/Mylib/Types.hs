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

module MyLib.Mylib.Types
    ( MyLib.Mylib.Types.Point_t(..)
    , MyLib.Mylib.Types.Rect_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct point_t@

    __defined at:__ @mylib\/types.h 4:9@

    __exported by:__ @mylib.h@
-}
data Point_t = Point_t
  { point_t_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @mylib\/types.h 4:25@

         __exported by:__ @mylib.h@
    -}
  , point_t_y :: BG.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @mylib\/types.h 4:35@

         __exported by:__ @mylib.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Point_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Point_t where

  readRaw =
    \ptr0 ->
          pure Point_t
      <*> HasCField.readRaw (BG.Proxy @"point_t_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point_t_y") ptr0

instance Marshal.WriteRaw Point_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point_t point_t_x2 point_t_y3 ->
               HasCField.writeRaw (BG.Proxy @"point_t_x") ptr0 point_t_x2
            >> HasCField.writeRaw (BG.Proxy @"point_t_y") ptr0 point_t_y3

deriving via Marshal.EquivStorable Point_t instance BG.Storable Point_t

deriving via Struct.IsStructViaReadRaw Point_t instance Struct.IsStruct Point_t

{-| __C declaration:__ @x@

    __defined at:__ @mylib\/types.h 4:25@

    __exported by:__ @mylib.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "point_t_x" Point_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point_t {point_t_x = y1, point_t_y = BG.getField @"point_t_y" x0}
      , BG.getField @"point_t_x" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "point_t_x" (BG.Ptr Point_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_t_x")

instance HasCField.HasCField Point_t "point_t_x" where

  type CFieldType Point_t "point_t_x" = BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @mylib\/types.h 4:35@

    __exported by:__ @mylib.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "point_t_y" Point_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point_t {point_t_y = y1, point_t_x = BG.getField @"point_t_x" x0}
      , BG.getField @"point_t_y" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "point_t_y" (BG.Ptr Point_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_t_y")

instance HasCField.HasCField Point_t "point_t_y" where

  type CFieldType Point_t "point_t_y" = BG.CDouble

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct rect_t@

    __defined at:__ @mylib\/types.h 5:9@

    __exported by:__ @mylib.h@
-}
data Rect_t = Rect_t
  { rect_t_origin :: Point_t
    {- ^ __C declaration:__ @origin@

         __defined at:__ @mylib\/types.h 5:26@

         __exported by:__ @mylib.h@
    -}
  , rect_t_width :: BG.CDouble
    {- ^ __C declaration:__ @width@

         __defined at:__ @mylib\/types.h 5:41@

         __exported by:__ @mylib.h@
    -}
  , rect_t_height :: BG.CDouble
    {- ^ __C declaration:__ @height@

         __defined at:__ @mylib\/types.h 5:55@

         __exported by:__ @mylib.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Rect_t where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Rect_t where

  readRaw =
    \ptr0 ->
          pure Rect_t
      <*> HasCField.readRaw (BG.Proxy @"rect_t_origin") ptr0
      <*> HasCField.readRaw (BG.Proxy @"rect_t_width") ptr0
      <*> HasCField.readRaw (BG.Proxy @"rect_t_height") ptr0

instance Marshal.WriteRaw Rect_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect_t rect_t_origin2 rect_t_width3 rect_t_height4 ->
               HasCField.writeRaw (BG.Proxy @"rect_t_origin") ptr0 rect_t_origin2
            >> HasCField.writeRaw (BG.Proxy @"rect_t_width") ptr0 rect_t_width3
            >> HasCField.writeRaw (BG.Proxy @"rect_t_height") ptr0 rect_t_height4

deriving via Marshal.EquivStorable Rect_t instance BG.Storable Rect_t

deriving via Struct.IsStructViaReadRaw Rect_t instance Struct.IsStruct Rect_t

{-| __C declaration:__ @origin@

    __defined at:__ @mylib\/types.h 5:26@

    __exported by:__ @mylib.h@
-}
instance ( ty ~ Point_t
         ) => BG.CompatHasField.HasField "rect_t_origin" Rect_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rect_t { rect_t_origin = y1
                 , rect_t_width = BG.getField @"rect_t_width" x0
                 , rect_t_height = BG.getField @"rect_t_height" x0
                 }
      , BG.getField @"rect_t_origin" x0
      )

instance ( ty ~ Point_t
         ) => BG.HasField "rect_t_origin" (BG.Ptr Rect_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rect_t_origin")

instance HasCField.HasCField Rect_t "rect_t_origin" where

  type CFieldType Rect_t "rect_t_origin" = Point_t

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @width@

    __defined at:__ @mylib\/types.h 5:41@

    __exported by:__ @mylib.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "rect_t_width" Rect_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rect_t { rect_t_width = y1
                 , rect_t_origin = BG.getField @"rect_t_origin" x0
                 , rect_t_height = BG.getField @"rect_t_height" x0
                 }
      , BG.getField @"rect_t_width" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "rect_t_width" (BG.Ptr Rect_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rect_t_width")

instance HasCField.HasCField Rect_t "rect_t_width" where

  type CFieldType Rect_t "rect_t_width" = BG.CDouble

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @height@

    __defined at:__ @mylib\/types.h 5:55@

    __exported by:__ @mylib.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "rect_t_height" Rect_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rect_t { rect_t_height = y1
                 , rect_t_origin = BG.getField @"rect_t_origin" x0
                 , rect_t_width = BG.getField @"rect_t_width" x0
                 }
      , BG.getField @"rect_t_height" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "rect_t_height" (BG.Ptr Rect_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"rect_t_height")

instance HasCField.HasCField Rect_t "rect_t_height" where

  type CFieldType Rect_t "rect_t_height" = BG.CDouble

  offset# = \_ -> \_ -> 24
