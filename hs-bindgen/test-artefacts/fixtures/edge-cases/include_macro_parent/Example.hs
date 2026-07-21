{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Pt(..)
    , Example.cHILD_HEADER
    , Example.Rect(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct pt@

    __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:9@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
data Pt = Pt
  { pt_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:25@

         __exported by:__ @edge-cases\/include_macro_parent.h@
    -}
  , pt_y :: BG.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:28@

         __exported by:__ @edge-cases\/include_macro_parent.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Pt where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Pt where

  readRaw =
    \ptr0 ->
          pure Pt
      <*> HasCField.readRaw (BG.Proxy @"pt_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"pt_y") ptr0

instance Marshal.WriteRaw Pt where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pt pt_x2 pt_y3 ->
               HasCField.writeRaw (BG.Proxy @"pt_x") ptr0 pt_x2
            >> HasCField.writeRaw (BG.Proxy @"pt_y") ptr0 pt_y3

deriving via Marshal.EquivStorable Pt instance BG.Storable Pt

{-| __C declaration:__ @x@

    __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:25@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "pt_x" Pt ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Pt {pt_x = y1, pt_y = BG.getField @"pt_y" x0}
      , BG.getField @"pt_x" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "pt_x" (BG.Ptr Pt) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"pt_x")

instance HasCField.HasCField Pt "pt_x" where

  type CFieldType Pt "pt_x" = BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:28@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
instance (ty ~ BG.CDouble) => BG.CompatHasField.HasField "pt_y" Pt ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Pt {pt_y = y1, pt_x = BG.getField @"pt_x" x0}
      , BG.getField @"pt_y" x0
      )

instance (ty ~ BG.CDouble) => BG.HasField "pt_y" (BG.Ptr Pt) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"pt_y")

instance HasCField.HasCField Pt "pt_y" where

  type CFieldType Pt "pt_y" = BG.CDouble

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @macro CHILD_HEADER@

    __C literal:__ @\"include_macro_child.h\"@

    __defined at:__ @edge-cases\/include_macro_parent.h 4:9@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
cHILD_HEADER :: BG.ByteString
cHILD_HEADER =
  BG.pack [0x69, 0x6E, 0x63, 0x6C, 0x75, 0x64, 0x65, 0x5F, 0x6D, 0x61, 0x63, 0x72, 0x6F, 0x5F, 0x63, 0x68, 0x69, 0x6C, 0x64, 0x2E, 0x68]

{-| __C declaration:__ @struct rect@

    __defined at:__ @edge-cases\/include_macro_parent.h 7:8@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
data Rect = Rect
  { rect_tl :: Pt
    {- ^ __C declaration:__ @tl@

         __defined at:__ @edge-cases\/include_macro_parent.h 7:18@

         __exported by:__ @edge-cases\/include_macro_parent.h@
    -}
  , rect_br :: Pt
    {- ^ __C declaration:__ @br@

         __defined at:__ @edge-cases\/include_macro_parent.h 7:22@

         __exported by:__ @edge-cases\/include_macro_parent.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Rect where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Rect where

  readRaw =
    \ptr0 ->
          pure Rect
      <*> HasCField.readRaw (BG.Proxy @"rect_tl") ptr0
      <*> HasCField.readRaw (BG.Proxy @"rect_br") ptr0

instance Marshal.WriteRaw Rect where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect rect_tl2 rect_br3 ->
               HasCField.writeRaw (BG.Proxy @"rect_tl") ptr0 rect_tl2
            >> HasCField.writeRaw (BG.Proxy @"rect_br") ptr0 rect_br3

deriving via Marshal.EquivStorable Rect instance BG.Storable Rect

{-| __C declaration:__ @tl@

    __defined at:__ @edge-cases\/include_macro_parent.h 7:18@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
instance (ty ~ Pt) => BG.CompatHasField.HasField "rect_tl" Rect ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rect {rect_tl = y1, rect_br = BG.getField @"rect_br" x0}
      , BG.getField @"rect_tl" x0
      )

instance (ty ~ Pt) => BG.HasField "rect_tl" (BG.Ptr Rect) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"rect_tl")

instance HasCField.HasCField Rect "rect_tl" where

  type CFieldType Rect "rect_tl" = Pt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @br@

    __defined at:__ @edge-cases\/include_macro_parent.h 7:22@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
instance (ty ~ Pt) => BG.CompatHasField.HasField "rect_br" Rect ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Rect {rect_br = y1, rect_tl = BG.getField @"rect_tl" x0}
      , BG.getField @"rect_br" x0
      )

instance (ty ~ Pt) => BG.HasField "rect_br" (BG.Ptr Rect) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"rect_br")

instance HasCField.HasCField Rect "rect_br" where

  type CFieldType Rect "rect_br" = Pt

  offset# = \_ -> \_ -> 16
