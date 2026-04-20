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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct pt@

    __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:9@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
data Pt = Pt
  { pt_x :: RIP.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:25@

         __exported by:__ @edge-cases\/include_macro_parent.h@
    -}
  , pt_y :: RIP.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @include_macro_child.h (CHILD_HEADER) 1:28@

         __exported by:__ @edge-cases\/include_macro_parent.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Pt where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Pt where

  readRaw =
    \ptr0 ->
          pure Pt
      <*> HasCField.readRaw (RIP.Proxy @"pt_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"pt_y") ptr0

instance Marshal.WriteRaw Pt where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pt pt_x2 pt_y3 ->
               HasCField.writeRaw (RIP.Proxy @"pt_x") ptr0 pt_x2
            >> HasCField.writeRaw (RIP.Proxy @"pt_y") ptr0 pt_y3

deriving via Marshal.EquivStorable Pt instance RIP.Storable Pt

instance HasCField.HasCField Pt "pt_x" where

  type CFieldType Pt "pt_x" = RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "pt_x" (RIP.Ptr Pt) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"pt_x")

instance HasCField.HasCField Pt "pt_y" where

  type CFieldType Pt "pt_y" = RIP.CDouble

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "pt_y" (RIP.Ptr Pt) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"pt_y")

{-| __C declaration:__ @macro CHILD_HEADER@

    __defined at:__ @edge-cases\/include_macro_parent.h 4:9@

    __exported by:__ @edge-cases\/include_macro_parent.h@
-}
cHILD_HEADER :: (RIP.Ptr RIP.CChar, Int)
cHILD_HEADER =
  ((RIP.Ptr "include_macro_child.h"#, 21) :: RIP.CStringLen)

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Rect where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Rect where

  readRaw =
    \ptr0 ->
          pure Rect
      <*> HasCField.readRaw (RIP.Proxy @"rect_tl") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"rect_br") ptr0

instance Marshal.WriteRaw Rect where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect rect_tl2 rect_br3 ->
               HasCField.writeRaw (RIP.Proxy @"rect_tl") ptr0 rect_tl2
            >> HasCField.writeRaw (RIP.Proxy @"rect_br") ptr0 rect_br3

deriving via Marshal.EquivStorable Rect instance RIP.Storable Rect

instance HasCField.HasCField Rect "rect_tl" where

  type CFieldType Rect "rect_tl" = Pt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Pt
         ) => RIP.HasField "rect_tl" (RIP.Ptr Rect) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"rect_tl")

instance HasCField.HasCField Rect "rect_br" where

  type CFieldType Rect "rect_br" = Pt

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) Pt
         ) => RIP.HasField "rect_br" (RIP.Ptr Rect) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"rect_br")
