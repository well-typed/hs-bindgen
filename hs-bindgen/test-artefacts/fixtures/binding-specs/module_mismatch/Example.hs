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

module Example
    ( Example.Point(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct point@

    __defined at:__ @binding-specs\/module_mismatch.h 4:8@

    __exported by:__ @binding-specs\/module_mismatch.h@
-}
data Point = Point
  { point_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/module_mismatch.h 5:7@

         __exported by:__ @binding-specs\/module_mismatch.h@
    -}
  , point_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/module_mismatch.h 6:7@

         __exported by:__ @binding-specs\/module_mismatch.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Point where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point where

  readRaw =
    \ptr0 ->
          pure Point
      <*> HasCField.readRaw (BG.Proxy @"point_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point_y") ptr0

instance Marshal.WriteRaw Point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point point_x2 point_y3 ->
               HasCField.writeRaw (BG.Proxy @"point_x") ptr0 point_x2
            >> HasCField.writeRaw (BG.Proxy @"point_y") ptr0 point_y3

deriving via Marshal.EquivStorable Point instance BG.Storable Point

deriving via Struct.IsStructViaReadRaw Point instance Struct.IsStruct Point

{-| __C declaration:__ @x@

    __defined at:__ @binding-specs\/module_mismatch.h 5:7@

    __exported by:__ @binding-specs\/module_mismatch.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "point_x" Point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point {point_x = y1, point_y = BG.getField @"point_y" x0}
      , BG.getField @"point_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point_x" (BG.Ptr Point) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_x")

instance HasCField.HasCField Point "point_x" where

  type CFieldType Point "point_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @binding-specs\/module_mismatch.h 6:7@

    __exported by:__ @binding-specs\/module_mismatch.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "point_y" Point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point {point_y = y1, point_x = BG.getField @"point_x" x0}
      , BG.getField @"point_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point_y" (BG.Ptr Point) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_y")

instance HasCField.HasCField Point "point_y" where

  type CFieldType Point "point_y" = BG.CInt

  offset# = \_ -> \_ -> 4
