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

module M.B
    ( M.B.Shape(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct shape@

    __defined at:__ @forward_decl\/b.h 4:8@

    __exported by:__ @forward_decl\/b.h@
-}
data Shape = Shape
  { shape_sides :: BG.CInt
    {- ^ __C declaration:__ @sides@

         __defined at:__ @forward_decl\/b.h 5:7@

         __exported by:__ @forward_decl\/b.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Shape where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Shape where

  readRaw =
    \ptr0 ->
          pure Shape
      <*> HasCField.readRaw (BG.Proxy @"shape_sides") ptr0

instance Marshal.WriteRaw Shape where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Shape shape_sides2 ->
            HasCField.writeRaw (BG.Proxy @"shape_sides") ptr0 shape_sides2

deriving via Marshal.EquivStorable Shape instance BG.Storable Shape

deriving via Struct.IsStructViaReadRaw Shape instance Struct.IsStruct Shape

{-| __C declaration:__ @sides@

    __defined at:__ @forward_decl\/b.h 5:7@

    __exported by:__ @forward_decl\/b.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "shape_sides" Shape ty where

  hasField =
    \x0 ->
      (\y1 ->
         Shape {shape_sides = y1}, BG.getField @"shape_sides" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "shape_sides" (BG.Ptr Shape) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"shape_sides")

instance HasCField.HasCField Shape "shape_sides" where

  type CFieldType Shape "shape_sides" = BG.CInt

  offset# = \_ -> \_ -> 0
