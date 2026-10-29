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
    ( M.B.B(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.A
import Prelude ((<*>), Eq, IO, Int, Show, pure, type (~))
import {-# SOURCE #-} M.B.Callback

{-| __C declaration:__ @struct b@

    __defined at:__ @b.h 5:8@

    __exported by:__ @root.h@
-}
data B = B
  { b_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @b.h 5:16@

         __exported by:__ @root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HasCField.readRaw (BG.Proxy @"b_y") ptr0

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_y2 ->
            HasCField.writeRaw (BG.Proxy @"b_y") ptr0 b_y2

deriving via Marshal.EquivStorable B instance BG.Storable B

deriving via Struct.IsStructViaReadRaw B instance Struct.IsStruct B

{-| __C declaration:__ @y@

    __defined at:__ @b.h 5:16@

    __exported by:__ @root.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b_y" B ty where

  hasField =
    \x0 -> (\y1 -> B {b_y = y1}, BG.getField @"b_y" x0)

instance (ty ~ BG.CInt) => BG.HasField "b_y" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b_y")

instance HasCField.HasCField B "b_y" where

  type CFieldType B "b_y" = BG.CInt

  offset# = \_ -> \_ -> 0

instance BG.ToFunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ()) where

  toFunPtr = hs_bindgen_507c1de5873f8883

instance BG.FromFunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ()) where

  fromFunPtr = hs_bindgen_735802aaf5f6880e
