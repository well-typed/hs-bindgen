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

module M.A
    ( M.A.A(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified M.B
import Prelude ((<*>), Eq, IO, Int, Show, pure, type (~))
import {-# SOURCE #-} M.A.Callback

{-| __C declaration:__ @struct a@

    __defined at:__ @a.h 5:8@

    __exported by:__ @root.h@
-}
data A = A
  { a_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @a.h 5:16@

         __exported by:__ @root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HasCField.readRaw (BG.Proxy @"a_x") ptr0

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_x2 ->
            HasCField.writeRaw (BG.Proxy @"a_x") ptr0 a_x2

deriving via Marshal.EquivStorable A instance BG.Storable A

deriving via Struct.IsStructViaReadRaw A instance Struct.IsStruct A

{-| __C declaration:__ @x@

    __defined at:__ @a.h 5:16@

    __exported by:__ @root.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "a_x" A ty where

  hasField =
    \x0 -> (\y1 -> A {a_x = y1}, BG.getField @"a_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "a_x" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a_x")

instance HasCField.HasCField A "a_x" where

  type CFieldType A "a_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance BG.ToFunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ()) where

  toFunPtr = hs_bindgen_c6d383e1471a9625

instance BG.FromFunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ()) where

  fromFunPtr = hs_bindgen_4f4c392c56000856
