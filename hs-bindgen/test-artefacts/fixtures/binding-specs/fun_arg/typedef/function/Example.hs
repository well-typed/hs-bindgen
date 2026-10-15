{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.A(..)
    , Example.B(..)
    , Example.E(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified M
import Prelude (IO, type (~))
import {-# SOURCE #-} Example.FFI

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 6:13@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype A = A
  { unwrapA :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)

instance BG.ToFunPtr A where

  toFunPtr = hs_bindgen_0c7d4776a632d026

instance BG.FromFunPtr A where

  fromFunPtr = hs_bindgen_0cf9a6d50f563441

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (BG.Generic)

instance (ty ~ A) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance (ty ~ A) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (BG.Generic)

instance (ty ~ M.C) => BG.CompatHasField.HasField "unwrapE" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrapE = y1}, BG.getField @"unwrapE" x0)

instance (ty ~ M.C) => BG.HasField "unwrapE" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
