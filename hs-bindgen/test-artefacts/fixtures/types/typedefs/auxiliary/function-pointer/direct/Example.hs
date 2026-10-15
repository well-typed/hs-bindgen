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
    ( Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (IO, type (~))
import {-# SOURCE #-} Example.FFI

{-| __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/direct.h 1:14@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/direct.h@
-}
newtype T = T
  { unwrapT :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

instance BG.ToFunPtr T where

  toFunPtr = hs_bindgen_b8534912f6256492

instance BG.FromFunPtr T where

  fromFunPtr = hs_bindgen_8f830eadb43a6fe4

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0
