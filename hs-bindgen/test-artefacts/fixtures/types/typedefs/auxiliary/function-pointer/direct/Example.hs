{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/direct.h 1:14@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/direct.h@
-}
newtype T = T
  { unwrapT :: RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toT@
foreign import ccall safe "wrapper" hs_bindgen_b8534912f6256492_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @toT@
hs_bindgen_b8534912f6256492 ::
     T
  -> IO (RIP.FunPtr T)
hs_bindgen_b8534912f6256492 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_b8534912f6256492_base (RIP.toFFIType fun0))

-- __unique:__ @fromT@
foreign import ccall safe "dynamic" hs_bindgen_8f830eadb43a6fe4_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @fromT@
hs_bindgen_8f830eadb43a6fe4 ::
     RIP.FunPtr T
  -> T
hs_bindgen_8f830eadb43a6fe4 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_8f830eadb43a6fe4_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr T where

  toFunPtr = hs_bindgen_b8534912f6256492

instance RIP.FromFunPtr T where

  fromFunPtr = hs_bindgen_8f830eadb43a6fe4

instance ( ty ~ (RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapT" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0
