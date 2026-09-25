{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
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
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/direct.h 1:14@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/direct.h@
-}
newtype T = T
  { unwrapT :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

-- __unique:__ @toT@
foreign import ccall safe "wrapper" hs_bindgen_b8534912f6256492_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toT@
hs_bindgen_b8534912f6256492 ::
     T
  -> IO (BG.FunPtr T)
hs_bindgen_b8534912f6256492 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_b8534912f6256492_base (\x1 ->
                                                            BG.getField @"unwrapT" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromT@
foreign import ccall safe "dynamic" hs_bindgen_8f830eadb43a6fe4_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromT@
hs_bindgen_8f830eadb43a6fe4 ::
     BG.FunPtr T
  -> T
hs_bindgen_8f830eadb43a6fe4 =
  \funPtr0 ->
    T (\x1 ->
         hs_bindgen_8f830eadb43a6fe4_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

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
