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
    ( Example.MyFunction(..)
    , Example.A(..)
    , Example.B(..)
    , Example.E(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified M

{-| __C declaration:__ @MyFunction@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 5:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype MyFunction = MyFunction
  { unwrapMyFunction :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toMyFunction@
foreign import ccall safe "wrapper" hs_bindgen_4e8a459829b269e0_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toMyFunction@
hs_bindgen_4e8a459829b269e0 ::
     MyFunction
  -> IO (BG.FunPtr MyFunction)
hs_bindgen_4e8a459829b269e0 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_4e8a459829b269e0_base (BG.toFFIType fun0))

-- __unique:__ @fromMyFunction@
foreign import ccall safe "dynamic" hs_bindgen_bb71f7e730356103_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromMyFunction@
hs_bindgen_bb71f7e730356103 ::
     BG.FunPtr MyFunction
  -> MyFunction
hs_bindgen_bb71f7e730356103 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_bb71f7e730356103_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr MyFunction where

  toFunPtr = hs_bindgen_4e8a459829b269e0

instance BG.FromFunPtr MyFunction where

  fromFunPtr = hs_bindgen_bb71f7e730356103

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapMyFunction" MyFunction ty where

  hasField =
    \x0 ->
      ( \y1 -> MyFunction {unwrapMyFunction = y1}
      , BG.getField @"unwrapMyFunction" x0
      )

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapMyFunction" (BG.Ptr MyFunction) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyFunction")

instance HasCField.HasCField MyFunction "unwrapMyFunction" where

  type CFieldType MyFunction "unwrapMyFunction" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 9:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype A = A
  { unwrapA :: MyFunction
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance (ty ~ MyFunction) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance (ty ~ MyFunction) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyFunction

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 10:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance (ty ~ A) => BG.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, BG.getField @"unwrapB" x0)

instance (ty ~ A) => BG.HasField "unwrapB" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro E@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 31:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
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
