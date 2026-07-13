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
    ( Example.A_Aux(..)
    , Example.A(..)
    , Example.B(..)
    , Example.E(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified M

{-| Auxiliary type used by 'A'

    __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 6:15@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype A_Aux = A_Aux
  { unwrapA_Aux :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toA_Aux@
foreign import ccall safe "wrapper" hs_bindgen_1cabb32c661d9a0e_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toA_Aux@
hs_bindgen_1cabb32c661d9a0e ::
     A_Aux
  -> IO (BG.FunPtr A_Aux)
hs_bindgen_1cabb32c661d9a0e =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_1cabb32c661d9a0e_base (BG.toFFIType fun0))

-- __unique:__ @fromA_Aux@
foreign import ccall safe "dynamic" hs_bindgen_cdb12400c6863f15_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromA_Aux@
hs_bindgen_cdb12400c6863f15 ::
     BG.FunPtr A_Aux
  -> A_Aux
hs_bindgen_cdb12400c6863f15 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_cdb12400c6863f15_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr A_Aux where

  toFunPtr = hs_bindgen_1cabb32c661d9a0e

instance BG.FromFunPtr A_Aux where

  fromFunPtr = hs_bindgen_cdb12400c6863f15

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapA_Aux" A_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         A_Aux {unwrapA_Aux = y1}, BG.getField @"unwrapA_Aux" x0)

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapA_Aux" (BG.Ptr A_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapA_Aux")

instance HasCField.HasCField A_Aux "unwrapA_Aux" where

  type CFieldType A_Aux "unwrapA_Aux" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 6:15@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype A = A
  { unwrapA :: BG.FunPtr A_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr A_Aux
         ) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance ( ty ~ BG.FunPtr A_Aux
         ) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = BG.FunPtr A_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

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

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance (ty ~ M.C) => BG.CompatHasField.HasField "unwrapE" E ty where

  hasField =
    \x0 ->
      (\y1 -> E {unwrapE = y1}, BG.getField @"unwrapE" x0)

instance (ty ~ M.C) => BG.HasField "unwrapE" (BG.Ptr E) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
