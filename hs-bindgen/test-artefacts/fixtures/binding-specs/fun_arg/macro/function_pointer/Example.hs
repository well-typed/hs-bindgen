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
    ( Example.MyFunctionPointer_Aux(..)
    , Example.MyFunctionPointer(..)
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

{-| Auxiliary type used by 'MyFunctionPointer'

    __C declaration:__ @MyFunctionPointer@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 5:15@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype MyFunctionPointer_Aux = MyFunctionPointer_Aux
  { unwrapMyFunctionPointer_Aux :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toMyFunctionPointer_Aux@
foreign import ccall safe "wrapper" hs_bindgen_47dfd04698dd2e6f_base ::
     (BG.Int32 -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> IO BG.Int32))

-- __unique:__ @toMyFunctionPointer_Aux@
hs_bindgen_47dfd04698dd2e6f ::
     MyFunctionPointer_Aux
  -> IO (BG.FunPtr MyFunctionPointer_Aux)
hs_bindgen_47dfd04698dd2e6f =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_47dfd04698dd2e6f_base (BG.toFFIType fun0))

-- __unique:__ @fromMyFunctionPointer_Aux@
foreign import ccall safe "dynamic" hs_bindgen_5738272f94a589e2_base ::
     BG.FunPtr (BG.Int32 -> IO BG.Int32)
  -> BG.Int32 -> IO BG.Int32

-- __unique:__ @fromMyFunctionPointer_Aux@
hs_bindgen_5738272f94a589e2 ::
     BG.FunPtr MyFunctionPointer_Aux
  -> MyFunctionPointer_Aux
hs_bindgen_5738272f94a589e2 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_5738272f94a589e2_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr MyFunctionPointer_Aux where

  toFunPtr = hs_bindgen_47dfd04698dd2e6f

instance BG.FromFunPtr MyFunctionPointer_Aux where

  fromFunPtr = hs_bindgen_5738272f94a589e2

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapMyFunctionPointer_Aux" MyFunctionPointer_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyFunctionPointer_Aux {unwrapMyFunctionPointer_Aux = y1}
      , BG.getField @"unwrapMyFunctionPointer_Aux" x0
      )

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapMyFunctionPointer_Aux" (BG.Ptr MyFunctionPointer_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyFunctionPointer_Aux")

instance HasCField.HasCField MyFunctionPointer_Aux "unwrapMyFunctionPointer_Aux" where

  type CFieldType MyFunctionPointer_Aux "unwrapMyFunctionPointer_Aux" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyFunctionPointer@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 5:15@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype MyFunctionPointer = MyFunctionPointer
  { unwrapMyFunctionPointer :: BG.FunPtr MyFunctionPointer_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr MyFunctionPointer_Aux
         ) => BG.CompatHasField.HasField "unwrapMyFunctionPointer" MyFunctionPointer ty where

  hasField =
    \x0 ->
      ( \y1 ->
          MyFunctionPointer {unwrapMyFunctionPointer = y1}
      , BG.getField @"unwrapMyFunctionPointer" x0
      )

instance ( ty ~ BG.FunPtr MyFunctionPointer_Aux
         ) => BG.HasField "unwrapMyFunctionPointer" (BG.Ptr MyFunctionPointer) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMyFunctionPointer")

instance HasCField.HasCField MyFunctionPointer "unwrapMyFunctionPointer" where

  type CFieldType MyFunctionPointer "unwrapMyFunctionPointer" =
    BG.FunPtr MyFunctionPointer_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 9:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype A = A
  { unwrapA :: MyFunctionPointer
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ MyFunctionPointer
         ) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance ( ty ~ MyFunctionPointer
         ) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyFunctionPointer

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 10:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
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

{-| __C declaration:__ @macro E@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 31:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
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
