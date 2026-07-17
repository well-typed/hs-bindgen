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
    ( Example.T_Aux(..)
    , Example.T(..)
    , Example.U_Aux(..)
    , Example.U(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| Auxiliary type used by 'T'

    __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 1:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype T_Aux = T_Aux
  { unwrapT_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toT_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5927fedc3abfa99c_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @toT_Aux@
hs_bindgen_5927fedc3abfa99c ::
     T_Aux
  -> IO (BG.FunPtr T_Aux)
hs_bindgen_5927fedc3abfa99c =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_5927fedc3abfa99c_base (BG.toFFIType fun0))

-- __unique:__ @fromT_Aux@
foreign import ccall safe "dynamic" hs_bindgen_281617c90fa9307a_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @fromT_Aux@
hs_bindgen_281617c90fa9307a ::
     BG.FunPtr T_Aux
  -> T_Aux
hs_bindgen_281617c90fa9307a =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_281617c90fa9307a_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr T_Aux where

  toFunPtr = hs_bindgen_5927fedc3abfa99c

instance BG.FromFunPtr T_Aux where

  fromFunPtr = hs_bindgen_281617c90fa9307a

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapT_Aux" T_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         T_Aux {unwrapT_Aux = y1}, BG.getField @"unwrapT_Aux" x0)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.HasField "unwrapT_Aux" (BG.Ptr T_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapT_Aux")

instance HasCField.HasCField T_Aux "unwrapT_Aux" where

  type CFieldType T_Aux "unwrapT_Aux" =
    BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 1:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype T = T
  { unwrapT :: IA.IncompleteArray (BG.FunPtr T_Aux)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray (BG.FunPtr T_Aux)
         ) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance ( ty ~ IA.IncompleteArray (BG.FunPtr T_Aux)
         ) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" =
    IA.IncompleteArray (BG.FunPtr T_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'U'

    __C declaration:__ @U@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 2:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype U_Aux = U_Aux
  { unwrapU_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toU_Aux@
foreign import ccall safe "wrapper" hs_bindgen_a78147e446a66171_base ::
     (BG.Int32 -> IO ())
  -> IO (BG.FunPtr (BG.Int32 -> IO ()))

-- __unique:__ @toU_Aux@
hs_bindgen_a78147e446a66171 ::
     U_Aux
  -> IO (BG.FunPtr U_Aux)
hs_bindgen_a78147e446a66171 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_a78147e446a66171_base (BG.toFFIType fun0))

-- __unique:__ @fromU_Aux@
foreign import ccall safe "dynamic" hs_bindgen_abb428b050bb7646_base ::
     BG.FunPtr (BG.Int32 -> IO ())
  -> BG.Int32 -> IO ()

-- __unique:__ @fromU_Aux@
hs_bindgen_abb428b050bb7646 ::
     BG.FunPtr U_Aux
  -> U_Aux
hs_bindgen_abb428b050bb7646 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_abb428b050bb7646_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr U_Aux where

  toFunPtr = hs_bindgen_a78147e446a66171

instance BG.FromFunPtr U_Aux where

  fromFunPtr = hs_bindgen_abb428b050bb7646

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapU_Aux" U_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         U_Aux {unwrapU_Aux = y1}, BG.getField @"unwrapU_Aux" x0)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.HasField "unwrapU_Aux" (BG.Ptr U_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapU_Aux")

instance HasCField.HasCField U_Aux "unwrapU_Aux" where

  type CFieldType U_Aux "unwrapU_Aux" =
    BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @U@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 2:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype U = U
  { unwrapU :: CA.ConstantArray 3 (BG.FunPtr U_Aux)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 (BG.FunPtr U_Aux)
         ) => BG.CompatHasField.HasField "unwrapU" U ty where

  hasField =
    \x0 ->
      (\y1 -> U {unwrapU = y1}, BG.getField @"unwrapU" x0)

instance ( ty ~ CA.ConstantArray 3 (BG.FunPtr U_Aux)
         ) => BG.HasField "unwrapU" (BG.Ptr U) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapU")

instance HasCField.HasCField U "unwrapU" where

  type CFieldType U "unwrapU" =
    CA.ConstantArray 3 (BG.FunPtr U_Aux)

  offset# = \_ -> \_ -> 0
