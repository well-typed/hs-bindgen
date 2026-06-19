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
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Auxiliary type used by 'T'

    __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 1:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype T_Aux = T_Aux
  { unwrapT_Aux :: RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toT_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5927fedc3abfa99c_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @toT_Aux@
hs_bindgen_5927fedc3abfa99c ::
     T_Aux
  -> IO (RIP.FunPtr T_Aux)
hs_bindgen_5927fedc3abfa99c =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_5927fedc3abfa99c_base (RIP.toFFIType fun0))

-- __unique:__ @fromT_Aux@
foreign import ccall safe "dynamic" hs_bindgen_281617c90fa9307a_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @fromT_Aux@
hs_bindgen_281617c90fa9307a ::
     RIP.FunPtr T_Aux
  -> T_Aux
hs_bindgen_281617c90fa9307a =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_281617c90fa9307a_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr T_Aux where

  toFunPtr = hs_bindgen_5927fedc3abfa99c

instance RIP.FromFunPtr T_Aux where

  fromFunPtr = hs_bindgen_281617c90fa9307a

instance ( ty ~ (RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapT_Aux" (RIP.Ptr T_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapT_Aux")

instance HasCField.HasCField T_Aux "unwrapT_Aux" where

  type CFieldType T_Aux "unwrapT_Aux" =
    RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 1:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype T = T
  { unwrapT :: IA.IncompleteArray (RIP.FunPtr T_Aux)
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray (RIP.FunPtr T_Aux)
         ) => RIP.HasField "unwrapT" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" =
    IA.IncompleteArray (RIP.FunPtr T_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'U'

    __C declaration:__ @U@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 2:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype U_Aux = U_Aux
  { unwrapU_Aux :: RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toU_Aux@
foreign import ccall safe "wrapper" hs_bindgen_a78147e446a66171_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @toU_Aux@
hs_bindgen_a78147e446a66171 ::
     U_Aux
  -> IO (RIP.FunPtr U_Aux)
hs_bindgen_a78147e446a66171 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_a78147e446a66171_base (RIP.toFFIType fun0))

-- __unique:__ @fromU_Aux@
foreign import ccall safe "dynamic" hs_bindgen_abb428b050bb7646_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @fromU_Aux@
hs_bindgen_abb428b050bb7646 ::
     RIP.FunPtr U_Aux
  -> U_Aux
hs_bindgen_abb428b050bb7646 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_abb428b050bb7646_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr U_Aux where

  toFunPtr = hs_bindgen_a78147e446a66171

instance RIP.FromFunPtr U_Aux where

  fromFunPtr = hs_bindgen_abb428b050bb7646

instance ( ty ~ (RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapU_Aux" (RIP.Ptr U_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapU_Aux")

instance HasCField.HasCField U_Aux "unwrapU_Aux" where

  type CFieldType U_Aux "unwrapU_Aux" =
    RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @U@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 2:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype U = U
  { unwrapU :: CA.ConstantArray 3 (RIP.FunPtr U_Aux)
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 (RIP.FunPtr U_Aux)
         ) => RIP.HasField "unwrapU" (RIP.Ptr U) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapU")

instance HasCField.HasCField U "unwrapU" where

  type CFieldType U "unwrapU" =
    CA.ConstantArray 3 (RIP.FunPtr U_Aux)

  offset# = \_ -> \_ -> 0
