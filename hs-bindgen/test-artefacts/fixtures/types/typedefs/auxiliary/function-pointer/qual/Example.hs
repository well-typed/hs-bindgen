{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.T_Aux(..)
    , Example.T(..)
    , Example.P_Aux(..)
    , Example.P(..)
    , Example.A_Aux(..)
    , Example.A(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Eq, IO, Ord, Show, fmap, type (~))

{-| Auxiliary type used by 'T'

    __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 1:23@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype T_Aux = T_Aux
  { unwrapT_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

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

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 1:23@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype T = T
  { unwrapT :: BG.FunPtr T_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr T_Aux
         ) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance ( ty ~ BG.FunPtr T_Aux
         ) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = BG.FunPtr T_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'P'

    __C declaration:__ @P@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 4:24@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype P_Aux = P_Aux
  { unwrapP_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapP_Aux" P_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         P_Aux {unwrapP_Aux = y1}, BG.getField @"unwrapP_Aux" x0)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.HasField "unwrapP_Aux" (BG.Ptr P_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapP_Aux")

instance HasCField.HasCField P_Aux "unwrapP_Aux" where

  type CFieldType P_Aux "unwrapP_Aux" =
    BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @P@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 4:24@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype P = P
  { unwrapP :: PtrConst.PtrConst (BG.FunPtr P_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst (BG.FunPtr P_Aux)
         ) => BG.CompatHasField.HasField "unwrapP" P ty where

  hasField =
    \x0 ->
      (\y1 -> P {unwrapP = y1}, BG.getField @"unwrapP" x0)

instance ( ty ~ PtrConst.PtrConst (BG.FunPtr P_Aux)
         ) => BG.HasField "unwrapP" (BG.Ptr P) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapP")

instance HasCField.HasCField P "unwrapP" where

  type CFieldType P "unwrapP" =
    PtrConst.PtrConst (BG.FunPtr P_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'A'

    __C declaration:__ @A@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 7:25@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype A_Aux = A_Aux
  { unwrapA_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapA_Aux" A_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         A_Aux {unwrapA_Aux = y1}, BG.getField @"unwrapA_Aux" x0)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.HasField "unwrapA_Aux" (BG.Ptr A_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapA_Aux")

instance HasCField.HasCField A_Aux "unwrapA_Aux" where

  type CFieldType A_Aux "unwrapA_Aux" =
    BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 7:25@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype A = A
  { unwrapA :: PtrConst.PtrConst (CA.ConstantArray 3 (BG.FunPtr A_Aux))
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst (CA.ConstantArray 3 (BG.FunPtr A_Aux))
         ) => BG.CompatHasField.HasField "unwrapA" A ty where

  hasField =
    \x0 ->
      (\y1 -> A {unwrapA = y1}, BG.getField @"unwrapA" x0)

instance ( ty ~ PtrConst.PtrConst (CA.ConstantArray 3 (BG.FunPtr A_Aux))
         ) => BG.HasField "unwrapA" (BG.Ptr A) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" =
    PtrConst.PtrConst (CA.ConstantArray 3 (BG.FunPtr A_Aux))

  offset# = \_ -> \_ -> 0

-- __unique:__ @toT_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5927fedc3abfa99c_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toT_Aux@
hs_bindgen_5927fedc3abfa99c ::
     T_Aux
  -> IO (BG.FunPtr T_Aux)
hs_bindgen_5927fedc3abfa99c =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_5927fedc3abfa99c_base (\x1 ->
                                                            BG.getField @"unwrapT_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromT_Aux@
foreign import ccall safe "dynamic" hs_bindgen_281617c90fa9307a_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromT_Aux@
hs_bindgen_281617c90fa9307a ::
     BG.FunPtr T_Aux
  -> T_Aux
hs_bindgen_281617c90fa9307a =
  \funPtr0 ->
    T_Aux (\x1 ->
             hs_bindgen_281617c90fa9307a_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

instance BG.ToFunPtr T_Aux where

  toFunPtr = hs_bindgen_5927fedc3abfa99c

instance BG.FromFunPtr T_Aux where

  fromFunPtr = hs_bindgen_281617c90fa9307a

-- __unique:__ @toP_Aux@
foreign import ccall safe "wrapper" hs_bindgen_2aa75dd47929393d_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toP_Aux@
hs_bindgen_2aa75dd47929393d ::
     P_Aux
  -> IO (BG.FunPtr P_Aux)
hs_bindgen_2aa75dd47929393d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_2aa75dd47929393d_base (\x1 ->
                                                            BG.getField @"unwrapP_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromP_Aux@
foreign import ccall safe "dynamic" hs_bindgen_1eff17fed0c3e155_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromP_Aux@
hs_bindgen_1eff17fed0c3e155 ::
     BG.FunPtr P_Aux
  -> P_Aux
hs_bindgen_1eff17fed0c3e155 =
  \funPtr0 ->
    P_Aux (\x1 ->
             hs_bindgen_1eff17fed0c3e155_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

instance BG.ToFunPtr P_Aux where

  toFunPtr = hs_bindgen_2aa75dd47929393d

instance BG.FromFunPtr P_Aux where

  fromFunPtr = hs_bindgen_1eff17fed0c3e155

-- __unique:__ @toA_Aux@
foreign import ccall safe "wrapper" hs_bindgen_1cabb32c661d9a0e_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toA_Aux@
hs_bindgen_1cabb32c661d9a0e ::
     A_Aux
  -> IO (BG.FunPtr A_Aux)
hs_bindgen_1cabb32c661d9a0e =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_1cabb32c661d9a0e_base (\x1 ->
                                                            BG.getField @"unwrapA_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromA_Aux@
foreign import ccall safe "dynamic" hs_bindgen_cdb12400c6863f15_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromA_Aux@
hs_bindgen_cdb12400c6863f15 ::
     BG.FunPtr A_Aux
  -> A_Aux
hs_bindgen_cdb12400c6863f15 =
  \funPtr0 ->
    A_Aux (\x1 ->
             hs_bindgen_cdb12400c6863f15_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

instance BG.ToFunPtr A_Aux where

  toFunPtr = hs_bindgen_1cabb32c661d9a0e

instance BG.FromFunPtr A_Aux where

  fromFunPtr = hs_bindgen_cdb12400c6863f15
