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
    , Example.Q_Aux(..)
    , Example.Q(..)
    )
  where

import qualified HsBindgen.Runtime.Block as Block
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Eq, IO, Ord, Show, fmap, type (~))

{-| Auxiliary type used by 'T'

    __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h 1:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h@
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

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h 1:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h@
-}
newtype T = T
  { unwrapT :: Block.Block T_Aux
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

instance ( ty ~ Block.Block T_Aux
         ) => BG.CompatHasField.HasField "unwrapT" T ty where

  hasField =
    \x0 ->
      (\y1 -> T {unwrapT = y1}, BG.getField @"unwrapT" x0)

instance ( ty ~ Block.Block T_Aux
         ) => BG.HasField "unwrapT" (BG.Ptr T) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = Block.Block T_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'P'

    __C declaration:__ @P@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h 4:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h@
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

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h 4:17@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h@
-}
newtype P = P
  { unwrapP :: BG.Ptr (Block.Block P_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (Block.Block P_Aux)
         ) => BG.CompatHasField.HasField "unwrapP" P ty where

  hasField =
    \x0 ->
      (\y1 -> P {unwrapP = y1}, BG.getField @"unwrapP" x0)

instance ( ty ~ BG.Ptr (Block.Block P_Aux)
         ) => BG.HasField "unwrapP" (BG.Ptr P) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapP")

instance HasCField.HasCField P "unwrapP" where

  type CFieldType P "unwrapP" =
    BG.Ptr (Block.Block P_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Q'

    __C declaration:__ @Q@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h 7:24@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h@
-}
newtype Q_Aux = Q_Aux
  { unwrapQ_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "unwrapQ_Aux" Q_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Q_Aux {unwrapQ_Aux = y1}, BG.getField @"unwrapQ_Aux" x0)

instance ( ty ~ (BG.CInt -> IO ())
         ) => BG.HasField "unwrapQ_Aux" (BG.Ptr Q_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapQ_Aux")

instance HasCField.HasCField Q_Aux "unwrapQ_Aux" where

  type CFieldType Q_Aux "unwrapQ_Aux" =
    BG.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Q@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h 7:24@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/block.h@
-}
newtype Q = Q
  { unwrapQ :: PtrConst.PtrConst (Block.Block Q_Aux)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst (Block.Block Q_Aux)
         ) => BG.CompatHasField.HasField "unwrapQ" Q ty where

  hasField =
    \x0 ->
      (\y1 -> Q {unwrapQ = y1}, BG.getField @"unwrapQ" x0)

instance ( ty ~ PtrConst.PtrConst (Block.Block Q_Aux)
         ) => BG.HasField "unwrapQ" (BG.Ptr Q) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapQ")

instance HasCField.HasCField Q "unwrapQ" where

  type CFieldType Q "unwrapQ" =
    PtrConst.PtrConst (Block.Block Q_Aux)

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

-- __unique:__ @toQ_Aux@
foreign import ccall safe "wrapper" hs_bindgen_e2d4702e9aada396_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toQ_Aux@
hs_bindgen_e2d4702e9aada396 ::
     Q_Aux
  -> IO (BG.FunPtr Q_Aux)
hs_bindgen_e2d4702e9aada396 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_e2d4702e9aada396_base (\x1 ->
                                                            BG.getField @"unwrapQ_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromQ_Aux@
foreign import ccall safe "dynamic" hs_bindgen_88fd204ea7c09037_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromQ_Aux@
hs_bindgen_88fd204ea7c09037 ::
     BG.FunPtr Q_Aux
  -> Q_Aux
hs_bindgen_88fd204ea7c09037 =
  \funPtr0 ->
    Q_Aux (\x1 ->
             hs_bindgen_88fd204ea7c09037_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

instance BG.ToFunPtr Q_Aux where

  toFunPtr = hs_bindgen_e2d4702e9aada396

instance BG.FromFunPtr Q_Aux where

  fromFunPtr = hs_bindgen_88fd204ea7c09037
