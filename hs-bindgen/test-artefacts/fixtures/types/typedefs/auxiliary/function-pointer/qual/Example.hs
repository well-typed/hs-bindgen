{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude (Eq, IO, Ord, Show, type (~))
import {-# SOURCE #-} Example.FFI

{-| Auxiliary type used by 'T'

    __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 1:23@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype T_Aux = T_Aux
  { unwrapT_Aux :: BG.CInt -> IO ()
  }
  deriving stock (BG.Generic)

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
