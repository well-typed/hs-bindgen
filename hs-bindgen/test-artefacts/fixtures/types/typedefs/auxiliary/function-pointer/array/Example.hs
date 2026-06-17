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
    , Example.U(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 1:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype T = T
  { unwrapT :: IA.IncompleteArray (RIP.FunPtr (RIP.CInt -> IO ()))
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray (RIP.FunPtr (RIP.CInt -> IO ()))
         ) => RIP.HasField "unwrapT" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" =
    IA.IncompleteArray (RIP.FunPtr (RIP.CInt -> IO ()))

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @U@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h 2:16@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/array.h@
-}
newtype U = U
  { unwrapU :: CA.ConstantArray 3 (RIP.FunPtr (RIP.CInt -> IO ()))
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 (RIP.FunPtr (RIP.CInt -> IO ()))
         ) => RIP.HasField "unwrapU" (RIP.Ptr U) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapU")

instance HasCField.HasCField U "unwrapU" where

  type CFieldType U "unwrapU" =
    CA.ConstantArray 3 (RIP.FunPtr (RIP.CInt -> IO ()))

  offset# = \_ -> \_ -> 0
