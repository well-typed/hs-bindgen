{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA

{-| __C declaration:__ @MyArray@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 4:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype MyArray = MyArray
  { unwrapMyArray :: IA.IncompleteArray RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ((~) ty) (IA.IncompleteArray RIP.CInt)
         ) => RIP.HasField "unwrapMyArray" (RIP.Ptr MyArray) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyArray")

instance HasCField.HasCField MyArray "unwrapMyArray" where

  type CFieldType MyArray "unwrapMyArray" =
    IA.IncompleteArray RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype A = A
  { unwrapA :: MyArray
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ((~) ty) MyArray
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyArray

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance (((~) ty) A) => RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
