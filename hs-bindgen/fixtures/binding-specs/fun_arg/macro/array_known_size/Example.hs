{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, Show)

{-| __C declaration:__ @MyArray@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 4:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
newtype MyArray = MyArray
  { unwrapMyArray :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyArray) "unwrapMyArray")
         ) => GHC.Records.HasField "unwrapMyArray" (Ptr.Ptr MyArray) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyArray")

instance HsBindgen.Runtime.HasCField.HasCField MyArray "unwrapMyArray" where

  type CFieldType MyArray "unwrapMyArray" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
newtype A = A
  { unwrapA :: MyArray
  }
  deriving stock (Eq, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "unwrapA")
         ) => GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyArray

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "unwrapB")
         ) => GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
