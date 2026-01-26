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
import qualified HsBindgen.Runtime.Array.KnownSize.Mutable
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, Ord, Show)

{-| __C declaration:__ @MyArray@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 4:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
newtype MyArray = MyArray
  { un_MyArray :: (HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyArray) "un_MyArray")
         ) => GHC.Records.HasField "un_MyArray" (Ptr.Ptr MyArray) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyArray")

instance HsBindgen.Runtime.HasCField.HasCField MyArray "un_MyArray" where

  type CFieldType MyArray "un_MyArray" =
    (HsBindgen.Runtime.Array.KnownSize.Mutable.Array 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
newtype A = A
  { un_A :: MyArray
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)
