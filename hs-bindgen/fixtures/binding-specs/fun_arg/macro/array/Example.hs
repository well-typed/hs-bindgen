{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, Show)

{-| __C declaration:__ @MyArray@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 4:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype MyArray = MyArray
  { un_MyArray :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyArray) "un_MyArray")
         ) => GHC.Records.HasField "un_MyArray" (Ptr.Ptr MyArray) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyArray")

instance HsBindgen.Runtime.HasCField.HasCField MyArray "un_MyArray" where

  type CFieldType MyArray "un_MyArray" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype A = A
  { un_A :: MyArray
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving stock (Eq, Show)
