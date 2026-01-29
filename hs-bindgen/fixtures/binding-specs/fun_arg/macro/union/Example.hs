{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.SizedByteArray
import HsBindgen.Runtime.TypeEquality (TyEq)

{-| __C declaration:__ @union MyUnion@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 4:7@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype MyUnion = MyUnion
  { un_MyUnion :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable MyUnion

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim MyUnion

{-|

  __See:__ 'set_myUnion_x'

__C declaration:__ @x@

__defined at:__ @binding-specs\/fun_arg\/macro\/union.h 4:21@

__exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
get_myUnion_x ::
     MyUnion
  -> FC.CInt
get_myUnion_x =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_myUnion_x'

-}
set_myUnion_x ::
     FC.CInt
  -> MyUnion
set_myUnion_x =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField MyUnion "myUnion_x" where

  type CFieldType MyUnion "myUnion_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyUnion) "myUnion_x")
         ) => GHC.Records.HasField "myUnion_x" (Ptr.Ptr MyUnion) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"myUnion_x")

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype A = A
  { un_A :: MyUnion
  }
  deriving newtype (F.Storable)

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving newtype (F.Storable)
