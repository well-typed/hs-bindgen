{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

{-| __C declaration:__ @union U@

    __defined at:__ @binding-specs\/fun_arg\/union.h:1:7@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
newtype U = U
  { un_U :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable U

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim U

{-|

  __See:__ 'set_u_x'

__C declaration:__ @x@

__defined at:__ @binding-specs\/fun_arg\/union.h:1:15@

__exported by:__ @binding-specs\/fun_arg\/union.h@
-}
get_u_x ::
     U
  -> FC.CInt
get_u_x = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_u_x'

-}
set_u_x ::
     FC.CInt
  -> U
set_u_x = HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField U "u_x" where

  type CFieldType U "u_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U) "u_x")
         ) => GHC.Records.HasField "u_x" (Ptr.Ptr U) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"u_x")
