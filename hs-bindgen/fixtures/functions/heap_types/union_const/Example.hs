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

    __defined at:__ @functions\/heap_types\/union_const.h 3:7@

    __exported by:__ @functions\/heap_types\/union_const.h@
-}
newtype T = T
  { un_T :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable T

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim T

{-|

  __See:__ 'set_t_x'

__C declaration:__ @x@

__defined at:__ @functions\/heap_types\/union_const.h 4:7@

__exported by:__ @functions\/heap_types\/union_const.h@
-}
get_t_x ::
     T
  -> FC.CInt
get_t_x = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_t_x'

-}
set_t_x ::
     FC.CInt
  -> T
set_t_x = HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField T "t_x" where

  type CFieldType T "t_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T) "t_x")
         ) => GHC.Records.HasField "t_x" (Ptr.Ptr T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"t_x")
