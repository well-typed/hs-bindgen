{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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

{-| __C declaration:__ @union y@

    __defined at:__ @declarations\/name_collision.h 3:7@

    __exported by:__ @declarations\/name_collision.h@
-}
data Y

{-| __C declaration:__ @union Y@

    __defined at:__ @declarations\/name_collision.h 4:7@

    __exported by:__ @declarations\/name_collision.h@
-}
newtype Y = Y
  { un_Y :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Y

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim Y

{-|

  __See:__ 'set_y_m'

__C declaration:__ @m@

__defined at:__ @declarations\/name_collision.h 5:7@

__exported by:__ @declarations\/name_collision.h@
-}
get_y_m ::
     Y
  -> FC.CInt
get_y_m = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_m'

-}
set_y_m ::
     FC.CInt
  -> Y
set_y_m = HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_y_o'

__C declaration:__ @o@

__defined at:__ @declarations\/name_collision.h 6:7@

__exported by:__ @declarations\/name_collision.h@
-}
get_y_o ::
     Y
  -> FC.CInt
get_y_o = HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y_o'

-}
set_y_o ::
     FC.CInt
  -> Y
set_y_o = HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Y "y_m" where

  type CFieldType Y "y_m" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Y) "y_m")
         ) => GHC.Records.HasField "y_m" (Ptr.Ptr Y) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"y_m")

instance HsBindgen.Runtime.HasCField.HasCField Y "y_o" where

  type CFieldType Y "y_o" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Y) "y_o")
         ) => GHC.Records.HasField "y_o" (Ptr.Ptr Y) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"y_o")
