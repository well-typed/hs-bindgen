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
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.SizedByteArray
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @X@

    __defined at:__ @definitions.h:23:8@

    __exported by:__ @definitions.h@
-}
data X = X
  { x_n :: FC.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @definitions.h:23:16@

         __exported by:__ @definitions.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable X where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure X
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"x_n") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          X x_n2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"x_n") ptr0 x_n2

instance HsBindgen.Runtime.HasCField.HasCField X "x_n" where

  type CFieldType X "x_n" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType X) "x_n")
         ) => GHC.Records.HasField "x_n" (Ptr.Ptr X) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"x_n")

{-| __C declaration:__ @Y@

    __defined at:__ @definitions.h:26:7@

    __exported by:__ @definitions.h@
-}
newtype Y = Y
  { un_Y :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Y

{-|

  __See:__ 'set_y_m'

__C declaration:__ @m@

__defined at:__ @definitions.h:26:15@

__exported by:__ @definitions.h@
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

__defined at:__ @definitions.h:26:22@

__exported by:__ @definitions.h@
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
