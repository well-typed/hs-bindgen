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
import qualified HsBindgen.Runtime.Marshal
import qualified HsBindgen.Runtime.SizedByteArray
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct X@

    __defined at:__ @declarations\/definitions.h 23:8@

    __exported by:__ @declarations\/definitions.h@
-}
data X = X
  { x_n :: FC.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @declarations\/definitions.h 23:16@

         __exported by:__ @declarations\/definitions.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable X where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure X
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"x_n") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          X x_n2 ->
            HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"x_n") ptr0 x_n2

instance Data.Primitive.Types.Prim X where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        X (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, X v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              X x_n4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 x_n4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        X (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, X v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              X x_n4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 x_n4 s3

instance HsBindgen.Runtime.HasCField.HasCField X "x_n" where

  type CFieldType X "x_n" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType X) "x_n")
         ) => GHC.Records.HasField "x_n" (Ptr.Ptr X) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"x_n")

{-| __C declaration:__ @union Y@

    __defined at:__ @declarations\/definitions.h 26:7@

    __exported by:__ @declarations\/definitions.h@
-}
newtype Y = Y
  { unwrapY :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.StaticSize Y

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.ReadRaw Y

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.WriteRaw Y

deriving via HsBindgen.Runtime.Marshal.EquivStorable Y instance F.Storable Y

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim Y

{-|

  __See:__ 'set_y_m'

__C declaration:__ @m@

__defined at:__ @declarations\/definitions.h 26:15@

__exported by:__ @declarations\/definitions.h@
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

__defined at:__ @declarations\/definitions.h 26:22@

__exported by:__ @declarations\/definitions.h@
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
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"y_m")

instance HsBindgen.Runtime.HasCField.HasCField Y "y_o" where

  type CFieldType Y "y_o" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Y) "y_o")
         ) => GHC.Records.HasField "y_o" (Ptr.Ptr Y) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"y_o")
