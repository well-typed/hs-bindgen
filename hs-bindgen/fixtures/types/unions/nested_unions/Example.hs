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
import Prelude ((<*>), Int, pure)

{-| __C declaration:__ @union unionA@

    __defined at:__ @types\/unions\/nested_unions.h 2:15@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
newtype UnionA = UnionA
  { un_UnionA :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable UnionA

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim UnionA

{-|

  __See:__ 'set_unionA_a'

__C declaration:__ @a@

__defined at:__ @types\/unions\/nested_unions.h 3:21@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_unionA_a ::
     UnionA
  -> FC.CInt
get_unionA_a =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_unionA_a'

-}
set_unionA_a ::
     FC.CInt
  -> UnionA
set_unionA_a =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_unionA_b'

__C declaration:__ @b@

__defined at:__ @types\/unions\/nested_unions.h 4:22@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_unionA_b ::
     UnionA
  -> FC.CChar
get_unionA_b =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_unionA_b'

-}
set_unionA_b ::
     FC.CChar
  -> UnionA
set_unionA_b =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField UnionA "unionA_a" where

  type CFieldType UnionA "unionA_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType UnionA) "unionA_a")
         ) => GHC.Records.HasField "unionA_a" (Ptr.Ptr UnionA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"unionA_a")

instance HsBindgen.Runtime.HasCField.HasCField UnionA "unionA_b" where

  type CFieldType UnionA "unionA_b" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType UnionA) "unionA_b")
         ) => GHC.Records.HasField "unionA_b" (Ptr.Ptr UnionA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"unionA_b")

{-| __C declaration:__ @struct exA@

    __defined at:__ @types\/unions\/nested_unions.h 1:8@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
data ExA = ExA
  { exA_fieldA1 :: UnionA
    {- ^ __C declaration:__ @fieldA1@

         __defined at:__ @types\/unions\/nested_unions.h 5:11@

         __exported by:__ @types\/unions\/nested_unions.h@
    -}
  }

instance F.Storable ExA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExA
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exA_fieldA1") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exA_fieldA1") ptr0 exA_fieldA12

instance HsBindgen.Runtime.HasCField.HasCField ExA "exA_fieldA1" where

  type CFieldType ExA "exA_fieldA1" = UnionA

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExA) "exA_fieldA1")
         ) => GHC.Records.HasField "exA_fieldA1" (Ptr.Ptr ExA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exA_fieldA1")

{-| __C declaration:__ @union \@exB_fieldB1@

    __defined at:__ @types\/unions\/nested_unions.h 9:9@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
newtype ExB_fieldB1 = ExB_fieldB1
  { un_ExB_fieldB1 :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable ExB_fieldB1

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim ExB_fieldB1

{-|

  __See:__ 'set_exB_fieldB1_a'

__C declaration:__ @a@

__defined at:__ @types\/unions\/nested_unions.h 10:21@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_exB_fieldB1_a ::
     ExB_fieldB1
  -> FC.CInt
get_exB_fieldB1_a =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_exB_fieldB1_a'

-}
set_exB_fieldB1_a ::
     FC.CInt
  -> ExB_fieldB1
set_exB_fieldB1_a =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_exB_fieldB1_b'

__C declaration:__ @b@

__defined at:__ @types\/unions\/nested_unions.h 11:22@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_exB_fieldB1_b ::
     ExB_fieldB1
  -> FC.CChar
get_exB_fieldB1_b =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_exB_fieldB1_b'

-}
set_exB_fieldB1_b ::
     FC.CChar
  -> ExB_fieldB1
set_exB_fieldB1_b =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField ExB_fieldB1 "exB_fieldB1_a" where

  type CFieldType ExB_fieldB1 "exB_fieldB1_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExB_fieldB1) "exB_fieldB1_a")
         ) => GHC.Records.HasField "exB_fieldB1_a" (Ptr.Ptr ExB_fieldB1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exB_fieldB1_a")

instance HsBindgen.Runtime.HasCField.HasCField ExB_fieldB1 "exB_fieldB1_b" where

  type CFieldType ExB_fieldB1 "exB_fieldB1_b" =
    FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExB_fieldB1) "exB_fieldB1_b")
         ) => GHC.Records.HasField "exB_fieldB1_b" (Ptr.Ptr ExB_fieldB1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exB_fieldB1_b")

{-| __C declaration:__ @struct exB@

    __defined at:__ @types\/unions\/nested_unions.h 8:8@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
data ExB = ExB
  { exB_fieldB1 :: ExB_fieldB1
    {- ^ __C declaration:__ @fieldB1@

         __defined at:__ @types\/unions\/nested_unions.h 12:11@

         __exported by:__ @types\/unions\/nested_unions.h@
    -}
  }

instance F.Storable ExB where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExB
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exB_fieldB1") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exB_fieldB1") ptr0 exB_fieldB12

instance HsBindgen.Runtime.HasCField.HasCField ExB "exB_fieldB1" where

  type CFieldType ExB "exB_fieldB1" = ExB_fieldB1

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExB) "exB_fieldB1")
         ) => GHC.Records.HasField "exB_fieldB1" (Ptr.Ptr ExB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exB_fieldB1")
