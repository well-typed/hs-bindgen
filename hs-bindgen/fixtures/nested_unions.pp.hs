{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), Int, pure)

{-| __C declaration:__ @unionA@

    __defined at:__ @nested_unions.h:2:15@

    __exported by:__ @nested_unions.h@
-}
newtype UnionA = UnionA
  { un_UnionA :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable UnionA

{-|

  __See:__ 'set_unionA_a'

__C declaration:__ @a@

__defined at:__ @nested_unions.h:3:21@

__exported by:__ @nested_unions.h@
-}
get_unionA_a :: UnionA -> FC.CInt
get_unionA_a =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_unionA_a'

-}
set_unionA_a :: FC.CInt -> UnionA
set_unionA_a =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_unionA_b'

__C declaration:__ @b@

__defined at:__ @nested_unions.h:4:22@

__exported by:__ @nested_unions.h@
-}
get_unionA_b :: UnionA -> FC.CChar
get_unionA_b =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_unionA_b'

-}
set_unionA_b :: FC.CChar -> UnionA
set_unionA_b =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __C declaration:__ @exA@

    __defined at:__ @nested_unions.h:1:8@

    __exported by:__ @nested_unions.h@
-}
data ExA = ExA
  { exA_fieldA1 :: UnionA
    {- ^ __C declaration:__ @fieldA1@

         __defined at:__ @nested_unions.h:5:11@

         __exported by:__ @nested_unions.h@
    -}
  }

instance F.Storable ExA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExA
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 ->
            F.pokeByteOff ptr0 (0 :: Int) exA_fieldA12

{-| __defined at:__ @nested_unions.h:9:9@

    __exported by:__ @nested_unions.h@
-}
newtype ExB_fieldB1 = ExB_fieldB1
  { un_ExB_fieldB1 :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable ExB_fieldB1

{-|

  __See:__ 'set_exB_fieldB1_a'

__C declaration:__ @a@

__defined at:__ @nested_unions.h:10:21@

__exported by:__ @nested_unions.h@
-}
get_exB_fieldB1_a :: ExB_fieldB1 -> FC.CInt
get_exB_fieldB1_a =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_exB_fieldB1_a'

-}
set_exB_fieldB1_a :: FC.CInt -> ExB_fieldB1
set_exB_fieldB1_a =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_exB_fieldB1_b'

__C declaration:__ @b@

__defined at:__ @nested_unions.h:11:22@

__exported by:__ @nested_unions.h@
-}
get_exB_fieldB1_b :: ExB_fieldB1 -> FC.CChar
get_exB_fieldB1_b =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_exB_fieldB1_b'

-}
set_exB_fieldB1_b :: FC.CChar -> ExB_fieldB1
set_exB_fieldB1_b =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __C declaration:__ @exB@

    __defined at:__ @nested_unions.h:8:8@

    __exported by:__ @nested_unions.h@
-}
data ExB = ExB
  { exB_fieldB1 :: ExB_fieldB1
    {- ^ __C declaration:__ @fieldB1@

         __defined at:__ @nested_unions.h:12:11@

         __exported by:__ @nested_unions.h@
    -}
  }

instance F.Storable ExB where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExB
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 ->
            F.pokeByteOff ptr0 (0 :: Int) exB_fieldB12
