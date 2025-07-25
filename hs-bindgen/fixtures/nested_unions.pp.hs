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

newtype UnionA = UnionA
  { un_UnionA :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable UnionA

{-|

  __See:__ 'set_unionA_a'

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

data ExA = ExA
  { exA_fieldA1 :: UnionA
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

newtype ExB_fieldB1 = ExB_fieldB1
  { un_ExB_fieldB1 :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable ExB_fieldB1

{-|

  __See:__ 'set_exB_fieldB1_a'

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

data ExB = ExB
  { exB_fieldB1 :: ExB_fieldB1
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
