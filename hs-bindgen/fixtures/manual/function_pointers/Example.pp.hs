{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), Eq, IO, Int, Show, pure)

{-| __C declaration:__ @int2int@

    __defined at:__ @manual\/function_pointers.h:19:13@

    __exported by:__ @manual\/function_pointers.h@
-}
newtype Int2int = Int2int
  { un_Int2int :: FC.CInt -> IO FC.CInt
  }

foreign import ccall safe "wrapper" toInt2int ::
     Int2int
  -> IO (Ptr.FunPtr Int2int)

foreign import ccall safe "dynamic" fromInt2int ::
     Ptr.FunPtr Int2int
  -> Int2int

instance HsBindgen.Runtime.FunPtr.ToFunPtr Int2int where

  toFunPtr = toInt2int

instance HsBindgen.Runtime.FunPtr.FromFunPtr Int2int where

  fromFunPtr = fromInt2int

{-| A struct field pointing to a function like apply1_nopointer().

__C declaration:__ @Apply1Struct@

__defined at:__ @manual\/function_pointers.h:37:8@

__exported by:__ @manual\/function_pointers.h@
-}
data Apply1Struct = Apply1Struct
  { apply1Struct_apply1_nopointer_struct_field :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
    {- ^ __C declaration:__ @apply1_nopointer_struct_field@

         __defined at:__ @manual\/function_pointers.h:38:16@

         __exported by:__ @manual\/function_pointers.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Apply1Struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Apply1Struct
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Apply1Struct apply1Struct_apply1_nopointer_struct_field2 ->
            F.pokeByteOff ptr0 (0 :: Int) apply1Struct_apply1_nopointer_struct_field2

{-| A union field pointing to a function like apply1_nopointer().

__C declaration:__ @Apply1Union@

__defined at:__ @manual\/function_pointers.h:43:7@

__exported by:__ @manual\/function_pointers.h@
-}
newtype Apply1Union = Apply1Union
  { un_Apply1Union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance F.Storable Apply1Union

{-|

  __See:__ 'set_apply1Union_apply1_nopointer_union_field'

__C declaration:__ @apply1_nopointer_union_field@

__defined at:__ @manual\/function_pointers.h:44:16@

__exported by:__ @manual\/function_pointers.h@
-}
get_apply1Union_apply1_nopointer_union_field ::
     Apply1Union
  -> Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
get_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_apply1Union_apply1_nopointer_union_field'

-}
set_apply1Union_apply1_nopointer_union_field ::
     Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
  -> Apply1Union
set_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.ByteArray.setUnionPayload
