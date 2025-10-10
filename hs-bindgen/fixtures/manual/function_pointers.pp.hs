{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.FunPtr.Class
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource "#include <manual/function_pointers.h>\nsigned int hs_bindgen_test_manualfunction_pointers_55e5eb89e54abf83 (signed int arg1) { return square(arg1); }\nsigned int hs_bindgen_test_manualfunction_pointers_680daf766a044980 (signed int arg1, signed int arg2) { return plus(arg1, arg2); }\nsigned int hs_bindgen_test_manualfunction_pointers_abcb860034253564 (signed int (*arg1) (signed int arg1), signed int arg2) { return apply1(arg1, arg2); }\nsigned int hs_bindgen_test_manualfunction_pointers_1ad13c166a710f40 (signed int (*arg1) (signed int arg1, signed int arg2), signed int arg2, signed int arg3) { return apply2(arg1, arg2, arg3); }\nsigned int hs_bindgen_test_manualfunction_pointers_a8ef4d9e6ce68f54 (int2int *arg1, signed int arg2) { return apply1_pointer_arg(arg1, arg2); }\nsigned int hs_bindgen_test_manualfunction_pointers_7dc4caa1f7f0caf0 (int2int *arg1, signed int arg2) { return apply1_nopointer_arg(arg1, arg2); }\nsigned int (*const hs_bindgen_test_manualfunction_pointers_3612aa0d10e36d5b (void)) (int2int *arg1, signed int arg2) { return apply1_nopointer_res(); }\n/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9 (void)) (signed int arg1) { return &square; } \n/* get_plus_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_bf838c747898dc42 (void)) (signed int arg1, signed int arg2) { return &plus; } \n/* get_apply1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070 (void)) (signed int (*arg1) (signed int arg1), signed int arg2) { return &apply1; } \n/* get_apply2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a (void)) (signed int (*arg1) (signed int arg1, signed int arg2), signed int arg2, signed int arg3) { return &apply2; } \n/* get_apply1_pointer_arg_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca (void)) (int2int *arg1, signed int arg2) { return &apply1_pointer_arg; } \n/* get_apply1_nopointer_arg_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81 (void)) (int2int *arg1, signed int arg2) { return &apply1_nopointer_arg; } \n/* get_apply1_nopointer_res_ptr */ __attribute__ ((const)) signed int (*const (*hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f (void)) (void)) (int2int *arg1, signed int arg2) { return &apply1_nopointer_res; } \n/* get_apply1_nopointer_var_ptr */ __attribute__ ((const)) signed int (*const *hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6 (void)) (int2int *arg1, signed int arg2) { return &apply1_nopointer_var; } \n/* get_apply1_struct_ptr */ __attribute__ ((const)) struct Apply1Struct const *hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a (void) { return &apply1_struct; } \n/* get_apply1_union_ptr */ __attribute__ ((const)) union Apply1Union const *hs_bindgen_test_manualfunction_pointers_d32b4879673188b6 (void) { return &apply1_union; } \n")

{-| __C declaration:__ @int2int@

    __defined at:__ @manual\/function_pointers.h:19:13@

    __exported by:__ @manual\/function_pointers.h@
-}
newtype Int2int = Int2int
  { un_Int2int :: FC.CInt -> IO FC.CInt
  }

foreign import ccall safe "wrapper" toInt2int
  :: Int2int
  -> IO (Ptr.FunPtr Int2int)

foreign import ccall safe "dynamic" fromInt2int
  :: Ptr.FunPtr Int2int
  -> Int2int

instance HsBindgen.Runtime.FunPtr.Class.ToFunPtr Int2int where

  toFunPtr = toInt2int

instance HsBindgen.Runtime.FunPtr.Class.FromFunPtr Int2int where

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
get_apply1Union_apply1_nopointer_union_field :: Apply1Union -> Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
get_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_apply1Union_apply1_nopointer_union_field'

-}
set_apply1Union_apply1_nopointer_union_field :: (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)) -> Apply1Union
set_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_55e5eb89e54abf83" square
  :: FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_680daf766a044980" plus
  :: FC.CInt
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_abcb860034253564" apply1
  :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @f@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_1ad13c166a710f40" apply2
  :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
     {- ^ __C declaration:__ @f@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_a8ef4d9e6ce68f54" apply1_pointer_arg
  :: Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_7dc4caa1f7f0caf0" apply1_nopointer_arg
  :: Ptr.FunPtr Int2int
  -> FC.CInt
  -> IO FC.CInt

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@
-}
foreign import ccall safe "hs_bindgen_test_manualfunction_pointers_3612aa0d10e36d5b" apply1_nopointer_res
  :: IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9" hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @manual\/function_pointers.h:5:12@

    __exported by:__ @manual\/function_pointers.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_c41111f40a04cdc9

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_bf838c747898dc42" hs_bindgen_test_manualfunction_pointers_bf838c747898dc42
  :: IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE plus_ptr #-}

{-| __C declaration:__ @plus@

    __defined at:__ @manual\/function_pointers.h:7:12@

    __exported by:__ @manual\/function_pointers.h@
-}
plus_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
plus_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_bf838c747898dc42

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070" hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070
  :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_ptr #-}

{-| __C declaration:__ @apply1@

    __defined at:__ @manual\/function_pointers.h:9:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> FC.CInt -> IO FC.CInt)
apply1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_4d1935e01bc37070

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a" hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a
  :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply2_ptr #-}

{-| __C declaration:__ @apply2@

    __defined at:__ @manual\/function_pointers.h:11:12@

    __exported by:__ @manual\/function_pointers.h@
-}
apply2_ptr :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)) -> FC.CInt -> FC.CInt -> IO FC.CInt)
apply2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_40cb8952bacd236a

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca" hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca
  :: IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_pointer_arg_ptr #-}

{-| Basically the same as apply1(), but here for illustratory purposes.

__C declaration:__ @apply1_pointer_arg@

__defined at:__ @manual\/function_pointers.h:22:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_pointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_pointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_653c5bde7704c3ca

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81" hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81
  :: IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

{-# NOINLINE apply1_nopointer_arg_ptr #-}

{-| A version of apply1_pointer_arg() that declares to take a argument of function type, rather than a pointer-to-function type.

__C declaration:__ @apply1_nopointer_arg@

__defined at:__ @manual\/function_pointers.h:26:12@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_arg_ptr :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_arg_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_3bb9417cd7afec81

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f" hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f
  :: IO (Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))))

{-# NOINLINE apply1_nopointer_res_ptr #-}

{-| A function returning a pointer to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_res@

__defined at:__ @manual\/function_pointers.h:31:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_res_ptr :: Ptr.FunPtr (IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))
apply1_nopointer_res_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_f7a08d090f6f7b0f

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6" hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6
  :: IO (Ptr.Ptr (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)))

{-# NOINLINE apply1_nopointer_var_ptr #-}

{-| A global variable pointing to a function like apply1_nopointer().

__C declaration:__ @apply1_nopointer_var@

__defined at:__ @manual\/function_pointers.h:34:21@

__exported by:__ @manual\/function_pointers.h@
-}
apply1_nopointer_var_ptr :: Ptr.Ptr (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
apply1_nopointer_var_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_c4bb317da29227a6

{-# NOINLINE apply1_nopointer_var #-}

apply1_nopointer_var :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
apply1_nopointer_var =
  GHC.IO.Unsafe.unsafePerformIO (F.peek apply1_nopointer_var_ptr)

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a" hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a
  :: IO (Ptr.Ptr Apply1Struct)

{-# NOINLINE apply1_struct_ptr #-}

{-| __C declaration:__ @apply1_struct@

    __defined at:__ @manual\/function_pointers.h:40:34@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_struct_ptr :: Ptr.Ptr Apply1Struct
apply1_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_6799ff6bc99dff2a

{-# NOINLINE apply1_struct #-}

apply1_struct :: Apply1Struct
apply1_struct =
  GHC.IO.Unsafe.unsafePerformIO (F.peek apply1_struct_ptr)

foreign import ccall unsafe "hs_bindgen_test_manualfunction_pointers_d32b4879673188b6" hs_bindgen_test_manualfunction_pointers_d32b4879673188b6
  :: IO (Ptr.Ptr Apply1Union)

{-# NOINLINE apply1_union_ptr #-}

{-| __C declaration:__ @apply1_union@

    __defined at:__ @manual\/function_pointers.h:46:32@

    __exported by:__ @manual\/function_pointers.h@
-}
apply1_union_ptr :: Ptr.Ptr Apply1Union
apply1_union_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualfunction_pointers_d32b4879673188b6

{-# NOINLINE apply1_union #-}

apply1_union :: Apply1Union
apply1_union =
  GHC.IO.Unsafe.unsafePerformIO (F.peek apply1_union_ptr)
