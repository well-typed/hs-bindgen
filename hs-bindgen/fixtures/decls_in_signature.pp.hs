{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <decls_in_signature.h>\nvoid hs_bindgen_test_decls_in_signature_16f5d4c94f55e369 (struct opaque *arg1, struct outside *arg2, struct outside *arg3) { normal(arg1, arg2, *arg3); }\n/* get_normal_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93 (void)) (struct opaque *arg1, struct outside *arg2, struct outside arg3) { return &normal; } \nvoid hs_bindgen_test_decls_in_signature_8b60d38de80093fa (struct named_struct *arg1) { f1(*arg1); }\n/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8 (void)) (struct named_struct arg1) { return &f1; } \nvoid hs_bindgen_test_decls_in_signature_4a86b0420a250963 (union named_union *arg1) { f2(*arg1); }\n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba (void)) (union named_union arg1) { return &f2; } \n")

{-| __C declaration:__ @opaque@

    __defined at:__ @decls_in_signature.h:2:8@

    __exported by:__ @decls_in_signature.h@
-}
data Opaque

{-| __C declaration:__ @outside@

    __defined at:__ @decls_in_signature.h:3:8@

    __exported by:__ @decls_in_signature.h@
-}
data Outside = Outside
  { outside_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @decls_in_signature.h:4:7@

         __exported by:__ @decls_in_signature.h@
    -}
  , outside_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @decls_in_signature.h:5:7@

         __exported by:__ @decls_in_signature.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Outside where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Outside
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) outside_x2
            >> F.pokeByteOff ptr0 (4 :: Int) outside_y3

{-| __C declaration:__ @normal@

    __defined at:__ @decls_in_signature.h:7:6@

    __exported by:__ @decls_in_signature.h@
-}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_16f5d4c94f55e369" normal_wrapper
  :: F.Ptr Opaque
     {- ^ __C declaration:__ @ptr_to_opaque@
     -}
  -> F.Ptr Outside
     {- ^ __C declaration:__ @ptr_to_defined@
     -}
  -> F.Ptr Outside
     {- ^ __C declaration:__ @by_value@
     -}
  -> IO ()

normal :: (F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ()
normal =
  \x0 ->
    \x1 ->
      \x2 -> F.with x2 (\y3 -> normal_wrapper x0 x1 y3)

{-| __C declaration:__ @normal@

    __defined at:__ @decls_in_signature.h:7:6@

    __exported by:__ @decls_in_signature.h@
-}
foreign import ccall unsafe "hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93" hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93
  :: IO (F.FunPtr ((F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ()))

{-# NOINLINE normal_ptr #-}

normal_ptr :: F.FunPtr ((F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ())
normal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @named_struct@

__defined at:__ @decls_in_signature.h:17:16@

__exported by:__ @decls_in_signature.h@
-}
data Named_struct = Named_struct
  { named_struct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @decls_in_signature.h:17:35@

         __exported by:__ @decls_in_signature.h@
    -}
  , named_struct_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @decls_in_signature.h:17:42@

         __exported by:__ @decls_in_signature.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Named_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Named_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named_struct named_struct_x2 named_struct_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) named_struct_x2
            >> F.pokeByteOff ptr0 (4 :: Int) named_struct_y3

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1(struct named_struct)@

__defined at:__ @decls_in_signature.h:17:6@

__exported by:__ @decls_in_signature.h@
-}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_8b60d38de80093fa" f1_wrapper
  :: F.Ptr Named_struct
     {- ^ __C declaration:__ @arg@
     -}
  -> IO ()

f1 :: Named_struct -> IO ()
f1 = \x0 -> F.with x0 (\y1 -> f1_wrapper y1)

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1(struct named_struct)@

__defined at:__ @decls_in_signature.h:17:6@

__exported by:__ @decls_in_signature.h@
-}
foreign import ccall unsafe "hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8" hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8
  :: IO (F.FunPtr (Named_struct -> IO ()))

{-# NOINLINE f1_ptr #-}

f1_ptr :: F.FunPtr (Named_struct -> IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8

{-| __C declaration:__ @named_union@

    __defined at:__ @decls_in_signature.h:20:15@

    __exported by:__ @decls_in_signature.h@
-}
newtype Named_union = Named_union
  { un_Named_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Named_union

{-|

  __See:__ 'set_named_union_x'

__C declaration:__ @x@

__defined at:__ @decls_in_signature.h:20:33@

__exported by:__ @decls_in_signature.h@
-}
get_named_union_x :: Named_union -> FC.CInt
get_named_union_x =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_named_union_x'

-}
set_named_union_x :: FC.CInt -> Named_union
set_named_union_x =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_named_union_y'

__C declaration:__ @y@

__defined at:__ @decls_in_signature.h:20:41@

__exported by:__ @decls_in_signature.h@
-}
get_named_union_y :: Named_union -> FC.CChar
get_named_union_y =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_named_union_y'

-}
set_named_union_y :: FC.CChar -> Named_union
set_named_union_y =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __C declaration:__ @f2@

    __defined at:__ @decls_in_signature.h:20:6@

    __exported by:__ @decls_in_signature.h@
-}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_4a86b0420a250963" f2_wrapper
  :: F.Ptr Named_union
     {- ^ __C declaration:__ @arg@
     -}
  -> IO ()

f2 :: Named_union -> IO ()
f2 = \x0 -> F.with x0 (\y1 -> f2_wrapper y1)

{-| __C declaration:__ @f2@

    __defined at:__ @decls_in_signature.h:20:6@

    __exported by:__ @decls_in_signature.h@
-}
foreign import ccall unsafe "hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba" hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba
  :: IO (F.FunPtr (Named_union -> IO ()))

{-# NOINLINE f2_ptr #-}

f2_ptr :: F.FunPtr (Named_union -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba
