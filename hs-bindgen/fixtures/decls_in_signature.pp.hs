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
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <decls_in_signature.h>\nvoid hs_bindgen_test_decls_in_signature_16f5d4c94f55e369 (struct opaque *arg1, struct outside *arg2, struct outside *arg3) { normal(arg1, arg2, *arg3); }\n/* get_normal_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93 (void)) (struct opaque *arg1, struct outside *arg2, struct outside arg3) { return &normal; } \nvoid hs_bindgen_test_decls_in_signature_8b60d38de80093fa (struct named_struct *arg1) { f1(*arg1); }\n/* get_f1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8 (void)) (struct named_struct arg1) { return &f1; } \nvoid hs_bindgen_test_decls_in_signature_4a86b0420a250963 (union named_union *arg1) { f2(*arg1); }\n/* get_f2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba (void)) (union named_union arg1) { return &f2; } \n")

data Opaque

data Outside = Outside
  { outside_x :: FC.CInt
  , outside_y :: FC.CInt
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

{-| __from C:__ @normal@ -}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_16f5d4c94f55e369" normal_wrapper
  :: F.Ptr Opaque
     {- ^ __from C:__ @ptr_to_opaque@ -}
  -> F.Ptr Outside
     {- ^ __from C:__ @ptr_to_defined@ -}
  -> F.Ptr Outside
     {- ^ __from C:__ @by_value@ -}
  -> IO ()

normal :: (F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ()
normal =
  \x0 ->
    \x1 ->
      \x2 -> F.with x2 (\y3 -> normal_wrapper x0 x1 y3)

foreign import ccall unsafe "hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93" hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93
  :: IO (F.FunPtr ((F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ()))

{-# NOINLINE normal_ptr #-}

normal_ptr :: F.FunPtr ((F.Ptr Opaque) -> (F.Ptr Outside) -> Outside -> IO ())
normal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_decls_in_signature_87a8c2dd9b065b93

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

  __from C:__ @named_struct@
-}
data Named_struct = Named_struct
  { named_struct_x :: FC.CInt
  , named_struct_y :: FC.CInt
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

  __from C:__ @f1(struct named_struct)@
-}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_8b60d38de80093fa" f1_wrapper
  :: F.Ptr Named_struct
     {- ^ __from C:__ @arg@ -}
  -> IO ()

f1 :: Named_struct -> IO ()
f1 = \x0 -> F.with x0 (\y1 -> f1_wrapper y1)

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

  __from C:__ @f1(struct named_struct)@
-}
foreign import ccall unsafe "hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8" hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8
  :: IO (F.FunPtr (Named_struct -> IO ()))

{-# NOINLINE f1_ptr #-}

f1_ptr :: F.FunPtr (Named_struct -> IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_decls_in_signature_a1b79fe9af8e18b8

newtype Named_union = Named_union
  { un_Named_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Named_union

{-|

  __See:__ 'set_named_union_x'

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

{-| __from C:__ @f2@ -}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_4a86b0420a250963" f2_wrapper
  :: F.Ptr Named_union
     {- ^ __from C:__ @arg@ -}
  -> IO ()

f2 :: Named_union -> IO ()
f2 = \x0 -> F.with x0 (\y1 -> f2_wrapper y1)

foreign import ccall unsafe "hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba" hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba
  :: IO (F.FunPtr (Named_union -> IO ()))

{-# NOINLINE f2_ptr #-}

f2_ptr :: F.FunPtr (Named_union -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_decls_in_signature_74cfd16f2b7e27ba
