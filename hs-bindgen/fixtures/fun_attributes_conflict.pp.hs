{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource "#include <fun_attributes_conflict.h>\nsigned int hs_bindgen_test_fun_attributes_conflict_5f961556d5c4089a (signed int arg1) { return square_cp(arg1); }\nsigned int hs_bindgen_test_fun_attributes_conflict_7dbcbb58ce184f13 (signed int arg1) { return square_pc(arg1); }\nsigned int hs_bindgen_test_fun_attributes_conflict_79823cdb6d9c52ff (signed int arg1) { return square_cc(arg1); }\nsigned int hs_bindgen_test_fun_attributes_conflict_3dd09f52296e7452 (signed int arg1) { return square_pp(arg1); }\n/* get_square_cp_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_a488b67527d299f8 (void)) (signed int arg1) { return &square_cp; } \n/* get_square_pc_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_c4cea088a40be2f5 (void)) (signed int arg1) { return &square_pc; } \n/* get_square_cc_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_3bc327fede4fc009 (void)) (signed int arg1) { return &square_cc; } \n/* get_square_pp_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_dca75c8c02c209b2 (void)) (signed int arg1) { return &square_pp; } \n")

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @fun_attributes_conflict.h:9:5@

__exported by:__ @fun_attributes_conflict.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_5f961556d5c4089a" square_cp
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_pc@

    __defined at:__ @fun_attributes_conflict.h:11:5@

    __exported by:__ @fun_attributes_conflict.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_7dbcbb58ce184f13" square_pc
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_cc@

    __defined at:__ @fun_attributes_conflict.h:13:5@

    __exported by:__ @fun_attributes_conflict.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_79823cdb6d9c52ff" square_cc
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @square_pp@

__defined at:__ @fun_attributes_conflict.h:15:5@

__exported by:__ @fun_attributes_conflict.h@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_3dd09f52296e7452" square_pp
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_conflict_a488b67527d299f8" hs_bindgen_test_fun_attributes_conflict_a488b67527d299f8
  :: IO (Ptr.FunPtr (FC.CInt -> HsBindgen.Runtime.FunPtr.Pure FC.CInt))

{-# NOINLINE square_cp_ptr #-}

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @fun_attributes_conflict.h:9:5@

__exported by:__ @fun_attributes_conflict.h@
-}
square_cp_ptr :: Ptr.FunPtr (FC.CInt -> HsBindgen.Runtime.FunPtr.Pure FC.CInt)
square_cp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_conflict_a488b67527d299f8

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_conflict_c4cea088a40be2f5" hs_bindgen_test_fun_attributes_conflict_c4cea088a40be2f5
  :: IO (Ptr.FunPtr (FC.CInt -> HsBindgen.Runtime.FunPtr.Pure FC.CInt))

{-# NOINLINE square_pc_ptr #-}

{-| __C declaration:__ @square_pc@

    __defined at:__ @fun_attributes_conflict.h:11:5@

    __exported by:__ @fun_attributes_conflict.h@
-}
square_pc_ptr :: Ptr.FunPtr (FC.CInt -> HsBindgen.Runtime.FunPtr.Pure FC.CInt)
square_pc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_conflict_c4cea088a40be2f5

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_conflict_3bc327fede4fc009" hs_bindgen_test_fun_attributes_conflict_3bc327fede4fc009
  :: IO (Ptr.FunPtr (FC.CInt -> HsBindgen.Runtime.FunPtr.Pure FC.CInt))

{-# NOINLINE square_cc_ptr #-}

{-| __C declaration:__ @square_cc@

    __defined at:__ @fun_attributes_conflict.h:13:5@

    __exported by:__ @fun_attributes_conflict.h@
-}
square_cc_ptr :: Ptr.FunPtr (FC.CInt -> HsBindgen.Runtime.FunPtr.Pure FC.CInt)
square_cc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_conflict_3bc327fede4fc009

foreign import ccall unsafe "hs_bindgen_test_fun_attributes_conflict_dca75c8c02c209b2" hs_bindgen_test_fun_attributes_conflict_dca75c8c02c209b2
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_pp_ptr #-}

{-| __C declaration:__ @square_pp@

    __defined at:__ @fun_attributes_conflict.h:15:5@

    __exported by:__ @fun_attributes_conflict.h@
-}
square_pp_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_fun_attributes_conflict_dca75c8c02c209b2
