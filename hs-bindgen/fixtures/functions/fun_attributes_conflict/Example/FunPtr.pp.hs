{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "/* get_square_cp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_a488b67527d299f8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cp;"
  , "}"
  , "/* get_square_pc_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_c4cea088a40be2f5 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pc;"
  , "}"
  , "/* get_square_cc_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_3bc327fede4fc009 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cc;"
  , "}"
  , "/* get_square_pp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_dca75c8c02c209b2 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pp;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_a488b67527d299f8" hs_bindgen_test_functionsfun_attributes_confl_a488b67527d299f8_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
    )

hs_bindgen_test_functionsfun_attributes_confl_a488b67527d299f8 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_test_functionsfun_attributes_confl_a488b67527d299f8 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionsfun_attributes_confl_a488b67527d299f8_base

{-# NOINLINE square_cp_ptr #-}

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h:9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_a488b67527d299f8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_c4cea088a40be2f5" hs_bindgen_test_functionsfun_attributes_confl_c4cea088a40be2f5_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
    )

hs_bindgen_test_functionsfun_attributes_confl_c4cea088a40be2f5 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_test_functionsfun_attributes_confl_c4cea088a40be2f5 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionsfun_attributes_confl_c4cea088a40be2f5_base

{-# NOINLINE square_pc_ptr #-}

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h:11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_c4cea088a40be2f5

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_3bc327fede4fc009" hs_bindgen_test_functionsfun_attributes_confl_3bc327fede4fc009_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
    )

hs_bindgen_test_functionsfun_attributes_confl_3bc327fede4fc009 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_test_functionsfun_attributes_confl_3bc327fede4fc009 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionsfun_attributes_confl_3bc327fede4fc009_base

{-# NOINLINE square_cc_ptr #-}

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h:13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_3bc327fede4fc009

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_dca75c8c02c209b2" hs_bindgen_test_functionsfun_attributes_confl_dca75c8c02c209b2_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
    )

hs_bindgen_test_functionsfun_attributes_confl_dca75c8c02c209b2 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_test_functionsfun_attributes_confl_dca75c8c02c209b2 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionsfun_attributes_confl_dca75c8c02c209b2_base

{-# NOINLINE square_pp_ptr #-}

{-| __C declaration:__ @square_pp@

    __defined at:__ @functions\/fun_attributes_conflict.h:15:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_dca75c8c02c209b2
