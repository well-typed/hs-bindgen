{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "/* Example_get_square_cp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_574f80bee9afb32c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cp;"
  , "}"
  , "/* Example_get_square_pc_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_e9ce4ba3fb76d372 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pc;"
  , "}"
  , "/* Example_get_square_cc_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_947c37eebdf37d61 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cc;"
  , "}"
  , "/* Example_get_square_pp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_confl_3f8b3de3936ce25a (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pp;"
  , "}"
  ]))

{-| __unique:__ @Example_get_square_cp_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_574f80bee9afb32c" hs_bindgen_test_functionsfun_attributes_confl_574f80bee9afb32c ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_cp_ptr #-}

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h:9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_574f80bee9afb32c

{-| __unique:__ @Example_get_square_pc_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_e9ce4ba3fb76d372" hs_bindgen_test_functionsfun_attributes_confl_e9ce4ba3fb76d372 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_pc_ptr #-}

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h:11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_e9ce4ba3fb76d372

{-| __unique:__ @Example_get_square_cc_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_947c37eebdf37d61" hs_bindgen_test_functionsfun_attributes_confl_947c37eebdf37d61 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_cc_ptr #-}

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h:13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_947c37eebdf37d61

{-| __unique:__ @Example_get_square_pp_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_3f8b3de3936ce25a" hs_bindgen_test_functionsfun_attributes_confl_3f8b3de3936ce25a ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_pp_ptr #-}

{-| __C declaration:__ @square_pp@

    __defined at:__ @functions\/fun_attributes_conflict.h:15:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_confl_3f8b3de3936ce25a
