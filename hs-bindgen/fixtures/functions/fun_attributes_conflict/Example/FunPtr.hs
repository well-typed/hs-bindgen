{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "/* test_functionsfun_attributes_confl_Example_get_square_cp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c7316eabb7ed43d1 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cp;"
  , "}"
  , "/* test_functionsfun_attributes_confl_Example_get_square_pc_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_246016175c264c62 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pc;"
  , "}"
  , "/* test_functionsfun_attributes_confl_Example_get_square_cc_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_927690360fc8e8ef (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_cc;"
  , "}"
  , "/* test_functionsfun_attributes_confl_Example_get_square_pp_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a7faaa7acbf26148 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square_pp;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c7316eabb7ed43d1" hs_bindgen_c7316eabb7ed43d1_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cp_ptr@
hs_bindgen_c7316eabb7ed43d1 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_c7316eabb7ed43d1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c7316eabb7ed43d1_base

{-# NOINLINE square_cp_ptr #-}

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h:9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c7316eabb7ed43d1

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_246016175c264c62" hs_bindgen_246016175c264c62_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pc_ptr@
hs_bindgen_246016175c264c62 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_246016175c264c62 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_246016175c264c62_base

{-# NOINLINE square_pc_ptr #-}

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h:11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_246016175c264c62

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_927690360fc8e8ef" hs_bindgen_927690360fc8e8ef_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionsfun_attributes_confl_Example_get_square_cc_ptr@
hs_bindgen_927690360fc8e8ef ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_927690360fc8e8ef =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_927690360fc8e8ef_base

{-# NOINLINE square_cc_ptr #-}

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h:13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_cc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_927690360fc8e8ef

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a7faaa7acbf26148" hs_bindgen_a7faaa7acbf26148_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_functionsfun_attributes_confl_Example_get_square_pp_ptr@
hs_bindgen_a7faaa7acbf26148 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_a7faaa7acbf26148 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a7faaa7acbf26148_base

{-# NOINLINE square_pp_ptr #-}

{-| __C declaration:__ @square_pp@

    __defined at:__ @functions\/fun_attributes_conflict.h:15:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_pp_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a7faaa7acbf26148
