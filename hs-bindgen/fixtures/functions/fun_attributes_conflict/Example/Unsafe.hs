{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "signed int hs_bindgen_648d4f0fd0df4c79 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cp(arg1);"
  , "}"
  , "signed int hs_bindgen_632a1e6eb5ceeda7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pc(arg1);"
  , "}"
  , "signed int hs_bindgen_56d75b1ff2482f13 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cc(arg1);"
  , "}"
  , "signed int hs_bindgen_eac2f9645ef29119 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pp(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_cp@
foreign import ccall unsafe "hs_bindgen_648d4f0fd0df4c79" hs_bindgen_648d4f0fd0df4c79_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_cp@
hs_bindgen_648d4f0fd0df4c79 ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_648d4f0fd0df4c79 =
  RIP.fromFFIType hs_bindgen_648d4f0fd0df4c79_base

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

  Marked @__attribute((const))__@

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h 9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
square_cp = hs_bindgen_648d4f0fd0df4c79

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_pc@
foreign import ccall unsafe "hs_bindgen_632a1e6eb5ceeda7" hs_bindgen_632a1e6eb5ceeda7_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_pc@
hs_bindgen_632a1e6eb5ceeda7 ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_632a1e6eb5ceeda7 =
  RIP.fromFFIType hs_bindgen_632a1e6eb5ceeda7_base

{-|

  Marked @__attribute((const))__@

__C declaration:__ @square_pc@

__defined at:__ @functions\/fun_attributes_conflict.h 11:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
square_pc = hs_bindgen_632a1e6eb5ceeda7

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_cc@
foreign import ccall unsafe "hs_bindgen_56d75b1ff2482f13" hs_bindgen_56d75b1ff2482f13_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_cc@
hs_bindgen_56d75b1ff2482f13 ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_56d75b1ff2482f13 =
  RIP.fromFFIType hs_bindgen_56d75b1ff2482f13_base

{-|

  Marked @__attribute((const))__@

__C declaration:__ @square_cc@

__defined at:__ @functions\/fun_attributes_conflict.h 13:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
square_cc = hs_bindgen_56d75b1ff2482f13

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_pp@
foreign import ccall unsafe "hs_bindgen_eac2f9645ef29119" hs_bindgen_eac2f9645ef29119_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_pp@
hs_bindgen_eac2f9645ef29119 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_eac2f9645ef29119 =
  RIP.fromFFIType hs_bindgen_eac2f9645ef29119_base

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @square_pp@

__defined at:__ @functions\/fun_attributes_conflict.h 15:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
square_pp = hs_bindgen_eac2f9645ef29119
