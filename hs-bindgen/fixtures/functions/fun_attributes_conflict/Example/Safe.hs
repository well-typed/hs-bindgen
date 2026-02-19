{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "signed int hs_bindgen_5d7162df3a16d8d5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square_cp)(arg1);"
  , "}"
  , "signed int hs_bindgen_7f240b4e0c2eea24 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square_pc)(arg1);"
  , "}"
  , "signed int hs_bindgen_d32b50f04af10764 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square_cc)(arg1);"
  , "}"
  , "signed int hs_bindgen_fab6c9860ff1400b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square_pp)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_cp@
foreign import ccall safe "hs_bindgen_5d7162df3a16d8d5" hs_bindgen_5d7162df3a16d8d5_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_cp@
hs_bindgen_5d7162df3a16d8d5 ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_5d7162df3a16d8d5 =
  RIP.fromFFIType hs_bindgen_5d7162df3a16d8d5_base

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
square_cp = hs_bindgen_5d7162df3a16d8d5

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pc@
foreign import ccall safe "hs_bindgen_7f240b4e0c2eea24" hs_bindgen_7f240b4e0c2eea24_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pc@
hs_bindgen_7f240b4e0c2eea24 ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_7f240b4e0c2eea24 =
  RIP.fromFFIType hs_bindgen_7f240b4e0c2eea24_base

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
square_pc = hs_bindgen_7f240b4e0c2eea24

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_cc@
foreign import ccall safe "hs_bindgen_d32b50f04af10764" hs_bindgen_d32b50f04af10764_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_cc@
hs_bindgen_d32b50f04af10764 ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_d32b50f04af10764 =
  RIP.fromFFIType hs_bindgen_d32b50f04af10764_base

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
square_cc = hs_bindgen_d32b50f04af10764

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pp@
foreign import ccall safe "hs_bindgen_fab6c9860ff1400b" hs_bindgen_fab6c9860ff1400b_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pp@
hs_bindgen_fab6c9860ff1400b ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_fab6c9860ff1400b =
  RIP.fromFFIType hs_bindgen_fab6c9860ff1400b_base

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
square_pp = hs_bindgen_fab6c9860ff1400b
