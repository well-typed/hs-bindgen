{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_579aec1d49212412 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cp(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_f409a6ef5d2e2660 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_018929ca284a6211 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_75e74a414278132a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pp(arg1);"
  , "}"
  ]))

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h:9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@

__unique:__ @Example_Unsafe_square_cp@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_579aec1d49212412" square_cp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h:11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@

    __unique:__ @Example_Unsafe_square_pc@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_f409a6ef5d2e2660" square_pc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h:13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@

    __unique:__ @Example_Unsafe_square_cc@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_018929ca284a6211" square_cc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @square_pp@

__defined at:__ @functions\/fun_attributes_conflict.h:15:5@

__exported by:__ @functions\/fun_attributes_conflict.h@

__unique:__ @Example_Unsafe_square_pp@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_75e74a414278132a" square_pp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
