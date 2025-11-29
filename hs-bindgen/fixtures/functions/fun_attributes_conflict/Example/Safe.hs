{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_05d3c772b0f20aca ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cp(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_f7ae8bbb46093b47 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_9e232358bd3bc8ef ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_809cafa7c31c4634 ("
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

__unique:__ @Example_Safe_square_cp@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_05d3c772b0f20aca" square_cp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h:11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@

    __unique:__ @Example_Safe_square_pc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_f7ae8bbb46093b47" square_pc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h:13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@

    __unique:__ @Example_Safe_square_cc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_9e232358bd3bc8ef" square_cc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @square_pp@

__defined at:__ @functions\/fun_attributes_conflict.h:15:5@

__exported by:__ @functions\/fun_attributes_conflict.h@

__unique:__ @Example_Safe_square_pp@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_809cafa7c31c4634" square_pp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
