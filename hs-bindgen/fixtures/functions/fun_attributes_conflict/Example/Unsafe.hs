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
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_564ce99eace0df40 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cp(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_216e7c195e335424 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_64cd3d5e7e4c6a2d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_fab086c774e84aae ("
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
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_564ce99eace0df40" square_cp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h:11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_216e7c195e335424" square_pc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h:13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_64cd3d5e7e4c6a2d" square_cc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @square_pp@

__defined at:__ @functions\/fun_attributes_conflict.h:15:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_confl_fab086c774e84aae" square_pp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
