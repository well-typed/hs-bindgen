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
foreign import ccall unsafe "hs_bindgen_648d4f0fd0df4c79" hs_bindgen_648d4f0fd0df4c79 ::
     FC.CInt
  -> FC.CInt

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h 9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
square_cp = hs_bindgen_648d4f0fd0df4c79

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_pc@
foreign import ccall unsafe "hs_bindgen_632a1e6eb5ceeda7" hs_bindgen_632a1e6eb5ceeda7 ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h 11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
square_pc = hs_bindgen_632a1e6eb5ceeda7

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_cc@
foreign import ccall unsafe "hs_bindgen_56d75b1ff2482f13" hs_bindgen_56d75b1ff2482f13 ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h 13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
square_cc = hs_bindgen_56d75b1ff2482f13

-- __unique:__ @test_functionsfun_attributes_confl_Example_Unsafe_square_pp@
foreign import ccall unsafe "hs_bindgen_eac2f9645ef29119" hs_bindgen_eac2f9645ef29119 ::
     FC.CInt
  -> IO FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @square_pp@

__defined at:__ @functions\/fun_attributes_conflict.h 15:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
square_pp = hs_bindgen_eac2f9645ef29119
