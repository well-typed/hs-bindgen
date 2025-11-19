{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes_conflict.h>"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_5f961556d5c4089a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cp(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_7dbcbb58ce184f13 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_79823cdb6d9c52ff ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_cc(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_confl_3dd09f52296e7452 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square_pp(arg1);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_5f961556d5c4089a" square_cp_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> FC.CInt
    )

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

__C declaration:__ @square_cp@

__defined at:__ @functions\/fun_attributes_conflict.h:9:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
square_cp =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType square_cp_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_7dbcbb58ce184f13" square_pc_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> FC.CInt
    )

{-| __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h:11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
square_pc =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType square_pc_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_79823cdb6d9c52ff" square_cc_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> FC.CInt
    )

{-| __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h:13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
square_cc =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType square_cc_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_confl_3dd09f52296e7452" square_pp_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> IO FC.CInt
    )

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @square_pp@

__defined at:__ @functions\/fun_attributes_conflict.h:15:5@

__exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
square_pp =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType square_pp_base
