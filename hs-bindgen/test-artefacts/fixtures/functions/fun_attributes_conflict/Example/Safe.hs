{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.square_cp
    , Example.Safe.square_pc
    , Example.Safe.square_cc
    , Example.Safe.square_pp
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     BG.Int32
  -> BG.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_cp@
hs_bindgen_5d7162df3a16d8d5 ::
     BG.CInt
  -> BG.CInt
hs_bindgen_5d7162df3a16d8d5 =
  BG.fromFFIType hs_bindgen_5d7162df3a16d8d5_base

{-|

    Marked @__attribute((const))__@

    __C declaration:__ @square_cp@

    __defined at:__ @functions\/fun_attributes_conflict.h 9:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cp ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> BG.CInt
square_cp = hs_bindgen_5d7162df3a16d8d5

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pc@
foreign import ccall safe "hs_bindgen_7f240b4e0c2eea24" hs_bindgen_7f240b4e0c2eea24_base ::
     BG.Int32
  -> BG.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pc@
hs_bindgen_7f240b4e0c2eea24 ::
     BG.CInt
  -> BG.CInt
hs_bindgen_7f240b4e0c2eea24 =
  BG.fromFFIType hs_bindgen_7f240b4e0c2eea24_base

{-|

    Marked @__attribute((const))__@

    __C declaration:__ @square_pc@

    __defined at:__ @functions\/fun_attributes_conflict.h 11:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pc ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> BG.CInt
square_pc = hs_bindgen_7f240b4e0c2eea24

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_cc@
foreign import ccall safe "hs_bindgen_d32b50f04af10764" hs_bindgen_d32b50f04af10764_base ::
     BG.Int32
  -> BG.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_cc@
hs_bindgen_d32b50f04af10764 ::
     BG.CInt
  -> BG.CInt
hs_bindgen_d32b50f04af10764 =
  BG.fromFFIType hs_bindgen_d32b50f04af10764_base

{-|

    Marked @__attribute((const))__@

    __C declaration:__ @square_cc@

    __defined at:__ @functions\/fun_attributes_conflict.h 13:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_cc ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> BG.CInt
square_cc = hs_bindgen_d32b50f04af10764

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pp@
foreign import ccall safe "hs_bindgen_fab6c9860ff1400b" hs_bindgen_fab6c9860ff1400b_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_confl_Example_Safe_square_pp@
hs_bindgen_fab6c9860ff1400b ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_fab6c9860ff1400b =
  BG.fromFFIType hs_bindgen_fab6c9860ff1400b_base

{-|

    Marked @__attribute((pure))__@

    __C declaration:__ @square_pp@

    __defined at:__ @functions\/fun_attributes_conflict.h 15:5@

    __exported by:__ @functions\/fun_attributes_conflict.h@
-}
square_pp ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
square_pp = hs_bindgen_fab6c9860ff1400b
