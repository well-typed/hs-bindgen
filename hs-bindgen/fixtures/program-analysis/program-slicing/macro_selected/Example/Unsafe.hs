{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/program-slicing/macro_selected.h>"
  , "void hs_bindgen_72197c1a408e40ac ("
  , "  T arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_401f15168ffec8ae ("
  , "  U arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogramslici_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_72197c1a408e40ac" hs_bindgen_72197c1a408e40ac_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_programanalysisprogramslici_Example_Unsafe_foo@
hs_bindgen_72197c1a408e40ac ::
     T
  -> IO ()
hs_bindgen_72197c1a408e40ac =
  RIP.fromFFIType hs_bindgen_72197c1a408e40ac_base

{-| __C declaration:__ @foo@

    __defined at:__ @program-analysis\/program-slicing\/macro_selected.h 5:6@

    __exported by:__ @program-analysis\/program-slicing\/macro_selected.h@
-}
foo ::
     T
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_72197c1a408e40ac

-- __unique:__ @test_programanalysisprogramslici_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_401f15168ffec8ae" hs_bindgen_401f15168ffec8ae_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_programanalysisprogramslici_Example_Unsafe_bar@
hs_bindgen_401f15168ffec8ae ::
     U
  -> IO ()
hs_bindgen_401f15168ffec8ae =
  RIP.fromFFIType hs_bindgen_401f15168ffec8ae_base

{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program-slicing\/macro_selected.h 10:6@

    __exported by:__ @program-analysis\/program-slicing\/macro_selected.h@
-}
bar ::
     U
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_401f15168ffec8ae
