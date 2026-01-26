{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/zero_copy.h>"
  , "signed int hs_bindgen_f9655173d51bbaac ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return reverse(arg1, arg2);"
  , "}"
  , "void hs_bindgen_ea25667627dd5ed2 ("
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  transpose(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_manualzero_copy_Example_Unsafe_reverse@
foreign import ccall unsafe "hs_bindgen_f9655173d51bbaac" hs_bindgen_f9655173d51bbaac_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualzero_copy_Example_Unsafe_reverse@
hs_bindgen_f9655173d51bbaac ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Vector
  -> Ptr.Ptr Vector
  -> IO FC.CInt
hs_bindgen_f9655173d51bbaac =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f9655173d51bbaac_base

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h 77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse ::
     HsBindgen.Runtime.ConstPtr.ConstPtr Vector
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Vector
     -- ^ __C declaration:__ @output@
  -> IO FC.CInt
reverse = hs_bindgen_f9655173d51bbaac

-- __unique:__ @test_manualzero_copy_Example_Unsafe_transpose@
foreign import ccall unsafe "hs_bindgen_ea25667627dd5ed2" hs_bindgen_ea25667627dd5ed2_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_manualzero_copy_Example_Unsafe_transpose@
hs_bindgen_ea25667627dd5ed2 ::
     Matrix
  -> Matrix
  -> IO ()
hs_bindgen_ea25667627dd5ed2 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ea25667627dd5ed2_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h 85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose ::
     Matrix
     -- ^ __C declaration:__ @input@
  -> Matrix
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_ea25667627dd5ed2
