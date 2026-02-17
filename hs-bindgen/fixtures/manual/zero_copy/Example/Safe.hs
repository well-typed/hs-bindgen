{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Array.Class
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/zero_copy.h>"
  , "signed int hs_bindgen_350cceac1101d344 ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return reverse(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2ff371c815d92b04 ("
  , "  triplet const *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  transpose(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_manualzero_copy_Example_Safe_reverse@
foreign import ccall safe "hs_bindgen_350cceac1101d344" hs_bindgen_350cceac1101d344_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_manualzero_copy_Example_Safe_reverse@
hs_bindgen_350cceac1101d344 ::
     HsBindgen.Runtime.PtrConst.PtrConst Vector
  -> Ptr.Ptr Vector
  -> IO FC.CInt
hs_bindgen_350cceac1101d344 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_350cceac1101d344_base

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h 77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse ::
     HsBindgen.Runtime.PtrConst.PtrConst Vector
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr Vector
     -- ^ __C declaration:__ @output@
  -> IO FC.CInt
reverse = hs_bindgen_350cceac1101d344

-- __unique:__ @test_manualzero_copy_Example_Safe_transpose@
foreign import ccall safe "hs_bindgen_2ff371c815d92b04" hs_bindgen_2ff371c815d92b04_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_manualzero_copy_Example_Safe_transpose@
hs_bindgen_2ff371c815d92b04 ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.Array.Class.Elem Matrix)
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem Matrix)
  -> IO ()
hs_bindgen_2ff371c815d92b04 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2ff371c815d92b04_base

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h 85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose ::
     HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.Array.Class.Elem Matrix)
     -- ^ __C declaration:__ @input@
  -> Ptr.Ptr (HsBindgen.Runtime.Array.Class.Elem Matrix)
     -- ^ __C declaration:__ @output@
  -> IO ()
transpose = hs_bindgen_2ff371c815d92b04
