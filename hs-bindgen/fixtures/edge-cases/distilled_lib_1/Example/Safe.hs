{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.LibC
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "int32_t hs_bindgen_57cb99ed92c001ad ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t (*arg3)[]"
  , ")"
  , "{"
  , "  return some_fun(arg1, arg2, *arg3);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_Safe_some_fun@
foreign import ccall safe "hs_bindgen_57cb99ed92c001ad" hs_bindgen_57cb99ed92c001ad_base ::
     Ptr.Ptr Void
  -> GHC.Word.Word32
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_Safe_some_fun@
hs_bindgen_57cb99ed92c001ad ::
     Ptr.Ptr A_type_t
  -> HsBindgen.Runtime.LibC.Word32
  -> Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.LibC.Word8)
  -> IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_57cb99ed92c001ad =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_57cb99ed92c001ad_base

{-| __C declaration:__ @some_fun@

    __defined at:__ @edge-cases\/distilled_lib_1.h 72:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
some_fun ::
     Ptr.Ptr A_type_t
     -- ^ __C declaration:__ @i@
  -> HsBindgen.Runtime.LibC.Word32
     -- ^ __C declaration:__ @j@
  -> Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.LibC.Word8)
     -- ^ __C declaration:__ @k@
  -> IO HsBindgen.Runtime.LibC.Int32
some_fun = hs_bindgen_57cb99ed92c001ad
