{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "int32_t hs_bindgen_57cb99ed92c001ad ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t (*arg3)[]"
  , ")"
  , "{"
  , "  return (some_fun)(arg1, arg2, *arg3);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_Safe_some_fun@
foreign import ccall safe "hs_bindgen_57cb99ed92c001ad" hs_bindgen_57cb99ed92c001ad_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word32
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_Safe_some_fun@
hs_bindgen_57cb99ed92c001ad ::
     RIP.Ptr A_type_t
  -> HsBindgen.Runtime.LibC.Word32
  -> RIP.Ptr (IA.IncompleteArray HsBindgen.Runtime.LibC.Word8)
  -> IO HsBindgen.Runtime.LibC.Int32
hs_bindgen_57cb99ed92c001ad =
  RIP.fromFFIType hs_bindgen_57cb99ed92c001ad_base

{-| __C declaration:__ @some_fun@

    __defined at:__ @edge-cases\/distilled_lib_1.h 72:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
some_fun ::
     RIP.Ptr A_type_t
     -- ^ __C declaration:__ @i@
  -> HsBindgen.Runtime.LibC.Word32
     -- ^ __C declaration:__ @j@
  -> RIP.Ptr (IA.IncompleteArray HsBindgen.Runtime.LibC.Word8)
     -- ^ __C declaration:__ @k@
  -> IO HsBindgen.Runtime.LibC.Int32
some_fun = hs_bindgen_57cb99ed92c001ad
