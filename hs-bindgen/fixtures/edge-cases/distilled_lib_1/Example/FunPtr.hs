{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "/* test_edgecasesdistilled_lib_1_Example_get_some_fun */"
  , "__attribute__ ((const))"
  , "int32_t (*hs_bindgen_1ade3cfc18679577 (void)) ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t arg3[]"
  , ")"
  , "{"
  , "  return &some_fun;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_some_fun@
foreign import ccall unsafe "hs_bindgen_1ade3cfc18679577" hs_bindgen_1ade3cfc18679577_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_some_fun@
hs_bindgen_1ade3cfc18679577 :: IO (RIP.FunPtr ((RIP.Ptr A_type_t) -> HsBindgen.Runtime.LibC.Word32 -> (IA.IncompleteArray HsBindgen.Runtime.LibC.Word8) -> IO HsBindgen.Runtime.LibC.Int32))
hs_bindgen_1ade3cfc18679577 =
  RIP.fromFFIType hs_bindgen_1ade3cfc18679577_base

{-# NOINLINE some_fun #-}
{-| __C declaration:__ @some_fun@

    __defined at:__ @edge-cases\/distilled_lib_1.h 72:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
some_fun :: RIP.FunPtr ((RIP.Ptr A_type_t) -> HsBindgen.Runtime.LibC.Word32 -> (IA.IncompleteArray HsBindgen.Runtime.LibC.Word8) -> IO HsBindgen.Runtime.LibC.Int32)
some_fun =
  RIP.unsafePerformIO hs_bindgen_1ade3cfc18679577
