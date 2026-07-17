{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.some_fun
    )
  where

import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "/* test_edgecasesdistilled_lib_1_Example_get_some_fun */"
  , "__attribute__ ((const))"
  , "int32_t (*hs_bindgen_1ade3cfc18679577 (void)) ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t *arg3"
  , ")"
  , "{"
  , "  return &some_fun;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_some_fun@
foreign import ccall unsafe "hs_bindgen_1ade3cfc18679577" hs_bindgen_1ade3cfc18679577_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_some_fun@
hs_bindgen_1ade3cfc18679577 :: IO (BG.FunPtr (BG.Ptr A_type_t -> HsBindgen.Runtime.LibC.Word32 -> BG.Ptr (IsA.Elem (IA.IncompleteArray HsBindgen.Runtime.LibC.Word8)) -> IO HsBindgen.Runtime.LibC.Int32))
hs_bindgen_1ade3cfc18679577 =
  BG.fromFFIType hs_bindgen_1ade3cfc18679577_base

{-# NOINLINE some_fun #-}
{-| __C declaration:__ @some_fun@

    __defined at:__ @edge-cases\/distilled_lib_1.h 72:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
some_fun :: BG.FunPtr (BG.Ptr A_type_t -> HsBindgen.Runtime.LibC.Word32 -> BG.Ptr (IsA.Elem (IA.IncompleteArray HsBindgen.Runtime.LibC.Word8)) -> IO HsBindgen.Runtime.LibC.Int32)
some_fun =
  BG.unsafePerformIO hs_bindgen_1ade3cfc18679577
