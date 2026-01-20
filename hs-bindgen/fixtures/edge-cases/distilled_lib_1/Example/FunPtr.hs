{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_some_fun@
hs_bindgen_1ade3cfc18679577 :: IO (Ptr.FunPtr ((Ptr.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32))
hs_bindgen_1ade3cfc18679577 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1ade3cfc18679577_base

{-# NOINLINE some_fun #-}
{-| __C declaration:__ @some_fun@

    __defined at:__ @edge-cases\/distilled_lib_1.h 72:9@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
some_fun :: Ptr.FunPtr ((Ptr.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32)
some_fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1ade3cfc18679577
