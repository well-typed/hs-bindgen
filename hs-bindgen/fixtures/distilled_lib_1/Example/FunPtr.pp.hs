{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <distilled_lib_1.h>"
  , "/* get_some_fun_ptr */"
  , "__attribute__ ((const))"
  , "int32_t (*hs_bindgen_test_distilled_lib_1_969c7d0305e0614c (void)) ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t arg3[]"
  , ")"
  , "{"
  , "  return &some_fun;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_distilled_lib_1_969c7d0305e0614c" hs_bindgen_test_distilled_lib_1_969c7d0305e0614c ::
     IO (Ptr.FunPtr ((Ptr.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32))

{-# NOINLINE some_fun_ptr #-}

{-| __C declaration:__ @some_fun@

    __defined at:__ @distilled_lib_1.h:72:9@

    __exported by:__ @distilled_lib_1.h@
-}
some_fun_ptr :: Ptr.FunPtr ((Ptr.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32)
some_fun_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_distilled_lib_1_969c7d0305e0614c
