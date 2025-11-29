{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/zero_copy.h>"
  , "/* Example_get_reverse_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_manualzero_copy_6c907b09c096f10f (void)) ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return &reverse;"
  , "}"
  , "/* Example_get_transpose_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_manualzero_copy_c8ec17b4322959b0 (void)) ("
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  ]))

{-| __unique:__ @Example_get_reverse_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualzero_copy_6c907b09c096f10f" hs_bindgen_test_manualzero_copy_6c907b09c096f10f ::
     IO (Ptr.FunPtr ((Ptr.Ptr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt))

{-# NOINLINE reverse_ptr #-}

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h:77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse_ptr :: Ptr.FunPtr ((Ptr.Ptr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt)
reverse_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualzero_copy_6c907b09c096f10f

{-| __unique:__ @Example_get_transpose_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_manualzero_copy_c8ec17b4322959b0" hs_bindgen_test_manualzero_copy_c8ec17b4322959b0 ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))

{-# NOINLINE transpose_ptr #-}

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h:85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualzero_copy_c8ec17b4322959b0
