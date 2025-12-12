{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/zero_copy.h>"
  , "/* test_manualzero_copy_Example_get_reverse_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d76d6b95b7803c78 (void)) ("
  , "  struct vector const *arg1,"
  , "  struct vector *arg2"
  , ")"
  , "{"
  , "  return &reverse;"
  , "}"
  , "/* test_manualzero_copy_Example_get_transpose_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f72c56033fb58d5e (void)) ("
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d76d6b95b7803c78" hs_bindgen_d76d6b95b7803c78_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualzero_copy_Example_get_reverse_ptr@
hs_bindgen_d76d6b95b7803c78 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.ConstPtr.ConstPtr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt))
hs_bindgen_d76d6b95b7803c78 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_d76d6b95b7803c78_base

{-# NOINLINE reverse_ptr #-}

{-| __C declaration:__ @reverse@

    __defined at:__ @manual\/zero_copy.h:77:5@

    __exported by:__ @manual\/zero_copy.h@
-}
reverse_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.ConstPtr.ConstPtr Vector) -> (Ptr.Ptr Vector) -> IO FC.CInt)
reverse_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d76d6b95b7803c78

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f72c56033fb58d5e" hs_bindgen_f72c56033fb58d5e_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_manualzero_copy_Example_get_transpose_ptr@
hs_bindgen_f72c56033fb58d5e ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))
hs_bindgen_f72c56033fb58d5e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f72c56033fb58d5e_base

{-# NOINLINE transpose_ptr #-}

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/zero_copy.h:85:6@

    __exported by:__ @manual\/zero_copy.h@
-}
transpose_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f72c56033fb58d5e
