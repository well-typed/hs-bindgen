{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/program-slicing/macro_selected.h>"
  , "/* test_programanalysisprogramslici_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7fa7d51da57eb497 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_programanalysisprogramslici_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e57577b970e09cca (void)) ("
  , "  U arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogramslici_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_7fa7d51da57eb497" hs_bindgen_7fa7d51da57eb497_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_programanalysisprogramslici_Example_get_foo@
hs_bindgen_7fa7d51da57eb497 :: IO (Ptr.FunPtr (T -> IO ()))
hs_bindgen_7fa7d51da57eb497 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7fa7d51da57eb497_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @program-analysis\/program-slicing\/macro_selected.h 5:6@

    __exported by:__ @program-analysis\/program-slicing\/macro_selected.h@
-}
foo :: Ptr.FunPtr (T -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7fa7d51da57eb497

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_e57577b970e09cca" hs_bindgen_e57577b970e09cca_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
hs_bindgen_e57577b970e09cca :: IO (Ptr.FunPtr (U -> IO ()))
hs_bindgen_e57577b970e09cca =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e57577b970e09cca_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program-slicing\/macro_selected.h 10:6@

    __exported by:__ @program-analysis\/program-slicing\/macro_selected.h@
-}
bar :: Ptr.FunPtr (U -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e57577b970e09cca
