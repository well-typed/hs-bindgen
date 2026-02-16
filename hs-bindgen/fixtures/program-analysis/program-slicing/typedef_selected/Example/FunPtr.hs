{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <program-analysis/program-slicing/typedef_selected.h>"
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_programanalysisprogramslici_Example_get_foo@
hs_bindgen_7fa7d51da57eb497 :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_7fa7d51da57eb497 =
  RIP.fromFFIType hs_bindgen_7fa7d51da57eb497_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @program-analysis\/program-slicing\/typedef_selected.h 5:6@

    __exported by:__ @program-analysis\/program-slicing\/typedef_selected.h@
-}
foo :: RIP.FunPtr (T -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_7fa7d51da57eb497

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_e57577b970e09cca" hs_bindgen_e57577b970e09cca_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_programanalysisprogramslici_Example_get_bar@
hs_bindgen_e57577b970e09cca :: IO (RIP.FunPtr (U -> IO ()))
hs_bindgen_e57577b970e09cca =
  RIP.fromFFIType hs_bindgen_e57577b970e09cca_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @program-analysis\/program-slicing\/typedef_selected.h 10:6@

    __exported by:__ @program-analysis\/program-slicing\/typedef_selected.h@
-}
bar :: RIP.FunPtr (U -> IO ())
bar = RIP.unsafePerformIO hs_bindgen_e57577b970e09cca
