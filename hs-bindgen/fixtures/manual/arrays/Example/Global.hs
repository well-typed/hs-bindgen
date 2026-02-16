{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/arrays.h>"
  , "/* test_manualarrays_Example_get_arr1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6843a8f5c61ff74e (void))[1]"
  , "{"
  , "  return &arr1;"
  , "}"
  , "/* test_manualarrays_Example_get_arr2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_60c5bfbe2e29672a (void))[3]"
  , "{"
  , "  return &arr2;"
  , "}"
  , "/* test_manualarrays_Example_get_arr3 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a0497dc4c0aba158 (void))[]"
  , "{"
  , "  return &arr3;"
  , "}"
  , "/* test_manualarrays_Example_get_sudoku */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_609c0f69bf6da356 (void))[3][3]"
  , "{"
  , "  return &sudoku;"
  , "}"
  , "/* test_manualarrays_Example_get_triplets */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_52a30badbe2c9671 (void))[][3]"
  , "{"
  , "  return &triplets;"
  , "}"
  , "/* test_manualarrays_Example_get_global_triplet_ptrs */"
  , "__attribute__ ((const))"
  , "triplet_ptrs *hs_bindgen_23817a1a9bc92057 (void)"
  , "{"
  , "  return &global_triplet_ptrs;"
  , "}"
  ]))

-- __unique:__ @test_manualarrays_Example_get_arr1@
foreign import ccall unsafe "hs_bindgen_6843a8f5c61ff74e" hs_bindgen_6843a8f5c61ff74e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_arr1@
hs_bindgen_6843a8f5c61ff74e :: IO (RIP.Ptr ((CA.ConstantArray 1) RIP.CInt))
hs_bindgen_6843a8f5c61ff74e =
  RIP.fromFFIType hs_bindgen_6843a8f5c61ff74e_base

{-# NOINLINE arr1 #-}
{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @manual\/arrays.h 13:12@

__exported by:__ @manual\/arrays.h@
-}
arr1 :: RIP.Ptr ((CA.ConstantArray 1) RIP.CInt)
arr1 =
  RIP.unsafePerformIO hs_bindgen_6843a8f5c61ff74e

-- __unique:__ @test_manualarrays_Example_get_arr2@
foreign import ccall unsafe "hs_bindgen_60c5bfbe2e29672a" hs_bindgen_60c5bfbe2e29672a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_arr2@
hs_bindgen_60c5bfbe2e29672a :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_60c5bfbe2e29672a =
  RIP.fromFFIType hs_bindgen_60c5bfbe2e29672a_base

{-# NOINLINE arr2 #-}
{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @manual\/arrays.h 16:12@

__exported by:__ @manual\/arrays.h@
-}
arr2 :: RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
arr2 =
  RIP.unsafePerformIO hs_bindgen_60c5bfbe2e29672a

-- __unique:__ @test_manualarrays_Example_get_arr3@
foreign import ccall unsafe "hs_bindgen_a0497dc4c0aba158" hs_bindgen_a0497dc4c0aba158_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_arr3@
hs_bindgen_a0497dc4c0aba158 :: IO (RIP.Ptr (IA.IncompleteArray RIP.CInt))
hs_bindgen_a0497dc4c0aba158 =
  RIP.fromFFIType hs_bindgen_a0497dc4c0aba158_base

{-# NOINLINE arr3 #-}
{-| Global, extern, incomplete

__C declaration:__ @arr3@

__defined at:__ @manual\/arrays.h 19:12@

__exported by:__ @manual\/arrays.h@
-}
arr3 :: RIP.Ptr (IA.IncompleteArray RIP.CInt)
arr3 =
  RIP.unsafePerformIO hs_bindgen_a0497dc4c0aba158

-- __unique:__ @test_manualarrays_Example_get_sudoku@
foreign import ccall unsafe "hs_bindgen_609c0f69bf6da356" hs_bindgen_609c0f69bf6da356_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_sudoku@
hs_bindgen_609c0f69bf6da356 :: IO (RIP.Ptr ((CA.ConstantArray 3) ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_609c0f69bf6da356 =
  RIP.fromFFIType hs_bindgen_609c0f69bf6da356_base

{-# NOINLINE sudoku #-}
{-| Multi-dimensional array of known size.

__C declaration:__ @sudoku@

__defined at:__ @manual\/arrays.h 22:12@

__exported by:__ @manual\/arrays.h@
-}
sudoku :: RIP.Ptr ((CA.ConstantArray 3) ((CA.ConstantArray 3) RIP.CInt))
sudoku =
  RIP.unsafePerformIO hs_bindgen_609c0f69bf6da356

-- __unique:__ @test_manualarrays_Example_get_triplets@
foreign import ccall unsafe "hs_bindgen_52a30badbe2c9671" hs_bindgen_52a30badbe2c9671_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_triplets@
hs_bindgen_52a30badbe2c9671 :: IO (RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_52a30badbe2c9671 =
  RIP.fromFFIType hs_bindgen_52a30badbe2c9671_base

{-# NOINLINE triplets #-}
{-| Multi-dimensional array of unknown size. Only the first dimension is allowed to be unknown.

__C declaration:__ @triplets@

__defined at:__ @manual\/arrays.h 26:12@

__exported by:__ @manual\/arrays.h@
-}
triplets :: RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
triplets =
  RIP.unsafePerformIO hs_bindgen_52a30badbe2c9671

-- __unique:__ @test_manualarrays_Example_get_global_triplet_ptrs@
foreign import ccall unsafe "hs_bindgen_23817a1a9bc92057" hs_bindgen_23817a1a9bc92057_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_global_triplet_ptrs@
hs_bindgen_23817a1a9bc92057 :: IO (RIP.Ptr Triplet_ptrs)
hs_bindgen_23817a1a9bc92057 =
  RIP.fromFFIType hs_bindgen_23817a1a9bc92057_base

{-# NOINLINE global_triplet_ptrs #-}
{-| A global of triplet_ptrs

__C declaration:__ @global_triplet_ptrs@

__defined at:__ @manual\/arrays.h 47:21@

__exported by:__ @manual\/arrays.h@
-}
global_triplet_ptrs :: RIP.Ptr Triplet_ptrs
global_triplet_ptrs =
  RIP.unsafePerformIO hs_bindgen_23817a1a9bc92057
