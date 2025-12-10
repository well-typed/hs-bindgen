{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <manual/arrays.h>"
  , "/* test_manualarrays_Example_get_arr1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_57b6693818620aec (void))[1]"
  , "{"
  , "  return &arr1;"
  , "}"
  , "/* test_manualarrays_Example_get_arr2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_928e8bd0dc2d0be1 (void))[3]"
  , "{"
  , "  return &arr2;"
  , "}"
  , "/* test_manualarrays_Example_get_arr3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_e6493a6ad08768b6 (void))[]"
  , "{"
  , "  return &arr3;"
  , "}"
  , "/* test_manualarrays_Example_get_sudoku_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_e692ad991693d041 (void))[3][3]"
  , "{"
  , "  return &sudoku;"
  , "}"
  , "/* test_manualarrays_Example_get_triplets_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_04df9f2a3065bb07 (void))[][3]"
  , "{"
  , "  return &triplets;"
  , "}"
  , "/* test_manualarrays_Example_get_global_triplet_ptrs_ptr */"
  , "__attribute__ ((const))"
  , "triplet_ptrs *hs_bindgen_7d8d8251f0367f2e (void)"
  , "{"
  , "  return &global_triplet_ptrs;"
  , "}"
  ]))

-- | __unique:__ @test_manualarrays_Example_get_arr1_ptr@
foreign import ccall unsafe "hs_bindgen_57b6693818620aec" hs_bindgen_57b6693818620aec ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr1_ptr #-}

{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @manual\/arrays.h:13:12@

__exported by:__ @manual\/arrays.h@
-}
arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_57b6693818620aec

-- | __unique:__ @test_manualarrays_Example_get_arr2_ptr@
foreign import ccall unsafe "hs_bindgen_928e8bd0dc2d0be1" hs_bindgen_928e8bd0dc2d0be1 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2_ptr #-}

{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @manual\/arrays.h:16:12@

__exported by:__ @manual\/arrays.h@
-}
arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_928e8bd0dc2d0be1

-- | __unique:__ @test_manualarrays_Example_get_arr3_ptr@
foreign import ccall unsafe "hs_bindgen_e6493a6ad08768b6" hs_bindgen_e6493a6ad08768b6 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr3_ptr #-}

{-| Global, extern, incomplete

__C declaration:__ @arr3@

__defined at:__ @manual\/arrays.h:19:12@

__exported by:__ @manual\/arrays.h@
-}
arr3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e6493a6ad08768b6

-- | __unique:__ @test_manualarrays_Example_get_sudoku_ptr@
foreign import ccall unsafe "hs_bindgen_e692ad991693d041" hs_bindgen_e692ad991693d041 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE sudoku_ptr #-}

{-| Multi-dimensional array of known size.

__C declaration:__ @sudoku@

__defined at:__ @manual\/arrays.h:22:12@

__exported by:__ @manual\/arrays.h@
-}
sudoku_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
sudoku_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e692ad991693d041

-- | __unique:__ @test_manualarrays_Example_get_triplets_ptr@
foreign import ccall unsafe "hs_bindgen_04df9f2a3065bb07" hs_bindgen_04df9f2a3065bb07 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE triplets_ptr #-}

{-| Multi-dimensional array of unknown size. Only the first dimension is allowed to be unknown.

__C declaration:__ @triplets@

__defined at:__ @manual\/arrays.h:26:12@

__exported by:__ @manual\/arrays.h@
-}
triplets_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
triplets_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_04df9f2a3065bb07

-- | __unique:__ @test_manualarrays_Example_get_global_triplet_ptrs_ptr@
foreign import ccall unsafe "hs_bindgen_7d8d8251f0367f2e" hs_bindgen_7d8d8251f0367f2e ::
     IO (Ptr.Ptr Triplet_ptrs)

{-# NOINLINE global_triplet_ptrs_ptr #-}

{-| A global of triplet_ptrs

__C declaration:__ @global_triplet_ptrs@

__defined at:__ @manual\/arrays.h:47:21@

__exported by:__ @manual\/arrays.h@
-}
global_triplet_ptrs_ptr :: Ptr.Ptr Triplet_ptrs
global_triplet_ptrs_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7d8d8251f0367f2e
