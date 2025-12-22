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
foreign import ccall unsafe "hs_bindgen_6843a8f5c61ff74e" hs_bindgen_6843a8f5c61ff74e ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr1 #-}
{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @manual\/arrays.h:13:12@

__exported by:__ @manual\/arrays.h@
-}
arr1 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6843a8f5c61ff74e

-- __unique:__ @test_manualarrays_Example_get_arr2@
foreign import ccall unsafe "hs_bindgen_60c5bfbe2e29672a" hs_bindgen_60c5bfbe2e29672a ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2 #-}
{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @manual\/arrays.h:16:12@

__exported by:__ @manual\/arrays.h@
-}
arr2 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_60c5bfbe2e29672a

-- __unique:__ @test_manualarrays_Example_get_arr3@
foreign import ccall unsafe "hs_bindgen_a0497dc4c0aba158" hs_bindgen_a0497dc4c0aba158 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr3 #-}
{-| Global, extern, incomplete

__C declaration:__ @arr3@

__defined at:__ @manual\/arrays.h:19:12@

__exported by:__ @manual\/arrays.h@
-}
arr3 :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a0497dc4c0aba158

-- __unique:__ @test_manualarrays_Example_get_sudoku@
foreign import ccall unsafe "hs_bindgen_609c0f69bf6da356" hs_bindgen_609c0f69bf6da356 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE sudoku #-}
{-| Multi-dimensional array of known size.

__C declaration:__ @sudoku@

__defined at:__ @manual\/arrays.h:22:12@

__exported by:__ @manual\/arrays.h@
-}
sudoku :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
sudoku =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_609c0f69bf6da356

-- __unique:__ @test_manualarrays_Example_get_triplets@
foreign import ccall unsafe "hs_bindgen_52a30badbe2c9671" hs_bindgen_52a30badbe2c9671 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE triplets #-}
{-| Multi-dimensional array of unknown size. Only the first dimension is allowed to be unknown.

__C declaration:__ @triplets@

__defined at:__ @manual\/arrays.h:26:12@

__exported by:__ @manual\/arrays.h@
-}
triplets :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
triplets =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_52a30badbe2c9671

-- __unique:__ @test_manualarrays_Example_get_global_triplet_ptrs@
foreign import ccall unsafe "hs_bindgen_23817a1a9bc92057" hs_bindgen_23817a1a9bc92057 ::
     IO (Ptr.Ptr Triplet_ptrs)

{-# NOINLINE global_triplet_ptrs #-}
{-| A global of triplet_ptrs

__C declaration:__ @global_triplet_ptrs@

__defined at:__ @manual\/arrays.h:47:21@

__exported by:__ @manual\/arrays.h@
-}
global_triplet_ptrs :: Ptr.Ptr Triplet_ptrs
global_triplet_ptrs =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_23817a1a9bc92057
