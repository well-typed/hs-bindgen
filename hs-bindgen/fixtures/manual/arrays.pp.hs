{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Prelude (Eq, IO, Show)

$(HsBindgen.Runtime.Prelude.addCSource "#include <manual/arrays.h>\nvoid hs_bindgen_test_manualarrays_f3d0c8dd1a83b3d0 (triplet *arg1, triplet *arg2) { transpose(arg1, arg2); }\nvoid hs_bindgen_test_manualarrays_27135f4747eb87db (signed int (**arg1)[3]) { pretty_print_triplets(arg1); }\n/* get_transpose_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_manualarrays_fc1dad225b555299 (void)) (matrix arg1, matrix arg2) { return &transpose; } \n/* get_pretty_print_triplets_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_manualarrays_0b485cea747ee35d (void)) (triplet_ptrs arg1) { return &pretty_print_triplets; } \n/* get_arr1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_1693226264ba4aeb (void))[1] { return &arr1; } \n/* get_arr2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_dafcf99a73b93389 (void))[3] { return &arr2; } \n/* get_arr3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_ca1016acc3449dee (void))[] { return &arr3; } \n/* get_sudoku_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_76857c9492b9374d (void))[3][3] { return &sudoku; } \n/* get_triplets_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_manualarrays_76f4df4c63822352 (void))[][3] { return &triplets; } \n/* get_global_triplet_ptrs_ptr */ __attribute__ ((const)) triplet_ptrs *hs_bindgen_test_manualarrays_f5de5a56e036b125 (void) { return &global_triplet_ptrs; } \n")

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/arrays.h:32:13@

    __exported by:__ @manual\/arrays.h@
-}
newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @matrix@

    __defined at:__ @manual\/arrays.h:34:17@

    __exported by:__ @manual\/arrays.h@
-}
newtype Matrix = Matrix
  { un_Matrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| A typedef representing a an array of unknown size, where each element is a pointer to an array of known size 3, where each element is an int.

__C declaration:__ @triplet_ptrs@

__defined at:__ @manual\/arrays.h:44:15@

__exported by:__ @manual\/arrays.h@
-}
newtype Triplet_ptrs = Triplet_ptrs
  { un_Triplet_ptrs :: HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h:36:6@

    __exported by:__ @manual\/arrays.h@
-}
foreign import ccall safe "hs_bindgen_test_manualarrays_f3d0c8dd1a83b3d0" transpose_wrapper
  :: Ptr.Ptr Triplet
     {- ^ __C declaration:__ @input@
     -}
  -> Ptr.Ptr Triplet
     {- ^ __C declaration:__ @output@
     -}
  -> IO ()

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h:36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose :: Matrix -> Matrix -> IO ()
transpose =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr3 ->
                                                                                                  transpose_wrapper ptr3 ptr2))

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h:50:13@

__exported by:__ @manual\/arrays.h@
-}
foreign import ccall safe "hs_bindgen_test_manualarrays_27135f4747eb87db" pretty_print_triplets_wrapper
  :: Ptr.Ptr (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h:50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets :: Triplet_ptrs -> IO ()
pretty_print_triplets =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    pretty_print_triplets_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_manualarrays_fc1dad225b555299" hs_bindgen_test_manualarrays_fc1dad225b555299
  :: IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))

{-# NOINLINE transpose_ptr #-}

{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h:36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_fc1dad225b555299

foreign import ccall unsafe "hs_bindgen_test_manualarrays_0b485cea747ee35d" hs_bindgen_test_manualarrays_0b485cea747ee35d
  :: IO (Ptr.FunPtr (Triplet_ptrs -> IO ()))

{-# NOINLINE pretty_print_triplets_ptr #-}

{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h:50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets_ptr :: Ptr.FunPtr (Triplet_ptrs -> IO ())
pretty_print_triplets_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_0b485cea747ee35d

foreign import ccall unsafe "hs_bindgen_test_manualarrays_1693226264ba4aeb" hs_bindgen_test_manualarrays_1693226264ba4aeb
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr1_ptr #-}

{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @manual\/arrays.h:13:12@

__exported by:__ @manual\/arrays.h@
-}
arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_1693226264ba4aeb

foreign import ccall unsafe "hs_bindgen_test_manualarrays_dafcf99a73b93389" hs_bindgen_test_manualarrays_dafcf99a73b93389
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2_ptr #-}

{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @manual\/arrays.h:16:12@

__exported by:__ @manual\/arrays.h@
-}
arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_dafcf99a73b93389

foreign import ccall unsafe "hs_bindgen_test_manualarrays_ca1016acc3449dee" hs_bindgen_test_manualarrays_ca1016acc3449dee
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr3_ptr #-}

{-| Global, extern, incomplete

__C declaration:__ @arr3@

__defined at:__ @manual\/arrays.h:19:12@

__exported by:__ @manual\/arrays.h@
-}
arr3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_ca1016acc3449dee

foreign import ccall unsafe "hs_bindgen_test_manualarrays_76857c9492b9374d" hs_bindgen_test_manualarrays_76857c9492b9374d
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE sudoku_ptr #-}

{-| Multi-dimensional array of known size.

__C declaration:__ @sudoku@

__defined at:__ @manual\/arrays.h:22:12@

__exported by:__ @manual\/arrays.h@
-}
sudoku_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
sudoku_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_76857c9492b9374d

foreign import ccall unsafe "hs_bindgen_test_manualarrays_76f4df4c63822352" hs_bindgen_test_manualarrays_76f4df4c63822352
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE triplets_ptr #-}

{-| Multi-dimensional array of unknown size. Only the first dimension is allowed to be unknown.

__C declaration:__ @triplets@

__defined at:__ @manual\/arrays.h:26:12@

__exported by:__ @manual\/arrays.h@
-}
triplets_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
triplets_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_76f4df4c63822352

foreign import ccall unsafe "hs_bindgen_test_manualarrays_f5de5a56e036b125" hs_bindgen_test_manualarrays_f5de5a56e036b125
  :: IO (Ptr.Ptr Triplet_ptrs)

{-# NOINLINE global_triplet_ptrs_ptr #-}

{-| A global of triplet_ptrs

__C declaration:__ @global_triplet_ptrs@

__defined at:__ @manual\/arrays.h:47:21@

__exported by:__ @manual\/arrays.h@
-}
global_triplet_ptrs_ptr :: Ptr.Ptr Triplet_ptrs
global_triplet_ptrs_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_manualarrays_f5de5a56e036b125
