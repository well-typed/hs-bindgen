{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <arrays/array.h>"
  , "/* test_arraysarray_Example_get_arr0_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_dd2a346b95b769db (void))[3]"
  , "{"
  , "  return &arr0;"
  , "}"
  , "/* test_arraysarray_Example_get_arr1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3867a46f740e141f (void))[3]"
  , "{"
  , "  return &arr1;"
  , "}"
  , "/* test_arraysarray_Example_get_arr2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c1b5868da3cfebbe (void))[3]"
  , "{"
  , "  return &arr2;"
  , "}"
  , "/* test_arraysarray_Example_get_arr3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_697b55cf10c5c7ae (void))[3]"
  , "{"
  , "  return &arr3;"
  , "}"
  , "/* test_arraysarray_Example_get_arr6_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f935cbe0a13b4987 (void))[1]"
  , "{"
  , "  return &arr6;"
  , "}"
  , "/* test_arraysarray_Example_get_arr7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_673085071176d81a (void))[]"
  , "{"
  , "  return &arr7;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3cf195887769eb3d (void))[3]"
  , "{"
  , "  return &arr_1;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_2_ptr */"
  , "__attribute__ ((const))"
  , "triplet *hs_bindgen_4621cb499a2b4cd3 (void)"
  , "{"
  , "  return &arr_2;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_cb7148df8f0668ef (void))[]"
  , "{"
  , "  return &arr_3;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_4_ptr */"
  , "__attribute__ ((const))"
  , "list *hs_bindgen_f0a4984c74b89803 (void)"
  , "{"
  , "  return &arr_4;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_5_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9f555ad1567e295a (void))[4][3]"
  , "{"
  , "  return &arr_5;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_6_ptr */"
  , "__attribute__ ((const))"
  , "matrix *hs_bindgen_f016939597566966 (void)"
  , "{"
  , "  return &arr_6;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_bb1876e9c2ece223 (void))[][3]"
  , "{"
  , "  return &arr_7;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_8_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist *hs_bindgen_dc2a31e3f871adec (void)"
  , "{"
  , "  return &arr_8;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_1_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_fc9438d00f745eee (void))[3]"
  , "{"
  , "  return &arr_1_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_2_const_ptr */"
  , "__attribute__ ((const))"
  , "triplet const *hs_bindgen_be3eba6be1a73c5d (void)"
  , "{"
  , "  return &arr_2_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_3_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_0c3c415a6bdd56a6 (void))[]"
  , "{"
  , "  return &arr_3_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_4_const_ptr */"
  , "__attribute__ ((const))"
  , "list const *hs_bindgen_a6fa7483b9d48043 (void)"
  , "{"
  , "  return &arr_4_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_5_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_93b436ac5ffd8c82 (void))[4][3]"
  , "{"
  , "  return &arr_5_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_6_const_ptr */"
  , "__attribute__ ((const))"
  , "matrix const *hs_bindgen_9e625256c9dc1a3f (void)"
  , "{"
  , "  return &arr_6_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_7_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_19cc3c6537ef51f0 (void))[][3]"
  , "{"
  , "  return &arr_7_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_8_const_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist const *hs_bindgen_9af0285a476aaf26 (void)"
  , "{"
  , "  return &arr_8_const;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dd2a346b95b769db" hs_bindgen_dd2a346b95b769db_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr0_ptr@
hs_bindgen_dd2a346b95b769db ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_dd2a346b95b769db =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dd2a346b95b769db_base

{-# NOINLINE arr0_ptr #-}

{-| Global, complete, not initialised

__C declaration:__ @arr0@

__defined at:__ @arrays\/array.h:11:5@

__exported by:__ @arrays\/array.h@
-}
arr0_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dd2a346b95b769db

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3867a46f740e141f" hs_bindgen_3867a46f740e141f_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr1_ptr@
hs_bindgen_3867a46f740e141f ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_3867a46f740e141f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3867a46f740e141f_base

{-# NOINLINE arr1_ptr #-}

{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @arrays\/array.h:14:5@

__exported by:__ @arrays\/array.h@
-}
arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3867a46f740e141f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c1b5868da3cfebbe" hs_bindgen_c1b5868da3cfebbe_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr2_ptr@
hs_bindgen_c1b5868da3cfebbe ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_c1b5868da3cfebbe =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c1b5868da3cfebbe_base

{-# NOINLINE arr2_ptr #-}

{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @arrays\/array.h:17:12@

__exported by:__ @arrays\/array.h@
-}
arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c1b5868da3cfebbe

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_697b55cf10c5c7ae" hs_bindgen_697b55cf10c5c7ae_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr3_ptr@
hs_bindgen_697b55cf10c5c7ae ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_697b55cf10c5c7ae =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_697b55cf10c5c7ae_base

{-# NOINLINE arr3_ptr #-}

{-| Global, extern, complete, initialised

__C declaration:__ @arr3@

__defined at:__ @arrays\/array.h:20:12@

__exported by:__ @arrays\/array.h@
-}
arr3_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_697b55cf10c5c7ae

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f935cbe0a13b4987" hs_bindgen_f935cbe0a13b4987_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr6_ptr@
hs_bindgen_f935cbe0a13b4987 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))
hs_bindgen_f935cbe0a13b4987 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f935cbe0a13b4987_base

{-# NOINLINE arr6_ptr #-}

{-| Global, incomplete

__C declaration:__ @arr6@

__defined at:__ @arrays\/array.h:29:5@

__exported by:__ @arrays\/array.h@
-}
arr6_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f935cbe0a13b4987

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_673085071176d81a" hs_bindgen_673085071176d81a_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr7_ptr@
hs_bindgen_673085071176d81a ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
hs_bindgen_673085071176d81a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_673085071176d81a_base

{-# NOINLINE arr7_ptr #-}

{-| Global, extern, incomplete

__C declaration:__ @arr7@

__defined at:__ @arrays\/array.h:32:12@

__exported by:__ @arrays\/array.h@
-}
arr7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_673085071176d81a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3cf195887769eb3d" hs_bindgen_3cf195887769eb3d_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_1_ptr@
hs_bindgen_3cf195887769eb3d ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_3cf195887769eb3d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3cf195887769eb3d_base

{-# NOINLINE arr_1_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1@

__defined at:__ @arrays\/array.h:62:12@

__exported by:__ @arrays\/array.h@
-}
arr_1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3cf195887769eb3d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4621cb499a2b4cd3" hs_bindgen_4621cb499a2b4cd3_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_2_ptr@
hs_bindgen_4621cb499a2b4cd3 ::
     IO (Ptr.Ptr Triplet)
hs_bindgen_4621cb499a2b4cd3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4621cb499a2b4cd3_base

{-# NOINLINE arr_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2@

__defined at:__ @arrays\/array.h:65:16@

__exported by:__ @arrays\/array.h@
-}
arr_2_ptr :: Ptr.Ptr Triplet
arr_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4621cb499a2b4cd3

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cb7148df8f0668ef" hs_bindgen_cb7148df8f0668ef_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_3_ptr@
hs_bindgen_cb7148df8f0668ef ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
hs_bindgen_cb7148df8f0668ef =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_cb7148df8f0668ef_base

{-# NOINLINE arr_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3@

__defined at:__ @arrays\/array.h:68:12@

__exported by:__ @arrays\/array.h@
-}
arr_3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cb7148df8f0668ef

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f0a4984c74b89803" hs_bindgen_f0a4984c74b89803_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_4_ptr@
hs_bindgen_f0a4984c74b89803 ::
     IO (Ptr.Ptr List)
hs_bindgen_f0a4984c74b89803 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f0a4984c74b89803_base

{-# NOINLINE arr_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4@

__defined at:__ @arrays\/array.h:71:13@

__exported by:__ @arrays\/array.h@
-}
arr_4_ptr :: Ptr.Ptr List
arr_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f0a4984c74b89803

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9f555ad1567e295a" hs_bindgen_9f555ad1567e295a_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_5_ptr@
hs_bindgen_9f555ad1567e295a ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
hs_bindgen_9f555ad1567e295a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9f555ad1567e295a_base

{-# NOINLINE arr_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5@

__defined at:__ @arrays\/array.h:74:12@

__exported by:__ @arrays\/array.h@
-}
arr_5_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9f555ad1567e295a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f016939597566966" hs_bindgen_f016939597566966_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_6_ptr@
hs_bindgen_f016939597566966 ::
     IO (Ptr.Ptr Matrix)
hs_bindgen_f016939597566966 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f016939597566966_base

{-# NOINLINE arr_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6@

__defined at:__ @arrays\/array.h:77:15@

__exported by:__ @arrays\/array.h@
-}
arr_6_ptr :: Ptr.Ptr Matrix
arr_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f016939597566966

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bb1876e9c2ece223" hs_bindgen_bb1876e9c2ece223_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_7_ptr@
hs_bindgen_bb1876e9c2ece223 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
hs_bindgen_bb1876e9c2ece223 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_bb1876e9c2ece223_base

{-# NOINLINE arr_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7@

__defined at:__ @arrays\/array.h:80:12@

__exported by:__ @arrays\/array.h@
-}
arr_7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bb1876e9c2ece223

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dc2a31e3f871adec" hs_bindgen_dc2a31e3f871adec_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_8_ptr@
hs_bindgen_dc2a31e3f871adec ::
     IO (Ptr.Ptr Tripletlist)
hs_bindgen_dc2a31e3f871adec =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dc2a31e3f871adec_base

{-# NOINLINE arr_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8@

__defined at:__ @arrays\/array.h:83:20@

__exported by:__ @arrays\/array.h@
-}
arr_8_ptr :: Ptr.Ptr Tripletlist
arr_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dc2a31e3f871adec

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_fc9438d00f745eee" hs_bindgen_fc9438d00f745eee_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_1_const_ptr@
hs_bindgen_fc9438d00f745eee ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_fc9438d00f745eee =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_fc9438d00f745eee_base

{-# NOINLINE arr_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1_const@

__defined at:__ @arrays\/array.h:90:18@

__exported by:__ @arrays\/array.h@
-}
arr_1_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fc9438d00f745eee

{-# NOINLINE arr_1_const #-}

arr_1_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
arr_1_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr arr_1_const_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_be3eba6be1a73c5d" hs_bindgen_be3eba6be1a73c5d_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_2_const_ptr@
hs_bindgen_be3eba6be1a73c5d ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Triplet)
hs_bindgen_be3eba6be1a73c5d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_be3eba6be1a73c5d_base

{-# NOINLINE arr_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2_const@

__defined at:__ @arrays\/array.h:93:22@

__exported by:__ @arrays\/array.h@
-}
arr_2_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
arr_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_be3eba6be1a73c5d

{-# NOINLINE arr_2_const #-}

arr_2_const :: Triplet
arr_2_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr arr_2_const_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_0c3c415a6bdd56a6" hs_bindgen_0c3c415a6bdd56a6_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_3_const_ptr@
hs_bindgen_0c3c415a6bdd56a6 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
hs_bindgen_0c3c415a6bdd56a6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_0c3c415a6bdd56a6_base

{-# NOINLINE arr_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3_const@

__defined at:__ @arrays\/array.h:96:18@

__exported by:__ @arrays\/array.h@
-}
arr_3_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0c3c415a6bdd56a6

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a6fa7483b9d48043" hs_bindgen_a6fa7483b9d48043_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_4_const_ptr@
hs_bindgen_a6fa7483b9d48043 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr List)
hs_bindgen_a6fa7483b9d48043 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a6fa7483b9d48043_base

{-# NOINLINE arr_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4_const@

__defined at:__ @arrays\/array.h:99:19@

__exported by:__ @arrays\/array.h@
-}
arr_4_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr List
arr_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a6fa7483b9d48043

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_93b436ac5ffd8c82" hs_bindgen_93b436ac5ffd8c82_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_5_const_ptr@
hs_bindgen_93b436ac5ffd8c82 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
hs_bindgen_93b436ac5ffd8c82 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_93b436ac5ffd8c82_base

{-# NOINLINE arr_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5_const@

__defined at:__ @arrays\/array.h:102:18@

__exported by:__ @arrays\/array.h@
-}
arr_5_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_93b436ac5ffd8c82

{-# NOINLINE arr_5_const #-}

arr_5_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_5_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr arr_5_const_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9e625256c9dc1a3f" hs_bindgen_9e625256c9dc1a3f_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_6_const_ptr@
hs_bindgen_9e625256c9dc1a3f ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Matrix)
hs_bindgen_9e625256c9dc1a3f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9e625256c9dc1a3f_base

{-# NOINLINE arr_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6_const@

__defined at:__ @arrays\/array.h:105:21@

__exported by:__ @arrays\/array.h@
-}
arr_6_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr Matrix
arr_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9e625256c9dc1a3f

{-# NOINLINE arr_6_const #-}

arr_6_const :: Matrix
arr_6_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr arr_6_const_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_19cc3c6537ef51f0" hs_bindgen_19cc3c6537ef51f0_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_7_const_ptr@
hs_bindgen_19cc3c6537ef51f0 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
hs_bindgen_19cc3c6537ef51f0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_19cc3c6537ef51f0_base

{-# NOINLINE arr_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7_const@

__defined at:__ @arrays\/array.h:108:18@

__exported by:__ @arrays\/array.h@
-}
arr_7_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_19cc3c6537ef51f0

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9af0285a476aaf26" hs_bindgen_9af0285a476aaf26_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_arr_8_const_ptr@
hs_bindgen_9af0285a476aaf26 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Tripletlist)
hs_bindgen_9af0285a476aaf26 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9af0285a476aaf26_base

{-# NOINLINE arr_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8_const@

__defined at:__ @arrays\/array.h:111:26@

__exported by:__ @arrays\/array.h@
-}
arr_8_const_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr Tripletlist
arr_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9af0285a476aaf26
