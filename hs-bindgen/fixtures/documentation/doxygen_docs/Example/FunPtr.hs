{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <documentation/doxygen_docs.h>"
  , "/* test_documentationdoxygen_docs_Example_get_process_data */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4a5cd66a4f26f8d5 (void)) ("
  , "  uint8_t const *arg1,"
  , "  uint8_t *arg2,"
  , "  size_t *arg3"
  , ")"
  , "{"
  , "  return &process_data;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_process_file */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_c8a059c65f18fea9 (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &process_file;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_calculate_value */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5ab7e06724867ab3 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &calculate_value;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_html_example */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_3f74fb834b0cd46b (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &html_example;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_list_example */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_2175b3c627db39cf (void)) ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &list_example;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_dangerous_function */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_b7a4debd18827a19 (void)) ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return &dangerous_function;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_detailed_return_codes */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9700b22f82eedafe (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &detailed_return_codes;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_old_function */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_49e0d34a627c6c19 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &old_function;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_versioned_function */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_180cd7537e40ce99 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &versioned_function;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_process_buffer */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f4ccc6d90e8d3ebd (void)) ("
  , "  char arg1[64],"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &process_buffer;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_my_memcpy */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_6a74d376c901b531 (void)) ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &my_memcpy;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_double_value */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_156a92f3c5176105 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &double_value;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_complex_function */"
  , "__attribute__ ((const))"
  , "status_code_t (*hs_bindgen_dfa39b6be50cb2ca (void)) ("
  , "  config_t *arg1,"
  , "  uint8_t const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &complex_function;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_hash */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8ad88b79fd71f9d4 (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &hash;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_square */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7ede0f7ec1b30650 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  ]))

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_data@
foreign import ccall unsafe "hs_bindgen_4a5cd66a4f26f8d5" hs_bindgen_4a5cd66a4f26f8d5_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_data@
hs_bindgen_4a5cd66a4f26f8d5 :: IO (Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8) -> (Ptr.Ptr HsBindgen.Runtime.LibC.Word8) -> (Ptr.Ptr HsBindgen.Runtime.LibC.CSize) -> IO FC.CInt))
hs_bindgen_4a5cd66a4f26f8d5 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_4a5cd66a4f26f8d5_base

{-# NOINLINE process_data #-}
{-|

  Function with detailed parameter documentation

  This function shows different parameter directions and types.

  [__@input_data@ /(input)/__]: Input data buffer

  [__@output_data@ /(output)/__]: Output data buffer

  [__@size@ /(input,output)/__]: Size of data, updated on return

  __returns:__ Status code (0 = success, -1 = error)

__C declaration:__ @process_data@

__defined at:__ @documentation\/doxygen_docs.h 105:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_data :: Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8) -> (Ptr.Ptr HsBindgen.Runtime.LibC.Word8) -> (Ptr.Ptr HsBindgen.Runtime.LibC.CSize) -> IO FC.CInt)
process_data =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4a5cd66a4f26f8d5

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_file@
foreign import ccall unsafe "hs_bindgen_c8a059c65f18fea9" hs_bindgen_c8a059c65f18fea9_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_file@
hs_bindgen_c8a059c65f18fea9 :: IO (Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst FC.CChar) -> IO FC.CBool))
hs_bindgen_c8a059c65f18fea9 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_c8a059c65f18fea9_base

{-# NOINLINE process_file #-}
{-|

  Function with inline commands and formatting

  This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

  [__@filename@ /(input)/__]: The @char*@ filename to process

  __returns:__ @true@ if successful, @false@ otherwise

__C declaration:__ @process_file@

__defined at:__ @documentation\/doxygen_docs.h 116:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_file :: Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst FC.CChar) -> IO FC.CBool)
process_file =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c8a059c65f18fea9

-- __unique:__ @test_documentationdoxygen_docs_Example_get_calculate_value@
foreign import ccall unsafe "hs_bindgen_5ab7e06724867ab3" hs_bindgen_5ab7e06724867ab3_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_calculate_value@
hs_bindgen_5ab7e06724867ab3 :: IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))
hs_bindgen_5ab7e06724867ab3 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5ab7e06724867ab3_base

{-# NOINLINE calculate_value #-}
{-|

  Function with verbatim code blocks

  Example usage:

  @
  int result = calculate_value(10, 20);
  printf("Result: %d@n", result);
  @

  [__@base@ /(input)/__]: Base value

  [__@multiplier@ /(input)/__]: Multiplier value

  __returns:__ Calculated result

__C declaration:__ @calculate_value@

__defined at:__ @documentation\/doxygen_docs.h 131:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
calculate_value :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
calculate_value =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5ab7e06724867ab3

-- __unique:__ @test_documentationdoxygen_docs_Example_get_html_example@
foreign import ccall unsafe "hs_bindgen_3f74fb834b0cd46b" hs_bindgen_3f74fb834b0cd46b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_html_example@
hs_bindgen_3f74fb834b0cd46b :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CBool))
hs_bindgen_3f74fb834b0cd46b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3f74fb834b0cd46b_base

{-# NOINLINE html_example #-}
{-|

  Function with HTML formatting

  This function demonstrates HTML bold and italic text. It also shows HTML code formatting.

  Input Output 0 false 1 true

  [__@value@ /(input)/__]: Input value

  __returns:__ Boolean result

__C declaration:__ @html_example@

__defined at:__ @documentation\/doxygen_docs.h 148:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
html_example :: Ptr.FunPtr (FC.CInt -> IO FC.CBool)
html_example =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3f74fb834b0cd46b

-- __unique:__ @test_documentationdoxygen_docs_Example_get_list_example@
foreign import ccall unsafe "hs_bindgen_2175b3c627db39cf" hs_bindgen_2175b3c627db39cf_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_list_example@
hs_bindgen_2175b3c627db39cf :: IO (Ptr.FunPtr ((Ptr.Ptr (HsBindgen.Runtime.PtrConst.PtrConst FC.CChar)) -> HsBindgen.Runtime.LibC.CSize -> IO FC.CBool))
hs_bindgen_2175b3c627db39cf =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2175b3c627db39cf_base

{-# NOINLINE list_example #-}
{-|

  Function with lists and special formatting

  This function demonstrates:

  * Bullet point lists

  * Nested list item 1

  * Nested list item 2

  * Multiple items

  * Nested formatting

  Numbered list:

  1. First @item@

  1. item

  2. Second __item__

  3. Third item

  Other numbered list:

  1. A

  2. B

  3. C

  [__@items@ /(input)/__]: Array of items

  [__@count@ /(input)/__]: Number of items

  __returns:__ Success status

__C declaration:__ @list_example@

__defined at:__ @documentation\/doxygen_docs.h 174:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
list_example :: Ptr.FunPtr ((Ptr.Ptr (HsBindgen.Runtime.PtrConst.PtrConst FC.CChar)) -> HsBindgen.Runtime.LibC.CSize -> IO FC.CBool)
list_example =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2175b3c627db39cf

-- __unique:__ @test_documentationdoxygen_docs_Example_get_dangerous_function@
foreign import ccall unsafe "hs_bindgen_b7a4debd18827a19" hs_bindgen_b7a4debd18827a19_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_dangerous_function@
hs_bindgen_b7a4debd18827a19 :: IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void)))
hs_bindgen_b7a4debd18827a19 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_b7a4debd18827a19_base

{-# NOINLINE dangerous_function #-}
{-|

  Function with warnings and notes

  __/WARNING:/__ This function may cause side effects

  __Note:__ Use with caution in multithreaded environments

  __see:__ related_function() for similar functionality

  [__@ptr@ /(input)/__]: Pointer to data

  __returns:__ Modified pointer

__C declaration:__ @dangerous_function@

__defined at:__ @documentation\/doxygen_docs.h 186:7@

__exported by:__ @documentation\/doxygen_docs.h@
-}
dangerous_function :: Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void))
dangerous_function =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b7a4debd18827a19

-- __unique:__ @test_documentationdoxygen_docs_Example_get_detailed_return_codes@
foreign import ccall unsafe "hs_bindgen_9700b22f82eedafe" hs_bindgen_9700b22f82eedafe_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_detailed_return_codes@
hs_bindgen_9700b22f82eedafe :: IO (Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst FC.CChar) -> IO FC.CInt))
hs_bindgen_9700b22f82eedafe =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9700b22f82eedafe_base

{-# NOINLINE detailed_return_codes #-}
{-|

  Function with return value details

  [__@input@ /(input)/__]: Input string

  __returns:__ 0 Success

  __returns:__ -1 Invalid input

  __returns:__ -2 Memory allocation failed

  __returns:__ -3 Processing error

__C declaration:__ @detailed_return_codes@

__defined at:__ @documentation\/doxygen_docs.h 197:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
detailed_return_codes :: Ptr.FunPtr ((HsBindgen.Runtime.PtrConst.PtrConst FC.CChar) -> IO FC.CInt)
detailed_return_codes =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9700b22f82eedafe

-- __unique:__ @test_documentationdoxygen_docs_Example_get_old_function@
foreign import ccall unsafe "hs_bindgen_49e0d34a627c6c19" hs_bindgen_49e0d34a627c6c19_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_old_function@
hs_bindgen_49e0d34a627c6c19 :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_49e0d34a627c6c19 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_49e0d34a627c6c19_base

{-# NOINLINE old_function #-}
{-|

  Function with deprecated annotation

  __deprecated:__ Use new_function() instead

  [__@old_param@ /(input)/__]: Legacy parameter

  __returns:__ Legacy result

__C declaration:__ @old_function@

__defined at:__ @documentation\/doxygen_docs.h 206:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
old_function :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
old_function =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_49e0d34a627c6c19

-- __unique:__ @test_documentationdoxygen_docs_Example_get_versioned_function@
foreign import ccall unsafe "hs_bindgen_180cd7537e40ce99" hs_bindgen_180cd7537e40ce99_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_versioned_function@
hs_bindgen_180cd7537e40ce99 :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_180cd7537e40ce99 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_180cd7537e40ce99_base

{-# NOINLINE versioned_function #-}
{-|

  Function with version information

  @since:  1.0

  [__@data@ /(input)/__]: Input data

  __returns:__ Processed data

__C declaration:__ @versioned_function@

__defined at:__ @documentation\/doxygen_docs.h 216:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
versioned_function :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
versioned_function =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_180cd7537e40ce99

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_buffer@
foreign import ccall unsafe "hs_bindgen_f4ccc6d90e8d3ebd" hs_bindgen_f4ccc6d90e8d3ebd_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_buffer@
hs_bindgen_f4ccc6d90e8d3ebd :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.LibC.CSize -> IO FC.CInt))
hs_bindgen_f4ccc6d90e8d3ebd =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f4ccc6d90e8d3ebd_base

{-# NOINLINE process_buffer #-}
{-|

  Static array parameter

  [__@buffer@ /(input)/__]: Buffer with minimum size

  [__@size@ /(input)/__]: Actual buffer size

  __returns:__ Number of bytes written

__C declaration:__ @process_buffer@

__defined at:__ @documentation\/doxygen_docs.h 332:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_buffer :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.LibC.CSize -> IO FC.CInt)
process_buffer =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f4ccc6d90e8d3ebd

-- __unique:__ @test_documentationdoxygen_docs_Example_get_my_memcpy@
foreign import ccall unsafe "hs_bindgen_6a74d376c901b531" hs_bindgen_6a74d376c901b531_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_my_memcpy@
hs_bindgen_6a74d376c901b531 :: IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (HsBindgen.Runtime.PtrConst.PtrConst Void) -> HsBindgen.Runtime.LibC.CSize -> IO (Ptr.Ptr Void)))
hs_bindgen_6a74d376c901b531 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6a74d376c901b531_base

{-# NOINLINE my_memcpy #-}
{-|

  Function with restrict pointers

  [__@dest@ /(input)/__]: Destination buffer (restrict)

  [__@src@ /(input)/__]: Source buffer (restrict)

  [__@n@ /(input)/__]: Number of bytes

  __returns:__ Destination pointer

__C declaration:__ @my_memcpy@

__defined at:__ @documentation\/doxygen_docs.h 342:7@

__exported by:__ @documentation\/doxygen_docs.h@
-}
my_memcpy :: Ptr.FunPtr ((Ptr.Ptr Void) -> (HsBindgen.Runtime.PtrConst.PtrConst Void) -> HsBindgen.Runtime.LibC.CSize -> IO (Ptr.Ptr Void))
my_memcpy =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6a74d376c901b531

-- __unique:__ @test_documentationdoxygen_docs_Example_get_double_value@
foreign import ccall unsafe "hs_bindgen_156a92f3c5176105" hs_bindgen_156a92f3c5176105_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_double_value@
hs_bindgen_156a92f3c5176105 :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_156a92f3c5176105 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_156a92f3c5176105_base

{-# NOINLINE double_value #-}
{-|

  Inline function

  [__@x@ /(input)/__]: Input value

  __returns:__ Doubled value

__C declaration:__ @double_value@

__defined at:__ @documentation\/doxygen_docs.h 350:19@

__exported by:__ @documentation\/doxygen_docs.h@
-}
double_value :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
double_value =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_156a92f3c5176105

-- __unique:__ @test_documentationdoxygen_docs_Example_get_complex_function@
foreign import ccall unsafe "hs_bindgen_dfa39b6be50cb2ca" hs_bindgen_dfa39b6be50cb2ca_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_complex_function@
hs_bindgen_dfa39b6be50cb2ca :: IO (Ptr.FunPtr ((Ptr.Ptr Config_t) -> (HsBindgen.Runtime.PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8) -> HsBindgen.Runtime.LibC.CSize -> IO Status_code_t))
hs_bindgen_dfa39b6be50cb2ca =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_dfa39b6be50cb2ca_base

{-# NOINLINE complex_function #-}
{-|

  Function with complex documentation

  This function demonstrates multiple documentation features:

  __Description:__

  Performs complex data processing with multiple steps.

  __Algorithm:__

  10. Validate input parameters

  200. Allocate temporary buffers

  3000. Process data in chunks

  41235. Clean up resources

  __Algorithm2:__

  * Validate input parameters

  * Allocate temporary buffers

  * Process data in chunks

  * Clean up resources

  __Example:__

  @
  config_t cfg = {
  .id = 1,
  .name = "test",
  .flags = 0,
  .callback = my_callback,
  .user_data = NULL
  };

  status_code_t result = complex_function(&cfg, data, size);
  if (result != STATUS_OK) {
  handle_error(result);
  }
  @

  [__@config@ /(input)/__]: Configuration structure (see 'Config_t' )

  [__@data@ /(input)/__]: Input data buffer

  [__@size@ /(input)/__]: Size of input data

  __returns:__ Status code indicating success or failure

  __pre condition:__ config must not be NULL

  __pre condition:__ data must not be NULL if size > 0

  __post condition:__ Output data is written to config->user_data

  __/WARNING:/__ May return NULL if memory allocation fails

  __/WARNING:/__ Sets errno to EINVAL if parameters are invalid

__C declaration:__ @complex_function@

__defined at:__ @documentation\/doxygen_docs.h 423:15@

__exported by:__ @documentation\/doxygen_docs.h@
-}
complex_function :: Ptr.FunPtr ((Ptr.Ptr Config_t) -> (HsBindgen.Runtime.PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8) -> HsBindgen.Runtime.LibC.CSize -> IO Status_code_t)
complex_function =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dfa39b6be50cb2ca

-- __unique:__ @test_documentationdoxygen_docs_Example_get_hash@
foreign import ccall unsafe "hs_bindgen_8ad88b79fd71f9d4" hs_bindgen_8ad88b79fd71f9d4_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_hash@
hs_bindgen_8ad88b79fd71f9d4 :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))
hs_bindgen_8ad88b79fd71f9d4 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8ad88b79fd71f9d4_base

{-# NOINLINE hash #-}
{-| __C declaration:__ @hash@

    __defined at:__ @documentation\/doxygen_docs.h 427:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
hash :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
hash =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8ad88b79fd71f9d4

-- __unique:__ @test_documentationdoxygen_docs_Example_get_square@
foreign import ccall unsafe "hs_bindgen_7ede0f7ec1b30650" hs_bindgen_7ede0f7ec1b30650_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_square@
hs_bindgen_7ede0f7ec1b30650 :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))
hs_bindgen_7ede0f7ec1b30650 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7ede0f7ec1b30650_base

{-# NOINLINE square #-}
{-| __C declaration:__ @square@

    __defined at:__ @documentation\/doxygen_docs.h 429:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
square :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7ede0f7ec1b30650
