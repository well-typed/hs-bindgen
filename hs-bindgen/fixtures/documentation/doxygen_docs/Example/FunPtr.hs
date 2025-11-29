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
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <documentation/doxygen_docs.h>"
  , "/* Example_get_process_data_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_d2668db72e056374 (void)) ("
  , "  uint8_t const *arg1,"
  , "  uint8_t *arg2,"
  , "  size_t *arg3"
  , ")"
  , "{"
  , "  return &process_data;"
  , "}"
  , "/* Example_get_process_file_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_documentationdoxygen_docs_ee9979a96b9e0ebb (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &process_file;"
  , "}"
  , "/* Example_get_calculate_value_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_b2d4acccebbbff83 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &calculate_value;"
  , "}"
  , "/* Example_get_html_example_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_documentationdoxygen_docs_6b2a4716982e2641 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &html_example;"
  , "}"
  , "/* Example_get_list_example_ptr */"
  , "__attribute__ ((const))"
  , "_Bool (*hs_bindgen_test_documentationdoxygen_docs_37cf534c55e80207 (void)) ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &list_example;"
  , "}"
  , "/* Example_get_dangerous_function_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_documentationdoxygen_docs_ad46af06eff9629d (void)) ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return &dangerous_function;"
  , "}"
  , "/* Example_get_detailed_return_codes_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_2ff0de7112ffdb09 (void)) ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return &detailed_return_codes;"
  , "}"
  , "/* Example_get_old_function_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_c2d8f5f8d87e423b (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &old_function;"
  , "}"
  , "/* Example_get_versioned_function_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_86636481b65b4ae9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &versioned_function;"
  , "}"
  , "/* Example_get_process_buffer_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_a3b64d5f8ab9969d (void)) ("
  , "  char arg1[64],"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &process_buffer;"
  , "}"
  , "/* Example_get_my_memcpy_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_documentationdoxygen_docs_d3b7a85b34dd0d4a (void)) ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &my_memcpy;"
  , "}"
  , "/* Example_get_double_value_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_48a446fc07a0c57f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &double_value;"
  , "}"
  , "/* Example_get_complex_function_ptr */"
  , "__attribute__ ((const))"
  , "status_code_t (*hs_bindgen_test_documentationdoxygen_docs_eb695dfabbcd3c2c (void)) ("
  , "  config_t *arg1,"
  , "  uint8_t const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &complex_function;"
  , "}"
  , "/* Example_get_hash_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_ad030d6582d9db0b (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &hash;"
  , "}"
  , "/* Example_get_square_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_documentationdoxygen_docs_61cbe351b243e6fe (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  ]))

{-| __unique:__ @Example_get_process_data_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_d2668db72e056374" hs_bindgen_test_documentationdoxygen_docs_d2668db72e056374 ::
     IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CSize) -> IO FC.CInt))

{-# NOINLINE process_data_ptr #-}

{-|

  Function with detailed parameter documentation

  This function shows different parameter directions and types.

  [__@input_data@ /(input)/__]: Input data buffer

  [__@output_data@ /(output)/__]: Output data buffer

  [__@size@ /(input,output)/__]: Size of data, updated on return

  __returns:__ Status code (0 = success, -1 = error)

__C declaration:__ @process_data@

__defined at:__ @documentation\/doxygen_docs.h:105:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_data_ptr :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CSize) -> IO FC.CInt)
process_data_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_d2668db72e056374

{-| __unique:__ @Example_get_process_file_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_ee9979a96b9e0ebb" hs_bindgen_test_documentationdoxygen_docs_ee9979a96b9e0ebb ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CBool))

{-# NOINLINE process_file_ptr #-}

{-|

  Function with inline commands and formatting

  This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

  [__@filename@ /(input)/__]: The @char*@ filename to process

  __returns:__ @true@ if successful, @false@ otherwise

__C declaration:__ @process_file@

__defined at:__ @documentation\/doxygen_docs.h:116:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_file_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CBool)
process_file_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_ee9979a96b9e0ebb

{-| __unique:__ @Example_get_calculate_value_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_b2d4acccebbbff83" hs_bindgen_test_documentationdoxygen_docs_b2d4acccebbbff83 ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE calculate_value_ptr #-}

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

__defined at:__ @documentation\/doxygen_docs.h:131:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
calculate_value_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
calculate_value_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_b2d4acccebbbff83

{-| __unique:__ @Example_get_html_example_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_6b2a4716982e2641" hs_bindgen_test_documentationdoxygen_docs_6b2a4716982e2641 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CBool))

{-# NOINLINE html_example_ptr #-}

{-|

  Function with HTML formatting

  This function demonstrates HTML bold and italic text. It also shows HTML code formatting.

  Input Output 0 false 1 true

  [__@value@ /(input)/__]: Input value

  __returns:__ Boolean result

__C declaration:__ @html_example@

__defined at:__ @documentation\/doxygen_docs.h:148:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
html_example_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CBool)
html_example_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_6b2a4716982e2641

{-| __unique:__ @Example_get_list_example_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_37cf534c55e80207" hs_bindgen_test_documentationdoxygen_docs_37cf534c55e80207 ::
     IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool))

{-# NOINLINE list_example_ptr #-}

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

__defined at:__ @documentation\/doxygen_docs.h:174:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
list_example_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool)
list_example_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_37cf534c55e80207

{-| __unique:__ @Example_get_dangerous_function_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_ad46af06eff9629d" hs_bindgen_test_documentationdoxygen_docs_ad46af06eff9629d ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void)))

{-# NOINLINE dangerous_function_ptr #-}

{-|

  Function with warnings and notes

  __/WARNING:/__ This function may cause side effects

  __Note:__ Use with caution in multithreaded environments

  __see:__ related_function() for similar functionality

  [__@ptr@ /(input)/__]: Pointer to data

  __returns:__ Modified pointer

__C declaration:__ @dangerous_function@

__defined at:__ @documentation\/doxygen_docs.h:186:7@

__exported by:__ @documentation\/doxygen_docs.h@
-}
dangerous_function_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void))
dangerous_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_ad46af06eff9629d

{-| __unique:__ @Example_get_detailed_return_codes_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_2ff0de7112ffdb09" hs_bindgen_test_documentationdoxygen_docs_2ff0de7112ffdb09 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE detailed_return_codes_ptr #-}

{-|

  Function with return value details

  [__@input@ /(input)/__]: Input string

  __returns:__ 0 Success

  __returns:__ -1 Invalid input

  __returns:__ -2 Memory allocation failed

  __returns:__ -3 Processing error

__C declaration:__ @detailed_return_codes@

__defined at:__ @documentation\/doxygen_docs.h:197:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
detailed_return_codes_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
detailed_return_codes_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_2ff0de7112ffdb09

{-| __unique:__ @Example_get_old_function_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_c2d8f5f8d87e423b" hs_bindgen_test_documentationdoxygen_docs_c2d8f5f8d87e423b ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE old_function_ptr #-}

{-|

  Function with deprecated annotation

  __deprecated:__ Use new_function() instead

  [__@old_param@ /(input)/__]: Legacy parameter

  __returns:__ Legacy result

__C declaration:__ @old_function@

__defined at:__ @documentation\/doxygen_docs.h:206:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
old_function_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
old_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_c2d8f5f8d87e423b

{-| __unique:__ @Example_get_versioned_function_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_86636481b65b4ae9" hs_bindgen_test_documentationdoxygen_docs_86636481b65b4ae9 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE versioned_function_ptr #-}

{-|

  Function with version information

  @since:  1.0

  [__@data@ /(input)/__]: Input data

  __returns:__ Processed data

__C declaration:__ @versioned_function@

__defined at:__ @documentation\/doxygen_docs.h:216:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
versioned_function_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
versioned_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_86636481b65b4ae9

{-| __unique:__ @Example_get_process_buffer_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_a3b64d5f8ab9969d" hs_bindgen_test_documentationdoxygen_docs_a3b64d5f8ab9969d ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt))

{-# NOINLINE process_buffer_ptr #-}

{-|

  Static array parameter

  [__@buffer@ /(input)/__]: Buffer with minimum size

  [__@size@ /(input)/__]: Actual buffer size

  __returns:__ Number of bytes written

__C declaration:__ @process_buffer@

__defined at:__ @documentation\/doxygen_docs.h:332:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_buffer_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt)
process_buffer_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_a3b64d5f8ab9969d

{-| __unique:__ @Example_get_my_memcpy_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_d3b7a85b34dd0d4a" hs_bindgen_test_documentationdoxygen_docs_d3b7a85b34dd0d4a ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memcpy_ptr #-}

{-|

  Function with restrict pointers

  [__@dest@ /(input)/__]: Destination buffer (restrict)

  [__@src@ /(input)/__]: Source buffer (restrict)

  [__@n@ /(input)/__]: Number of bytes

  __returns:__ Destination pointer

__C declaration:__ @my_memcpy@

__defined at:__ @documentation\/doxygen_docs.h:342:7@

__exported by:__ @documentation\/doxygen_docs.h@
-}
my_memcpy_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void))
my_memcpy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_d3b7a85b34dd0d4a

{-| __unique:__ @Example_get_double_value_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_48a446fc07a0c57f" hs_bindgen_test_documentationdoxygen_docs_48a446fc07a0c57f ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE double_value_ptr #-}

{-|

  Inline function

  [__@x@ /(input)/__]: Input value

  __returns:__ Doubled value

__C declaration:__ @double_value@

__defined at:__ @documentation\/doxygen_docs.h:350:19@

__exported by:__ @documentation\/doxygen_docs.h@
-}
double_value_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
double_value_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_48a446fc07a0c57f

{-| __unique:__ @Example_get_complex_function_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_eb695dfabbcd3c2c" hs_bindgen_test_documentationdoxygen_docs_eb695dfabbcd3c2c ::
     IO (Ptr.FunPtr ((Ptr.Ptr Config_t) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> IO Status_code_t))

{-# NOINLINE complex_function_ptr #-}

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

__defined at:__ @documentation\/doxygen_docs.h:423:15@

__exported by:__ @documentation\/doxygen_docs.h@
-}
complex_function_ptr :: Ptr.FunPtr ((Ptr.Ptr Config_t) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> IO Status_code_t)
complex_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_eb695dfabbcd3c2c

{-| __unique:__ @Example_get_hash_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_ad030d6582d9db0b" hs_bindgen_test_documentationdoxygen_docs_ad030d6582d9db0b ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE hash_ptr #-}

{-| __C declaration:__ @hash@

    __defined at:__ @documentation\/doxygen_docs.h:427:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
hash_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
hash_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_ad030d6582d9db0b

{-| __unique:__ @Example_get_square_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_61cbe351b243e6fe" hs_bindgen_test_documentationdoxygen_docs_61cbe351b243e6fe ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @documentation\/doxygen_docs.h:429:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_61cbe351b243e6fe
