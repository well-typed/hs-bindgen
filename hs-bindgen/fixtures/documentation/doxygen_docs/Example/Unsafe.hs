{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <documentation/doxygen_docs.h>"
  , "signed int hs_bindgen_e6085a910ba41ecb ("
  , "  uint8_t const *arg1,"
  , "  uint8_t *arg2,"
  , "  size_t *arg3"
  , ")"
  , "{"
  , "  return process_data(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_c27e893aea0b0a77 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return process_file(arg1);"
  , "}"
  , "signed int hs_bindgen_bc1b0e25a72f4ec0 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return calculate_value(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_09abc3cb74562964 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return html_example(arg1);"
  , "}"
  , "_Bool hs_bindgen_47cba1a95d265f84 ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return list_example(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_e065fc013e4eccd6 ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return dangerous_function(arg1);"
  , "}"
  , "signed int hs_bindgen_bc4f7e24b2ad4ace ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return detailed_return_codes(arg1);"
  , "}"
  , "signed int hs_bindgen_8deec146389ae8b3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return old_function(arg1);"
  , "}"
  , "signed int hs_bindgen_da2dcc1473935665 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return versioned_function(arg1);"
  , "}"
  , "signed int hs_bindgen_97c1191917e6eece ("
  , "  char *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return process_buffer(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_58253bb560dc3eb3 ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_44dd19b16ee38e5b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return double_value(arg1);"
  , "}"
  , "status_code_t hs_bindgen_fd6fce7c8d8b2f79 ("
  , "  config_t *arg1,"
  , "  uint8_t const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return complex_function(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_dd36c8b317ccfcc4 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "signed int hs_bindgen_6875e30a7fe8d30a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  ]))

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_data@
foreign import ccall unsafe "hs_bindgen_e6085a910ba41ecb" hs_bindgen_e6085a910ba41ecb_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_data@
hs_bindgen_e6085a910ba41ecb ::
     HsBindgen.Runtime.ConstPtr.ConstPtr HsBindgen.Runtime.Prelude.Word8
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.CSize
  -> IO FC.CInt
hs_bindgen_e6085a910ba41ecb =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e6085a910ba41ecb_base

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
process_data ::
     HsBindgen.Runtime.ConstPtr.ConstPtr HsBindgen.Runtime.Prelude.Word8
     {- ^

        [__@input_data@ /(input)/__]: Input data buffer

     __C declaration:__ @input_data@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^

        [__@output_data@ /(output)/__]: Output data buffer

     __C declaration:__ @output_data@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@size@ /(input,output)/__]: Size of data, updated on return

     __C declaration:__ @size@
     -}
  -> IO FC.CInt
process_data = hs_bindgen_e6085a910ba41ecb

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_file@
foreign import ccall unsafe "hs_bindgen_c27e893aea0b0a77" hs_bindgen_c27e893aea0b0a77_base ::
     Ptr.Ptr Void
  -> IO GHC.Word.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_file@
hs_bindgen_c27e893aea0b0a77 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
  -> IO FC.CBool
hs_bindgen_c27e893aea0b0a77 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c27e893aea0b0a77_base

{-|

  Function with inline commands and formatting

  This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

  [__@filename@ /(input)/__]: The @char*@ filename to process

  __returns:__ @true@ if successful, @false@ otherwise

__C declaration:__ @process_file@

__defined at:__ @documentation\/doxygen_docs.h 116:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_file ::
     HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
     {- ^

        [__@filename@ /(input)/__]: The @char*@ filename to process

     __C declaration:__ @filename@
     -}
  -> IO FC.CBool
process_file = hs_bindgen_c27e893aea0b0a77

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_calculate_value@
foreign import ccall unsafe "hs_bindgen_bc1b0e25a72f4ec0" hs_bindgen_bc1b0e25a72f4ec0_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_calculate_value@
hs_bindgen_bc1b0e25a72f4ec0 ::
     FC.CInt
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_bc1b0e25a72f4ec0 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bc1b0e25a72f4ec0_base

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
calculate_value ::
     FC.CInt
     {- ^

        [__@base@ /(input)/__]: Base value

     __C declaration:__ @base@
     -}
  -> FC.CInt
     {- ^

        [__@multiplier@ /(input)/__]: Multiplier value

     __C declaration:__ @multiplier@
     -}
  -> IO FC.CInt
calculate_value = hs_bindgen_bc1b0e25a72f4ec0

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_html_example@
foreign import ccall unsafe "hs_bindgen_09abc3cb74562964" hs_bindgen_09abc3cb74562964_base ::
     GHC.Int.Int32
  -> IO GHC.Word.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_html_example@
hs_bindgen_09abc3cb74562964 ::
     FC.CInt
  -> IO FC.CBool
hs_bindgen_09abc3cb74562964 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_09abc3cb74562964_base

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
html_example ::
     FC.CInt
     {- ^

        [__@value@ /(input)/__]: Input value

     __C declaration:__ @value@
     -}
  -> IO FC.CBool
html_example = hs_bindgen_09abc3cb74562964

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_list_example@
foreign import ccall unsafe "hs_bindgen_47cba1a95d265f84" hs_bindgen_47cba1a95d265f84_base ::
     Ptr.Ptr Void
  -> GHC.Word.Word64
  -> IO GHC.Word.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_list_example@
hs_bindgen_47cba1a95d265f84 ::
     Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar)
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO FC.CBool
hs_bindgen_47cba1a95d265f84 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_47cba1a95d265f84_base

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
list_example ::
     Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar)
     {- ^

        [__@items@ /(input)/__]: Array of items

     __C declaration:__ @items@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@count@ /(input)/__]: Number of items

     __C declaration:__ @count@
     -}
  -> IO FC.CBool
list_example = hs_bindgen_47cba1a95d265f84

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_dangerous_function@
foreign import ccall unsafe "hs_bindgen_e065fc013e4eccd6" hs_bindgen_e065fc013e4eccd6_base ::
     Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_dangerous_function@
hs_bindgen_e065fc013e4eccd6 ::
     Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)
hs_bindgen_e065fc013e4eccd6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e065fc013e4eccd6_base

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
dangerous_function ::
     Ptr.Ptr Void
     {- ^

        [__@ptr@ /(input)/__]: Pointer to data

     __C declaration:__ @ptr@
     -}
  -> IO (Ptr.Ptr Void)
dangerous_function = hs_bindgen_e065fc013e4eccd6

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_detailed_return_codes@
foreign import ccall unsafe "hs_bindgen_bc4f7e24b2ad4ace" hs_bindgen_bc4f7e24b2ad4ace_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_detailed_return_codes@
hs_bindgen_bc4f7e24b2ad4ace ::
     HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
  -> IO FC.CInt
hs_bindgen_bc4f7e24b2ad4ace =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bc4f7e24b2ad4ace_base

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
detailed_return_codes ::
     HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
     {- ^

        [__@input@ /(input)/__]: Input string

     __C declaration:__ @input@
     -}
  -> IO FC.CInt
detailed_return_codes = hs_bindgen_bc4f7e24b2ad4ace

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_old_function@
foreign import ccall unsafe "hs_bindgen_8deec146389ae8b3" hs_bindgen_8deec146389ae8b3_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_old_function@
hs_bindgen_8deec146389ae8b3 ::
     FC.CInt
  -> IO FC.CInt
hs_bindgen_8deec146389ae8b3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8deec146389ae8b3_base

{-|

  Function with deprecated annotation

  __deprecated:__ Use new_function() instead

  [__@old_param@ /(input)/__]: Legacy parameter

  __returns:__ Legacy result

__C declaration:__ @old_function@

__defined at:__ @documentation\/doxygen_docs.h 206:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
old_function ::
     FC.CInt
     {- ^

        [__@old_param@ /(input)/__]: Legacy parameter

     __C declaration:__ @old_param@
     -}
  -> IO FC.CInt
old_function = hs_bindgen_8deec146389ae8b3

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_versioned_function@
foreign import ccall unsafe "hs_bindgen_da2dcc1473935665" hs_bindgen_da2dcc1473935665_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_versioned_function@
hs_bindgen_da2dcc1473935665 ::
     FC.CInt
  -> IO FC.CInt
hs_bindgen_da2dcc1473935665 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_da2dcc1473935665_base

{-|

  Function with version information

  @since:  1.0

  [__@data@ /(input)/__]: Input data

  __returns:__ Processed data

__C declaration:__ @versioned_function@

__defined at:__ @documentation\/doxygen_docs.h 216:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
versioned_function ::
     FC.CInt
     {- ^

        [__@data@ /(input)/__]: Input data

     __C declaration:__ @data@
     -}
  -> IO FC.CInt
versioned_function = hs_bindgen_da2dcc1473935665

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_buffer@
foreign import ccall unsafe "hs_bindgen_97c1191917e6eece" hs_bindgen_97c1191917e6eece_base ::
     Ptr.Ptr Void
  -> GHC.Word.Word64
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_buffer@
hs_bindgen_97c1191917e6eece ::
     Ptr.Ptr FC.CChar
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO FC.CInt
hs_bindgen_97c1191917e6eece =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_97c1191917e6eece_base

{-|

  Static array parameter

  [__@buffer@ /(input)/__]: Buffer with minimum size

  [__@size@ /(input)/__]: Actual buffer size

  __returns:__ Number of bytes written

__C declaration:__ @process_buffer@

__defined at:__ @documentation\/doxygen_docs.h 332:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
process_buffer ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@buffer@ /(input)/__]: Buffer with minimum size

     __C declaration:__ @buffer@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@size@ /(input)/__]: Actual buffer size

     __C declaration:__ @size@
     -}
  -> IO FC.CInt
process_buffer = hs_bindgen_97c1191917e6eece

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_my_memcpy@
foreign import ccall unsafe "hs_bindgen_58253bb560dc3eb3" hs_bindgen_58253bb560dc3eb3_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> GHC.Word.Word64
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_my_memcpy@
hs_bindgen_58253bb560dc3eb3 ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO (Ptr.Ptr Void)
hs_bindgen_58253bb560dc3eb3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_58253bb560dc3eb3_base

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
my_memcpy ::
     Ptr.Ptr Void
     {- ^

        [__@dest@ /(input)/__]: Destination buffer (restrict)

     __C declaration:__ @dest@
     -}
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
     {- ^

        [__@src@ /(input)/__]: Source buffer (restrict)

     __C declaration:__ @src@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@n@ /(input)/__]: Number of bytes

     __C declaration:__ @n@
     -}
  -> IO (Ptr.Ptr Void)
my_memcpy = hs_bindgen_58253bb560dc3eb3

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_double_value@
foreign import ccall unsafe "hs_bindgen_44dd19b16ee38e5b" hs_bindgen_44dd19b16ee38e5b_base ::
     GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_double_value@
hs_bindgen_44dd19b16ee38e5b ::
     FC.CInt
  -> IO FC.CInt
hs_bindgen_44dd19b16ee38e5b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_44dd19b16ee38e5b_base

{-|

  Inline function

  [__@x@ /(input)/__]: Input value

  __returns:__ Doubled value

__C declaration:__ @double_value@

__defined at:__ @documentation\/doxygen_docs.h 350:19@

__exported by:__ @documentation\/doxygen_docs.h@
-}
double_value ::
     FC.CInt
     {- ^

        [__@x@ /(input)/__]: Input value

     __C declaration:__ @x@
     -}
  -> IO FC.CInt
double_value = hs_bindgen_44dd19b16ee38e5b

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_complex_function@
foreign import ccall unsafe "hs_bindgen_fd6fce7c8d8b2f79" hs_bindgen_fd6fce7c8d8b2f79_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> GHC.Word.Word64
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_complex_function@
hs_bindgen_fd6fce7c8d8b2f79 ::
     Ptr.Ptr Config_t
  -> HsBindgen.Runtime.ConstPtr.ConstPtr HsBindgen.Runtime.Prelude.Word8
  -> HsBindgen.Runtime.Prelude.CSize
  -> IO Status_code_t
hs_bindgen_fd6fce7c8d8b2f79 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fd6fce7c8d8b2f79_base

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
complex_function ::
     Ptr.Ptr Config_t
     {- ^

        [__@config@ /(input)/__]: Configuration structure (see 'Config_t' )

     __C declaration:__ @config@
     -}
  -> HsBindgen.Runtime.ConstPtr.ConstPtr HsBindgen.Runtime.Prelude.Word8
     {- ^

        [__@data@ /(input)/__]: Input data buffer

     __C declaration:__ @data@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@size@ /(input)/__]: Size of input data

     __C declaration:__ @size@
     -}
  -> IO Status_code_t
complex_function = hs_bindgen_fd6fce7c8d8b2f79

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_hash@
foreign import ccall unsafe "hs_bindgen_dd36c8b317ccfcc4" hs_bindgen_dd36c8b317ccfcc4_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_hash@
hs_bindgen_dd36c8b317ccfcc4 ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt
hs_bindgen_dd36c8b317ccfcc4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_dd36c8b317ccfcc4_base

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @documentation\/doxygen_docs.h 427:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
hash ::
     Ptr.Ptr FC.CChar
     -- ^ __C declaration:__ @s@
  -> IO FC.CInt
hash = hs_bindgen_dd36c8b317ccfcc4

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_square@
foreign import ccall unsafe "hs_bindgen_6875e30a7fe8d30a" hs_bindgen_6875e30a7fe8d30a_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_square@
hs_bindgen_6875e30a7fe8d30a ::
     FC.CInt
  -> FC.CInt
hs_bindgen_6875e30a7fe8d30a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6875e30a7fe8d30a_base

{-|

  Marked @__attribute((const))__@

__C declaration:__ @square@

__defined at:__ @documentation\/doxygen_docs.h 429:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
square ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> FC.CInt
square = hs_bindgen_6875e30a7fe8d30a
