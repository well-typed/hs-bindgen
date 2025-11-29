{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <documentation/doxygen_docs.h>"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_2d2b30c70759c0c4 ("
  , "  uint8_t const *arg1,"
  , "  uint8_t *arg2,"
  , "  size_t *arg3"
  , ")"
  , "{"
  , "  return process_data(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_test_documentationdoxygen_docs_d6a9988889495ac1 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return process_file(arg1);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_082e1bba4d72bc95 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return calculate_value(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_test_documentationdoxygen_docs_fddff4284f0988ec ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return html_example(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_documentationdoxygen_docs_41af05ef1797fa6d ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return list_example(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_documentationdoxygen_docs_3f6186f38de47df9 ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return dangerous_function(arg1);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_b5913ddf382b031a ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return detailed_return_codes(arg1);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_2b7f6e5fbd1f46e1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return old_function(arg1);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_7760ce72dbe3dbbb ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return versioned_function(arg1);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_75f51ea0caca0775 ("
  , "  char *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return process_buffer(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_documentationdoxygen_docs_e8c4a96cefd6117e ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_e73c6e96d9e7581d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return double_value(arg1);"
  , "}"
  , "status_code_t hs_bindgen_test_documentationdoxygen_docs_bc28ed88ec7705d4 ("
  , "  config_t *arg1,"
  , "  uint8_t const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return complex_function(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_88887d4b5f42f079 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "signed int hs_bindgen_test_documentationdoxygen_docs_cb3c687f16289bb3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  ]))

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

__unique:__ @ExampleJust Unsafeprocess_data@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_2d2b30c70759c0c4" process_data ::
     Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
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

{-|

  Function with inline commands and formatting

  This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

  [__@filename@ /(input)/__]: The @char*@ filename to process

  __returns:__ @true@ if successful, @false@ otherwise

__C declaration:__ @process_file@

__defined at:__ @documentation\/doxygen_docs.h:116:6@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafeprocess_file@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_d6a9988889495ac1" process_file ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@filename@ /(input)/__]: The @char*@ filename to process

     __C declaration:__ @filename@
     -}
  -> IO FC.CBool

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

__unique:__ @ExampleJust Unsafecalculate_value@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_082e1bba4d72bc95" calculate_value ::
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

{-|

  Function with HTML formatting

  This function demonstrates HTML bold and italic text. It also shows HTML code formatting.

  Input Output 0 false 1 true

  [__@value@ /(input)/__]: Input value

  __returns:__ Boolean result

__C declaration:__ @html_example@

__defined at:__ @documentation\/doxygen_docs.h:148:6@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafehtml_example@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_fddff4284f0988ec" html_example ::
     FC.CInt
     {- ^

        [__@value@ /(input)/__]: Input value

     __C declaration:__ @value@
     -}
  -> IO FC.CBool

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

__unique:__ @ExampleJust Unsafelist_example@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_41af05ef1797fa6d" list_example ::
     Ptr.Ptr (Ptr.Ptr FC.CChar)
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

__unique:__ @ExampleJust Unsafedangerous_function@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_3f6186f38de47df9" dangerous_function ::
     Ptr.Ptr Void
     {- ^

        [__@ptr@ /(input)/__]: Pointer to data

     __C declaration:__ @ptr@
     -}
  -> IO (Ptr.Ptr Void)

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

__unique:__ @ExampleJust Unsafedetailed_return_codes@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_b5913ddf382b031a" detailed_return_codes ::
     Ptr.Ptr FC.CChar
     {- ^

        [__@input@ /(input)/__]: Input string

     __C declaration:__ @input@
     -}
  -> IO FC.CInt

{-|

  Function with deprecated annotation

  __deprecated:__ Use new_function() instead

  [__@old_param@ /(input)/__]: Legacy parameter

  __returns:__ Legacy result

__C declaration:__ @old_function@

__defined at:__ @documentation\/doxygen_docs.h:206:5@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafeold_function@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_2b7f6e5fbd1f46e1" old_function ::
     FC.CInt
     {- ^

        [__@old_param@ /(input)/__]: Legacy parameter

     __C declaration:__ @old_param@
     -}
  -> IO FC.CInt

{-|

  Function with version information

  @since:  1.0

  [__@data@ /(input)/__]: Input data

  __returns:__ Processed data

__C declaration:__ @versioned_function@

__defined at:__ @documentation\/doxygen_docs.h:216:5@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafeversioned_function@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_7760ce72dbe3dbbb" versioned_function ::
     FC.CInt
     {- ^ __C declaration:__ @data'@
     -}
  -> IO FC.CInt

{-|

  Static array parameter

  [__@buffer@ /(input)/__]: Buffer with minimum size

  [__@size@ /(input)/__]: Actual buffer size

  __returns:__ Number of bytes written

__C declaration:__ @process_buffer@

__defined at:__ @documentation\/doxygen_docs.h:332:5@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafeprocess_buffer@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_75f51ea0caca0775" process_buffer ::
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

{-|

  Function with restrict pointers

  [__@dest@ /(input)/__]: Destination buffer (restrict)

  [__@src@ /(input)/__]: Source buffer (restrict)

  [__@n@ /(input)/__]: Number of bytes

  __returns:__ Destination pointer

__C declaration:__ @my_memcpy@

__defined at:__ @documentation\/doxygen_docs.h:342:7@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafemy_memcpy@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_e8c4a96cefd6117e" my_memcpy ::
     Ptr.Ptr Void
     {- ^

        [__@dest@ /(input)/__]: Destination buffer (restrict)

     __C declaration:__ @dest@
     -}
  -> Ptr.Ptr Void
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

{-|

  Inline function

  [__@x@ /(input)/__]: Input value

  __returns:__ Doubled value

__C declaration:__ @double_value@

__defined at:__ @documentation\/doxygen_docs.h:350:19@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafedouble_value@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_e73c6e96d9e7581d" double_value ::
     FC.CInt
     {- ^

        [__@x@ /(input)/__]: Input value

     __C declaration:__ @x@
     -}
  -> IO FC.CInt

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

__unique:__ @ExampleJust Unsafecomplex_function@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_bc28ed88ec7705d4" complex_function ::
     Ptr.Ptr Config_t
     {- ^

        [__@config@ /(input)/__]: Configuration structure (see 'Config_t' )

     __C declaration:__ @config@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^ __C declaration:__ @data'@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^

        [__@size@ /(input)/__]: Size of input data

     __C declaration:__ @size@
     -}
  -> IO Status_code_t

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @documentation\/doxygen_docs.h:427:5@

__exported by:__ @documentation\/doxygen_docs.h@

__unique:__ @ExampleJust Unsafehash@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_88887d4b5f42f079" hash ::
     Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @square@

    __defined at:__ @documentation\/doxygen_docs.h:429:5@

    __exported by:__ @documentation\/doxygen_docs.h@

    __unique:__ @ExampleJust Unsafesquare@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_cb3c687f16289bb3" square ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
