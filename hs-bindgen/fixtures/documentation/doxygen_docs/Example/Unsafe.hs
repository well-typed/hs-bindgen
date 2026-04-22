{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.process_data
    , Example.Unsafe.process_file
    , Example.Unsafe.calculate_value
    , Example.Unsafe.html_example
    , Example.Unsafe.list_example
    , Example.Unsafe.dangerous_function
    , Example.Unsafe.detailed_return_codes
    , Example.Unsafe.old_function
    , Example.Unsafe.versioned_function
    , Example.Unsafe.process_buffer
    , Example.Unsafe.my_memcpy
    , Example.Unsafe.double_value
    , Example.Unsafe.complex_function
    , Example.Unsafe.hash
    , Example.Unsafe.square
    , Example.Unsafe.auto_brief_func
    , Example.Unsafe.multi_paragraph_details
    , Example.Unsafe.todo_remark_attention
    , Example.Unsafe.html_entities_func
    , Example.Unsafe.nested_inline_format
    , Example.Unsafe.tagged_code_example
    , Example.Unsafe.backslash_syntax
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <documentation/doxygen_docs.h>"
  , "signed int hs_bindgen_e6085a910ba41ecb ("
  , "  uint8_t const *arg1,"
  , "  uint8_t *arg2,"
  , "  size_t *arg3"
  , ")"
  , "{"
  , "  return (process_data)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_c27e893aea0b0a77 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (process_file)(arg1);"
  , "}"
  , "signed int hs_bindgen_bc1b0e25a72f4ec0 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return (calculate_value)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_09abc3cb74562964 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (html_example)(arg1);"
  , "}"
  , "_Bool hs_bindgen_47cba1a95d265f84 ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (list_example)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_e065fc013e4eccd6 ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return (dangerous_function)(arg1);"
  , "}"
  , "signed int hs_bindgen_bc4f7e24b2ad4ace ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (detailed_return_codes)(arg1);"
  , "}"
  , "signed int hs_bindgen_8deec146389ae8b3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (old_function)(arg1);"
  , "}"
  , "signed int hs_bindgen_da2dcc1473935665 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (versioned_function)(arg1);"
  , "}"
  , "signed int hs_bindgen_97c1191917e6eece ("
  , "  char *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (process_buffer)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_58253bb560dc3eb3 ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (my_memcpy)(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_44dd19b16ee38e5b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (double_value)(arg1);"
  , "}"
  , "status_code_t hs_bindgen_fd6fce7c8d8b2f79 ("
  , "  config_t *arg1,"
  , "  uint8_t const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (complex_function)(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_dd36c8b317ccfcc4 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (hash)(arg1);"
  , "}"
  , "signed int hs_bindgen_6875e30a7fe8d30a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square)(arg1);"
  , "}"
  , "signed int hs_bindgen_3094fd32a3eedd49 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (auto_brief_func)(arg1);"
  , "}"
  , "void hs_bindgen_73270d34a2bf31b5 ("
  , "  char *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (multi_paragraph_details)(arg1, arg2);"
  , "}"
  , "signed int *hs_bindgen_836de64a94d7bf03 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (todo_remark_attention)(arg1);"
  , "}"
  , "signed int hs_bindgen_aa0467b9d3b1a48d ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (html_entities_func)(arg1);"
  , "}"
  , "void hs_bindgen_b654d8047edfed77 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (nested_inline_format)(arg1);"
  , "}"
  , "signed int hs_bindgen_aeb274aded3470e5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (tagged_code_example)(arg1);"
  , "}"
  , "signed int hs_bindgen_907720dffab9442a ("
  , "  char const *arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return (backslash_syntax)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_data@
foreign import ccall unsafe "hs_bindgen_e6085a910ba41ecb" hs_bindgen_e6085a910ba41ecb_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_data@
hs_bindgen_e6085a910ba41ecb ::
     PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8
  -> RIP.Ptr HsBindgen.Runtime.LibC.Word8
  -> RIP.Ptr HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CInt
hs_bindgen_e6085a910ba41ecb =
  RIP.fromFFIType hs_bindgen_e6085a910ba41ecb_base

{-| Function with detailed parameter documentation.

    This function shows different parameter directions and types.

    [__@input_data@ /(input)/__]: Input data buffer

    [__@output_data@ /(output)/__]: Output data buffer

    [__@size@ /(input,output)/__]: Size of data, updated on return

    __Returns:__ Status code (0 = success, -1 = error)

    __C declaration:__ @process_data@

    __defined at:__ @documentation\/doxygen_docs.h 111:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
process_data ::
     PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8
     {- ^

          [__@input_data@ /(input)/__]: Input data buffer

          __C declaration:__ @input_data@
     -}
  -> RIP.Ptr HsBindgen.Runtime.LibC.Word8
     {- ^

          [__@output_data@ /(output)/__]: Output data buffer

          __C declaration:__ @output_data@
     -}
  -> RIP.Ptr HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@size@ /(input,output)/__]: Size of data, updated on return

          __C declaration:__ @size@
     -}
  -> IO RIP.CInt
process_data = hs_bindgen_e6085a910ba41ecb

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_file@
foreign import ccall unsafe "hs_bindgen_c27e893aea0b0a77" hs_bindgen_c27e893aea0b0a77_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_file@
hs_bindgen_c27e893aea0b0a77 ::
     PtrConst.PtrConst RIP.CChar
  -> IO RIP.CBool
hs_bindgen_c27e893aea0b0a77 =
  RIP.fromFFIType hs_bindgen_c27e893aea0b0a77_base

{-| Function with inline commands and formatting.

    This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

    [__@filename@__]: The @char*@ filename to process

    __Returns:__ @true@ if successful, @false@ otherwise

    __C declaration:__ @process_file@

    __defined at:__ @documentation\/doxygen_docs.h 122:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
process_file ::
     PtrConst.PtrConst RIP.CChar
     {- ^

          [__@filename@__]: The @char*@ filename to process

          __C declaration:__ @filename@
     -}
  -> IO RIP.CBool
process_file = hs_bindgen_c27e893aea0b0a77

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_calculate_value@
foreign import ccall unsafe "hs_bindgen_bc1b0e25a72f4ec0" hs_bindgen_bc1b0e25a72f4ec0_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_calculate_value@
hs_bindgen_bc1b0e25a72f4ec0 ::
     RIP.CInt
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_bc1b0e25a72f4ec0 =
  RIP.fromFFIType hs_bindgen_bc1b0e25a72f4ec0_base

{-| Function with verbatim code blocks.

    Example usage:

    @
    int result = calculate_value(10, 20);
    printf("Result: %d\n", result);
    @

    [__@base@__]: Base value

    [__@multiplier@__]: Multiplier value

    __Returns:__ Calculated result

    __C declaration:__ @calculate_value@

    __defined at:__ @documentation\/doxygen_docs.h 137:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
calculate_value ::
     RIP.CInt
     {- ^

          [__@base@__]: Base value

          __C declaration:__ @base@
     -}
  -> RIP.CInt
     {- ^

          [__@multiplier@__]: Multiplier value

          __C declaration:__ @multiplier@
     -}
  -> IO RIP.CInt
calculate_value = hs_bindgen_bc1b0e25a72f4ec0

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_html_example@
foreign import ccall unsafe "hs_bindgen_09abc3cb74562964" hs_bindgen_09abc3cb74562964_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_html_example@
hs_bindgen_09abc3cb74562964 ::
     RIP.CInt
  -> IO RIP.CBool
hs_bindgen_09abc3cb74562964 =
  RIP.fromFFIType hs_bindgen_09abc3cb74562964_base

{-| Function with HTML formatting.

    This function demonstrates __HTML bold__ and /italic/ text. It also shows @HTML code@ formatting.

    Input

    Output

    0

    false

    1

    true

    [__@value@__]: Input value

    __Returns:__ Boolean result

    __C declaration:__ @html_example@

    __defined at:__ @documentation\/doxygen_docs.h 154:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
html_example ::
     RIP.CInt
     {- ^

          [__@value@__]: Input value

          __C declaration:__ @value@
     -}
  -> IO RIP.CBool
html_example = hs_bindgen_09abc3cb74562964

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_list_example@
foreign import ccall unsafe "hs_bindgen_47cba1a95d265f84" hs_bindgen_47cba1a95d265f84_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_list_example@
hs_bindgen_47cba1a95d265f84 ::
     RIP.Ptr (PtrConst.PtrConst RIP.CChar)
  -> HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CBool
hs_bindgen_47cba1a95d265f84 =
  RIP.fromFFIType hs_bindgen_47cba1a95d265f84_base

{-| Function with lists and special formatting.

    This function demonstrates:

    * Bullet point lists
      * Nested list item 1
      * Nested list item 2

    * Multiple items

    * Nested formatting

    Numbered list:

    1. First @item@ 1. item

    2. Second __item__

    3. Third item

    Other numbered list:

    1. A

    2. B

    3. C

    [__@items@__]: Array of items

    [__@count@__]: Number of items

    __Returns:__ Success status

    __C declaration:__ @list_example@

    __defined at:__ @documentation\/doxygen_docs.h 180:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
list_example ::
     RIP.Ptr (PtrConst.PtrConst RIP.CChar)
     {- ^

          [__@items@__]: Array of items

          __C declaration:__ @items@
     -}
  -> HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@count@__]: Number of items

          __C declaration:__ @count@
     -}
  -> IO RIP.CBool
list_example = hs_bindgen_47cba1a95d265f84

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_dangerous_function@
foreign import ccall unsafe "hs_bindgen_e065fc013e4eccd6" hs_bindgen_e065fc013e4eccd6_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_dangerous_function@
hs_bindgen_e065fc013e4eccd6 ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_e065fc013e4eccd6 =
  RIP.fromFFIType hs_bindgen_e065fc013e4eccd6_base

{-| Function with warnings and notes.

    __WARNING:__ This function may cause side effects

    __Note:__ Use with caution in multithreaded environments

    __See:__ related_function() for similar functionality

    [__@ptr@__]: Pointer to data

    __Returns:__ Modified pointer

    __C declaration:__ @dangerous_function@

    __defined at:__ @documentation\/doxygen_docs.h 192:7@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
dangerous_function ::
     RIP.Ptr RIP.Void
     {- ^

          [__@ptr@__]: Pointer to data

          __C declaration:__ @ptr@
     -}
  -> IO (RIP.Ptr RIP.Void)
dangerous_function = hs_bindgen_e065fc013e4eccd6

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_detailed_return_codes@
foreign import ccall unsafe "hs_bindgen_bc4f7e24b2ad4ace" hs_bindgen_bc4f7e24b2ad4ace_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_detailed_return_codes@
hs_bindgen_bc4f7e24b2ad4ace ::
     PtrConst.PtrConst RIP.CChar
  -> IO RIP.CInt
hs_bindgen_bc4f7e24b2ad4ace =
  RIP.fromFFIType hs_bindgen_bc4f7e24b2ad4ace_base

{-| Function with return value details.

    [__@input@__]: Input string

    [__@0@__]: Success

    [__@-1@__]: Invalid input

    [__@-2@__]: Memory allocation failed

    [__@-3@__]: Processing error

    __C declaration:__ @detailed_return_codes@

    __defined at:__ @documentation\/doxygen_docs.h 203:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
detailed_return_codes ::
     PtrConst.PtrConst RIP.CChar
     {- ^

          [__@input@__]: Input string

          __C declaration:__ @input@
     -}
  -> IO RIP.CInt
detailed_return_codes = hs_bindgen_bc4f7e24b2ad4ace

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_old_function@
foreign import ccall unsafe "hs_bindgen_8deec146389ae8b3" hs_bindgen_8deec146389ae8b3_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_old_function@
hs_bindgen_8deec146389ae8b3 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_8deec146389ae8b3 =
  RIP.fromFFIType hs_bindgen_8deec146389ae8b3_base

{-| Function with deprecated annotation.

    __Deprecated:__ Use new_function() instead

    [__@old_param@__]: Legacy parameter

    __Returns:__ Legacy result

    __C declaration:__ @old_function@

    __defined at:__ @documentation\/doxygen_docs.h 212:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
old_function ::
     RIP.CInt
     {- ^

          [__@old_param@__]: Legacy parameter

          __C declaration:__ @old_param@
     -}
  -> IO RIP.CInt
old_function = hs_bindgen_8deec146389ae8b3

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_versioned_function@
foreign import ccall unsafe "hs_bindgen_da2dcc1473935665" hs_bindgen_da2dcc1473935665_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_versioned_function@
hs_bindgen_da2dcc1473935665 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_da2dcc1473935665 =
  RIP.fromFFIType hs_bindgen_da2dcc1473935665_base

{-| Function with version information.

    @since 1.0

    __Version:__ 1.2

    [__@data@__]: Input data

    __Returns:__ Processed data

    __C declaration:__ @versioned_function@

    __defined at:__ @documentation\/doxygen_docs.h 222:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
versioned_function ::
     RIP.CInt
     {- ^

          [__@data@__]: Input data

          __C declaration:__ @data@
     -}
  -> IO RIP.CInt
versioned_function = hs_bindgen_da2dcc1473935665

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_buffer@
foreign import ccall unsafe "hs_bindgen_97c1191917e6eece" hs_bindgen_97c1191917e6eece_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_process_buffer@
hs_bindgen_97c1191917e6eece ::
     RIP.Ptr (IsA.Elem (CA.ConstantArray 64 RIP.CChar))
  -> HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CInt
hs_bindgen_97c1191917e6eece =
  RIP.fromFFIType hs_bindgen_97c1191917e6eece_base

{-| Static array parameter.

    [__@buffer@__]: Buffer with minimum size

    [__@size@__]: Actual buffer size

    __Returns:__ Number of bytes written

    __C declaration:__ @process_buffer@

    __defined at:__ @documentation\/doxygen_docs.h 338:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
process_buffer ::
     RIP.Ptr (IsA.Elem (CA.ConstantArray 64 RIP.CChar))
     {- ^

          [__@buffer@__]: Buffer with minimum size

          __C declaration:__ @buffer@
     -}
  -> HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@size@__]: Actual buffer size

          __C declaration:__ @size@
     -}
  -> IO RIP.CInt
process_buffer = hs_bindgen_97c1191917e6eece

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_my_memcpy@
foreign import ccall unsafe "hs_bindgen_58253bb560dc3eb3" hs_bindgen_58253bb560dc3eb3_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_my_memcpy@
hs_bindgen_58253bb560dc3eb3 ::
     RIP.Ptr RIP.Void
  -> PtrConst.PtrConst RIP.Void
  -> HsBindgen.Runtime.LibC.CSize
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_58253bb560dc3eb3 =
  RIP.fromFFIType hs_bindgen_58253bb560dc3eb3_base

{-| Function with restrict pointers.

    [__@dest@__]: Destination buffer (restrict)

    [__@src@__]: Source buffer (restrict)

    [__@n@__]: Number of bytes

    __Returns:__ Destination pointer

    __C declaration:__ @my_memcpy@

    __defined at:__ @documentation\/doxygen_docs.h 348:7@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
my_memcpy ::
     RIP.Ptr RIP.Void
     {- ^

          [__@dest@__]: Destination buffer (restrict)

          __C declaration:__ @dest@
     -}
  -> PtrConst.PtrConst RIP.Void
     {- ^

          [__@src@__]: Source buffer (restrict)

          __C declaration:__ @src@
     -}
  -> HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@n@__]: Number of bytes

          __C declaration:__ @n@
     -}
  -> IO (RIP.Ptr RIP.Void)
my_memcpy = hs_bindgen_58253bb560dc3eb3

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_double_value@
foreign import ccall unsafe "hs_bindgen_44dd19b16ee38e5b" hs_bindgen_44dd19b16ee38e5b_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_double_value@
hs_bindgen_44dd19b16ee38e5b ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_44dd19b16ee38e5b =
  RIP.fromFFIType hs_bindgen_44dd19b16ee38e5b_base

{-| Inline function.

    [__@x@__]: Input value

    __Returns:__ Doubled value

    __C declaration:__ @double_value@

    __defined at:__ @documentation\/doxygen_docs.h 356:19@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
double_value ::
     RIP.CInt
     {- ^

          [__@x@__]: Input value

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
double_value = hs_bindgen_44dd19b16ee38e5b

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_complex_function@
foreign import ccall unsafe "hs_bindgen_fd6fce7c8d8b2f79" hs_bindgen_fd6fce7c8d8b2f79_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_complex_function@
hs_bindgen_fd6fce7c8d8b2f79 ::
     RIP.Ptr Config_t
  -> PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8
  -> HsBindgen.Runtime.LibC.CSize
  -> IO Status_code_t
hs_bindgen_fd6fce7c8d8b2f79 =
  RIP.fromFFIType hs_bindgen_fd6fce7c8d8b2f79_base

{-| Function with complex documentation.

    This function demonstrates multiple documentation features:

    __Description:__

    Performs complex data processing with multiple steps.

    __Algorithm:__

    1. Validate input parameters

    2. Allocate temporary buffers

    3. Process data in chunks

    4. Clean up resources

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

    [__@config@__]: Configuration structure (see 'Config_t' )

    [__@data@__]: Input data buffer

    [__@size@__]: Size of input data

    __Returns:__ Status code indicating success or failure

    __Precondition:__ config must not be NULL

    __Precondition:__ data must not be NULL if size > 0

    __Postcondition:__ Output data is written to config->user_data

    __WARNING:__ May return NULL if memory allocation fails

    __WARNING:__ Sets errno to EINVAL if parameters are invalid

    __C declaration:__ @complex_function@

    __defined at:__ @documentation\/doxygen_docs.h 428:15@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
complex_function ::
     RIP.Ptr Config_t
     {- ^

          [__@config@__]: Configuration structure (see 'Config_t' )

          __C declaration:__ @config@
     -}
  -> PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8
     {- ^

          [__@data@__]: Input data buffer

          __C declaration:__ @data@
     -}
  -> HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@size@__]: Size of input data

          __C declaration:__ @size@
     -}
  -> IO Status_code_t
complex_function = hs_bindgen_fd6fce7c8d8b2f79

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_hash@
foreign import ccall unsafe "hs_bindgen_dd36c8b317ccfcc4" hs_bindgen_dd36c8b317ccfcc4_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_hash@
hs_bindgen_dd36c8b317ccfcc4 ::
     RIP.Ptr RIP.CChar
  -> IO RIP.CInt
hs_bindgen_dd36c8b317ccfcc4 =
  RIP.fromFFIType hs_bindgen_dd36c8b317ccfcc4_base

{-|

    Marked @__attribute((pure))__@

    __C declaration:__ @hash@

    __defined at:__ @documentation\/doxygen_docs.h 432:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
hash ::
     RIP.Ptr RIP.CChar
     -- ^ __C declaration:__ @s@
  -> IO RIP.CInt
hash = hs_bindgen_dd36c8b317ccfcc4

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_square@
foreign import ccall unsafe "hs_bindgen_6875e30a7fe8d30a" hs_bindgen_6875e30a7fe8d30a_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_square@
hs_bindgen_6875e30a7fe8d30a ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_6875e30a7fe8d30a =
  RIP.fromFFIType hs_bindgen_6875e30a7fe8d30a_base

{-|

    Marked @__attribute((const))__@

    __C declaration:__ @square@

    __defined at:__ @documentation\/doxygen_docs.h 434:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
square ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
square = hs_bindgen_6875e30a7fe8d30a

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_auto_brief_func@
foreign import ccall unsafe "hs_bindgen_3094fd32a3eedd49" hs_bindgen_3094fd32a3eedd49_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_auto_brief_func@
hs_bindgen_3094fd32a3eedd49 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_3094fd32a3eedd49 =
  RIP.fromFFIType hs_bindgen_3094fd32a3eedd49_base

{-| Auto-brief function without explicit @brief tag.

    This tests that the first sentence is used as the brief description when no explicit @brief is present.

    [__@x@__]: The input value

    __Returns:__ The negated value

    __C declaration:__ @auto_brief_func@

    __defined at:__ @documentation\/doxygen_docs.h 455:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
auto_brief_func ::
     RIP.CInt
     {- ^

          [__@x@__]: The input value

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
auto_brief_func = hs_bindgen_3094fd32a3eedd49

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_multi_paragraph_details@
foreign import ccall unsafe "hs_bindgen_73270d34a2bf31b5" hs_bindgen_73270d34a2bf31b5_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_multi_paragraph_details@
hs_bindgen_73270d34a2bf31b5 ::
     RIP.Ptr RIP.CChar
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_73270d34a2bf31b5 =
  RIP.fromFFIType hs_bindgen_73270d34a2bf31b5_base

{-| Multi-paragraph details.

    First paragraph of the detailed description. This explains the basic purpose of the function.

    Second paragraph with more context. This includes information about the algorithm and its complexity, which is O(n) in the input size.

    Third paragraph with usage notes. Callers should ensure that the buffer is large enough before calling this function.

    [__@buf@__]: Output buffer

    [__@len@__]: Buffer length

    __C declaration:__ @multi_paragraph_details@

    __defined at:__ @documentation\/doxygen_docs.h 472:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
multi_paragraph_details ::
     RIP.Ptr RIP.CChar
     {- ^

          [__@buf@__]: Output buffer

          __C declaration:__ @buf@
     -}
  -> HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@len@__]: Buffer length

          __C declaration:__ @len@
     -}
  -> IO ()
multi_paragraph_details = hs_bindgen_73270d34a2bf31b5

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_todo_remark_attention@
foreign import ccall unsafe "hs_bindgen_836de64a94d7bf03" hs_bindgen_836de64a94d7bf03_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_todo_remark_attention@
hs_bindgen_836de64a94d7bf03 ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_836de64a94d7bf03 =
  RIP.fromFFIType hs_bindgen_836de64a94d7bf03_base

{-| Function with @todo and @remark.

    __Todo:__ Optimize this function for large inputs

    Add support for negative values

    __Remark:__ This function is thread-safe

    __ATTENTION:__ The caller must free the returned pointer

    [__@n@__]: Input count

    __Returns:__ Allocated array

    __C declaration:__ @todo_remark_attention@

    __defined at:__ @documentation\/doxygen_docs.h 485:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
todo_remark_attention ::
     RIP.CInt
     {- ^

          [__@n@__]: Input count

          __C declaration:__ @n@
     -}
  -> IO (RIP.Ptr RIP.CInt)
todo_remark_attention = hs_bindgen_836de64a94d7bf03

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_html_entities_func@
foreign import ccall unsafe "hs_bindgen_aa0467b9d3b1a48d" hs_bindgen_aa0467b9d3b1a48d_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_html_entities_func@
hs_bindgen_aa0467b9d3b1a48d ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_aa0467b9d3b1a48d =
  RIP.fromFFIType hs_bindgen_aa0467b9d3b1a48d_base

{-| HTML entities: & means AND, <tag> is a tag.

    Handles values < 0 and values > 100 differently. Copyright 2025.

    [__@x@__]: Input (> 0 required)

    __Returns:__ Result code

    __C declaration:__ @html_entities_func@

    __defined at:__ @documentation\/doxygen_docs.h 507:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
html_entities_func ::
     RIP.CInt
     {- ^

          [__@x@__]: Input (> 0 required)

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
html_entities_func = hs_bindgen_aa0467b9d3b1a48d

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_nested_inline_format@
foreign import ccall unsafe "hs_bindgen_b654d8047edfed77" hs_bindgen_b654d8047edfed77_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_nested_inline_format@
hs_bindgen_b654d8047edfed77 ::
     RIP.CInt
  -> IO ()
hs_bindgen_b654d8047edfed77 =
  RIP.fromFFIType hs_bindgen_b654d8047edfed77_base

{-| Nested inline: __bold__ , @code@ , __@bold_code@ ,__ /__emph_bold__ ./

    [__@x@__]: Input value

    __C declaration:__ @nested_inline_format@

    __defined at:__ @documentation\/doxygen_docs.h 514:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
nested_inline_format ::
     RIP.CInt
     {- ^

          [__@x@__]: Input value

          __C declaration:__ @x@
     -}
  -> IO ()
nested_inline_format = hs_bindgen_b654d8047edfed77

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_tagged_code_example@
foreign import ccall unsafe "hs_bindgen_aeb274aded3470e5" hs_bindgen_aeb274aded3470e5_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_tagged_code_example@
hs_bindgen_aeb274aded3470e5 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_aeb274aded3470e5 =
  RIP.fromFFIType hs_bindgen_aeb274aded3470e5_base

{-| Language-tagged code block.

    Example usage:

    @
    int result = tagged_code_example(42);
    printf("Result: %d\n", result);
    @

    [__@x@__]: Input value

    __Returns:__ Processed value

    __C declaration:__ @tagged_code_example@

    __defined at:__ @documentation\/doxygen_docs.h 528:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
tagged_code_example ::
     RIP.CInt
     {- ^

          [__@x@__]: Input value

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
tagged_code_example = hs_bindgen_aeb274aded3470e5

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_backslash_syntax@
foreign import ccall unsafe "hs_bindgen_907720dffab9442a" hs_bindgen_907720dffab9442a_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Unsafe_backslash_syntax@
hs_bindgen_907720dffab9442a ::
     PtrConst.PtrConst RIP.CChar
  -> RIP.Ptr RIP.CChar
  -> IO RIP.CInt
hs_bindgen_907720dffab9442a =
  RIP.fromFFIType hs_bindgen_907720dffab9442a_base

{-| Function documented with backslash syntax.

    This function uses backslash commands instead of @ commands to verify both syntaxes are handled equivalently.

    [__@input@__]: The input string

    [__@output@__]: The output buffer

    __Returns:__ Number of bytes written

    __See:__ 'auto_brief_func'

    @since 2.0

    __C declaration:__ @backslash_syntax@

    __defined at:__ @documentation\/doxygen_docs.h 543:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
backslash_syntax ::
     PtrConst.PtrConst RIP.CChar
     {- ^

          [__@input@__]: The input string

          __C declaration:__ @input@
     -}
  -> RIP.Ptr RIP.CChar
     {- ^

          [__@output@__]: The output buffer

          __C declaration:__ @output@
     -}
  -> IO RIP.CInt
backslash_syntax = hs_bindgen_907720dffab9442a
