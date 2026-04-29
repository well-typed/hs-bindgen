{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( -- * Function Definitions
      Example.Safe.process_data
    , Example.Safe.process_file
    , Example.Safe.calculate_value
    , Example.Safe.html_example
    , Example.Safe.list_example
    , Example.Safe.dangerous_function
    , Example.Safe.detailed_return_codes
    , Example.Safe.old_function
    , Example.Safe.versioned_function
    , Example.Safe.process_buffer
    , Example.Safe.my_memcpy
    , Example.Safe.double_value
      -- ** I/O Helpers
    , Example.Safe.read_chunk
    , Example.Safe.write_chunk
      -- * Advanced Features
    , Example.Safe.complex_function
    , Example.Safe.hash
    , Example.Safe.square
      -- * Extra Doxygen Coverage
    , Example.Safe.auto_brief_func
    , Example.Safe.multi_paragraph_details
    , Example.Safe.todo_remark_attention
    , Example.Safe.html_entities_func
    , Example.Safe.nested_inline_format
    , Example.Safe.tagged_code_example
    , Example.Safe.backslash_syntax
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
  , "signed int hs_bindgen_7eada9f65d982412 ("
  , "  uint8_t const *arg1,"
  , "  uint8_t *arg2,"
  , "  size_t *arg3"
  , ")"
  , "{"
  , "  return (process_data)(arg1, arg2, arg3);"
  , "}"
  , "_Bool hs_bindgen_fb85499c501da1a7 ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (process_file)(arg1);"
  , "}"
  , "signed int hs_bindgen_a73fc7b108035c5c ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return (calculate_value)(arg1, arg2);"
  , "}"
  , "_Bool hs_bindgen_9b7f6745401b4652 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (html_example)(arg1);"
  , "}"
  , "_Bool hs_bindgen_825411dc114e599b ("
  , "  char const **arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (list_example)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_17264dcff7e9b698 ("
  , "  void *arg1"
  , ")"
  , "{"
  , "  return (dangerous_function)(arg1);"
  , "}"
  , "signed int hs_bindgen_c8ca619ec2e70d8d ("
  , "  char const *arg1"
  , ")"
  , "{"
  , "  return (detailed_return_codes)(arg1);"
  , "}"
  , "signed int hs_bindgen_25e1070e2ce10048 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (old_function)(arg1);"
  , "}"
  , "signed int hs_bindgen_a9eeeb09808e71cc ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (versioned_function)(arg1);"
  , "}"
  , "signed int hs_bindgen_0c0057f1700372a7 ("
  , "  char *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (process_buffer)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_294db77671f95524 ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (my_memcpy)(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_f5bc63a9952c2618 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (double_value)(arg1);"
  , "}"
  , "signed int hs_bindgen_3545f99d29c4fc53 ("
  , "  signed int arg1,"
  , "  void *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (read_chunk)(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_839c1d19dfe95042 ("
  , "  signed int arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (write_chunk)(arg1, arg2, arg3);"
  , "}"
  , "status_code_t hs_bindgen_c4e7e99dba20204d ("
  , "  config_t *arg1,"
  , "  uint8_t const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (complex_function)(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_935f2aead358d9ef ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (hash)(arg1);"
  , "}"
  , "signed int hs_bindgen_39fef54c23d4e1ee ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square)(arg1);"
  , "}"
  , "signed int hs_bindgen_29749932a4449f10 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (auto_brief_func)(arg1);"
  , "}"
  , "void hs_bindgen_03f0269c9296b8c0 ("
  , "  char *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  (multi_paragraph_details)(arg1, arg2);"
  , "}"
  , "signed int *hs_bindgen_0446a821eb966f46 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (todo_remark_attention)(arg1);"
  , "}"
  , "signed int hs_bindgen_25a9ea376bb793e9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (html_entities_func)(arg1);"
  , "}"
  , "void hs_bindgen_6f311771e5e063e6 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (nested_inline_format)(arg1);"
  , "}"
  , "signed int hs_bindgen_34473a588f1157e4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (tagged_code_example)(arg1);"
  , "}"
  , "signed int hs_bindgen_471f4a3c2db31252 ("
  , "  char const *arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return (backslash_syntax)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_process_data@
foreign import ccall safe "hs_bindgen_7eada9f65d982412" hs_bindgen_7eada9f65d982412_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_process_data@
hs_bindgen_7eada9f65d982412 ::
     PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8
  -> RIP.Ptr HsBindgen.Runtime.LibC.Word8
  -> RIP.Ptr HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CInt
hs_bindgen_7eada9f65d982412 =
  RIP.fromFFIType hs_bindgen_7eada9f65d982412_base

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
process_data = hs_bindgen_7eada9f65d982412

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_process_file@
foreign import ccall safe "hs_bindgen_fb85499c501da1a7" hs_bindgen_fb85499c501da1a7_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_process_file@
hs_bindgen_fb85499c501da1a7 ::
     PtrConst.PtrConst RIP.CChar
  -> IO RIP.CBool
hs_bindgen_fb85499c501da1a7 =
  RIP.fromFFIType hs_bindgen_fb85499c501da1a7_base

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
process_file = hs_bindgen_fb85499c501da1a7

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_calculate_value@
foreign import ccall safe "hs_bindgen_a73fc7b108035c5c" hs_bindgen_a73fc7b108035c5c_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_calculate_value@
hs_bindgen_a73fc7b108035c5c ::
     RIP.CInt
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_a73fc7b108035c5c =
  RIP.fromFFIType hs_bindgen_a73fc7b108035c5c_base

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
calculate_value = hs_bindgen_a73fc7b108035c5c

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_html_example@
foreign import ccall safe "hs_bindgen_9b7f6745401b4652" hs_bindgen_9b7f6745401b4652_base ::
     RIP.Int32
  -> IO RIP.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_html_example@
hs_bindgen_9b7f6745401b4652 ::
     RIP.CInt
  -> IO RIP.CBool
hs_bindgen_9b7f6745401b4652 =
  RIP.fromFFIType hs_bindgen_9b7f6745401b4652_base

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
html_example = hs_bindgen_9b7f6745401b4652

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_list_example@
foreign import ccall safe "hs_bindgen_825411dc114e599b" hs_bindgen_825411dc114e599b_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Word8

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_list_example@
hs_bindgen_825411dc114e599b ::
     RIP.Ptr (PtrConst.PtrConst RIP.CChar)
  -> HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CBool
hs_bindgen_825411dc114e599b =
  RIP.fromFFIType hs_bindgen_825411dc114e599b_base

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
list_example = hs_bindgen_825411dc114e599b

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_dangerous_function@
foreign import ccall safe "hs_bindgen_17264dcff7e9b698" hs_bindgen_17264dcff7e9b698_base ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_dangerous_function@
hs_bindgen_17264dcff7e9b698 ::
     RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_17264dcff7e9b698 =
  RIP.fromFFIType hs_bindgen_17264dcff7e9b698_base

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
dangerous_function = hs_bindgen_17264dcff7e9b698

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_detailed_return_codes@
foreign import ccall safe "hs_bindgen_c8ca619ec2e70d8d" hs_bindgen_c8ca619ec2e70d8d_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_detailed_return_codes@
hs_bindgen_c8ca619ec2e70d8d ::
     PtrConst.PtrConst RIP.CChar
  -> IO RIP.CInt
hs_bindgen_c8ca619ec2e70d8d =
  RIP.fromFFIType hs_bindgen_c8ca619ec2e70d8d_base

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
detailed_return_codes = hs_bindgen_c8ca619ec2e70d8d

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_old_function@
foreign import ccall safe "hs_bindgen_25e1070e2ce10048" hs_bindgen_25e1070e2ce10048_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_old_function@
hs_bindgen_25e1070e2ce10048 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_25e1070e2ce10048 =
  RIP.fromFFIType hs_bindgen_25e1070e2ce10048_base

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
old_function = hs_bindgen_25e1070e2ce10048

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_versioned_function@
foreign import ccall safe "hs_bindgen_a9eeeb09808e71cc" hs_bindgen_a9eeeb09808e71cc_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_versioned_function@
hs_bindgen_a9eeeb09808e71cc ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_a9eeeb09808e71cc =
  RIP.fromFFIType hs_bindgen_a9eeeb09808e71cc_base

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
versioned_function = hs_bindgen_a9eeeb09808e71cc

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_process_buffer@
foreign import ccall safe "hs_bindgen_0c0057f1700372a7" hs_bindgen_0c0057f1700372a7_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_process_buffer@
hs_bindgen_0c0057f1700372a7 ::
     RIP.Ptr (IsA.Elem (CA.ConstantArray 64 RIP.CChar))
  -> HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CInt
hs_bindgen_0c0057f1700372a7 =
  RIP.fromFFIType hs_bindgen_0c0057f1700372a7_base

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
process_buffer = hs_bindgen_0c0057f1700372a7

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_my_memcpy@
foreign import ccall safe "hs_bindgen_294db77671f95524" hs_bindgen_294db77671f95524_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_my_memcpy@
hs_bindgen_294db77671f95524 ::
     RIP.Ptr RIP.Void
  -> PtrConst.PtrConst RIP.Void
  -> HsBindgen.Runtime.LibC.CSize
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_294db77671f95524 =
  RIP.fromFFIType hs_bindgen_294db77671f95524_base

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
my_memcpy = hs_bindgen_294db77671f95524

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_double_value@
foreign import ccall safe "hs_bindgen_f5bc63a9952c2618" hs_bindgen_f5bc63a9952c2618_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_double_value@
hs_bindgen_f5bc63a9952c2618 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_f5bc63a9952c2618 =
  RIP.fromFFIType hs_bindgen_f5bc63a9952c2618_base

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
double_value = hs_bindgen_f5bc63a9952c2618

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_read_chunk@
foreign import ccall safe "hs_bindgen_3545f99d29c4fc53" hs_bindgen_3545f99d29c4fc53_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_read_chunk@
hs_bindgen_3545f99d29c4fc53 ::
     RIP.CInt
  -> RIP.Ptr RIP.Void
  -> HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CInt
hs_bindgen_3545f99d29c4fc53 =
  RIP.fromFFIType hs_bindgen_3545f99d29c4fc53_base

{-| Read a chunk of bytes into the provided buffer.

    Demonstrates a section nested inside @functions@ .

    [__@fd@__]: File descriptor

    [__@buf@__]: Output buffer

    [__@count@__]: Number of bytes to read

    __Returns:__ Number of bytes read

    __C declaration:__ @read_chunk@

    __defined at:__ @documentation\/doxygen_docs.h 387:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
read_chunk ::
     RIP.CInt
     {- ^

          [__@fd@__]: File descriptor

          __C declaration:__ @fd@
     -}
  -> RIP.Ptr RIP.Void
     {- ^

          [__@buf@__]: Output buffer

          __C declaration:__ @buf@
     -}
  -> HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@count@__]: Number of bytes to read

          __C declaration:__ @count@
     -}
  -> IO RIP.CInt
read_chunk = hs_bindgen_3545f99d29c4fc53

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_write_chunk@
foreign import ccall safe "hs_bindgen_839c1d19dfe95042" hs_bindgen_839c1d19dfe95042_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_write_chunk@
hs_bindgen_839c1d19dfe95042 ::
     RIP.CInt
  -> PtrConst.PtrConst RIP.Void
  -> HsBindgen.Runtime.LibC.CSize
  -> IO RIP.CInt
hs_bindgen_839c1d19dfe95042 =
  RIP.fromFFIType hs_bindgen_839c1d19dfe95042_base

{-| Write a chunk of bytes from the provided buffer.

    [__@fd@__]: File descriptor

    [__@buf@__]: Input buffer

    [__@count@__]: Number of bytes to write

    __Returns:__ Number of bytes written

    __C declaration:__ @write_chunk@

    __defined at:__ @documentation\/doxygen_docs.h 397:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
write_chunk ::
     RIP.CInt
     {- ^

          [__@fd@__]: File descriptor

          __C declaration:__ @fd@
     -}
  -> PtrConst.PtrConst RIP.Void
     {- ^

          [__@buf@__]: Input buffer

          __C declaration:__ @buf@
     -}
  -> HsBindgen.Runtime.LibC.CSize
     {- ^

          [__@count@__]: Number of bytes to write

          __C declaration:__ @count@
     -}
  -> IO RIP.CInt
write_chunk = hs_bindgen_839c1d19dfe95042

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_complex_function@
foreign import ccall safe "hs_bindgen_c4e7e99dba20204d" hs_bindgen_c4e7e99dba20204d_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_complex_function@
hs_bindgen_c4e7e99dba20204d ::
     RIP.Ptr Config_t
  -> PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8
  -> HsBindgen.Runtime.LibC.CSize
  -> IO Status_code_t
hs_bindgen_c4e7e99dba20204d =
  RIP.fromFFIType hs_bindgen_c4e7e99dba20204d_base

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

    __defined at:__ @documentation\/doxygen_docs.h 459:15@

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
complex_function = hs_bindgen_c4e7e99dba20204d

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_hash@
foreign import ccall safe "hs_bindgen_935f2aead358d9ef" hs_bindgen_935f2aead358d9ef_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_hash@
hs_bindgen_935f2aead358d9ef ::
     RIP.Ptr RIP.CChar
  -> IO RIP.CInt
hs_bindgen_935f2aead358d9ef =
  RIP.fromFFIType hs_bindgen_935f2aead358d9ef_base

{-|

    Marked @__attribute((pure))__@

    __C declaration:__ @hash@

    __defined at:__ @documentation\/doxygen_docs.h 463:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
hash ::
     RIP.Ptr RIP.CChar
     -- ^ __C declaration:__ @s@
  -> IO RIP.CInt
hash = hs_bindgen_935f2aead358d9ef

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_square@
foreign import ccall safe "hs_bindgen_39fef54c23d4e1ee" hs_bindgen_39fef54c23d4e1ee_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_square@
hs_bindgen_39fef54c23d4e1ee ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_39fef54c23d4e1ee =
  RIP.fromFFIType hs_bindgen_39fef54c23d4e1ee_base

{-|

    Marked @__attribute((const))__@

    __C declaration:__ @square@

    __defined at:__ @documentation\/doxygen_docs.h 465:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
square ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.CInt
square = hs_bindgen_39fef54c23d4e1ee

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_auto_brief_func@
foreign import ccall safe "hs_bindgen_29749932a4449f10" hs_bindgen_29749932a4449f10_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_auto_brief_func@
hs_bindgen_29749932a4449f10 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_29749932a4449f10 =
  RIP.fromFFIType hs_bindgen_29749932a4449f10_base

{-| Auto-brief function without explicit @brief tag.

    This tests that the first sentence is used as the brief description when no explicit @brief is present.

    [__@x@__]: The input value

    __Returns:__ The negated value

    __C declaration:__ @auto_brief_func@

    __defined at:__ @documentation\/doxygen_docs.h 486:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
auto_brief_func ::
     RIP.CInt
     {- ^

          [__@x@__]: The input value

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
auto_brief_func = hs_bindgen_29749932a4449f10

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_multi_paragraph_details@
foreign import ccall safe "hs_bindgen_03f0269c9296b8c0" hs_bindgen_03f0269c9296b8c0_base ::
     RIP.Ptr RIP.Void
  -> RIP.Word64
  -> IO ()

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_multi_paragraph_details@
hs_bindgen_03f0269c9296b8c0 ::
     RIP.Ptr RIP.CChar
  -> HsBindgen.Runtime.LibC.CSize
  -> IO ()
hs_bindgen_03f0269c9296b8c0 =
  RIP.fromFFIType hs_bindgen_03f0269c9296b8c0_base

{-| Multi-paragraph details.

    First paragraph of the detailed description. This explains the basic purpose of the function.

    Second paragraph with more context. This includes information about the algorithm and its complexity, which is O(n) in the input size.

    Third paragraph with usage notes. Callers should ensure that the buffer is large enough before calling this function.

    [__@buf@__]: Output buffer

    [__@len@__]: Buffer length

    __C declaration:__ @multi_paragraph_details@

    __defined at:__ @documentation\/doxygen_docs.h 503:6@

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
multi_paragraph_details = hs_bindgen_03f0269c9296b8c0

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_todo_remark_attention@
foreign import ccall safe "hs_bindgen_0446a821eb966f46" hs_bindgen_0446a821eb966f46_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_todo_remark_attention@
hs_bindgen_0446a821eb966f46 ::
     RIP.CInt
  -> IO (RIP.Ptr RIP.CInt)
hs_bindgen_0446a821eb966f46 =
  RIP.fromFFIType hs_bindgen_0446a821eb966f46_base

{-| Function with @todo and @remark.

    __Todo:__ Optimize this function for large inputs

    Add support for negative values

    __Remark:__ This function is thread-safe

    __ATTENTION:__ The caller must free the returned pointer

    [__@n@__]: Input count

    __Returns:__ Allocated array

    __C declaration:__ @todo_remark_attention@

    __defined at:__ @documentation\/doxygen_docs.h 516:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
todo_remark_attention ::
     RIP.CInt
     {- ^

          [__@n@__]: Input count

          __C declaration:__ @n@
     -}
  -> IO (RIP.Ptr RIP.CInt)
todo_remark_attention = hs_bindgen_0446a821eb966f46

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_html_entities_func@
foreign import ccall safe "hs_bindgen_25a9ea376bb793e9" hs_bindgen_25a9ea376bb793e9_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_html_entities_func@
hs_bindgen_25a9ea376bb793e9 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_25a9ea376bb793e9 =
  RIP.fromFFIType hs_bindgen_25a9ea376bb793e9_base

{-| HTML entities: & means AND, <tag> is a tag.

    Handles values < 0 and values > 100 differently. Copyright 2025.

    [__@x@__]: Input (> 0 required)

    __Returns:__ Result code

    __C declaration:__ @html_entities_func@

    __defined at:__ @documentation\/doxygen_docs.h 538:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
html_entities_func ::
     RIP.CInt
     {- ^

          [__@x@__]: Input (> 0 required)

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
html_entities_func = hs_bindgen_25a9ea376bb793e9

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_nested_inline_format@
foreign import ccall safe "hs_bindgen_6f311771e5e063e6" hs_bindgen_6f311771e5e063e6_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_nested_inline_format@
hs_bindgen_6f311771e5e063e6 ::
     RIP.CInt
  -> IO ()
hs_bindgen_6f311771e5e063e6 =
  RIP.fromFFIType hs_bindgen_6f311771e5e063e6_base

{-| Nested inline: __bold__ , @code@ , __@bold_code@ ,__ /__emph_bold__ ./

    [__@x@__]: Input value

    __C declaration:__ @nested_inline_format@

    __defined at:__ @documentation\/doxygen_docs.h 545:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
nested_inline_format ::
     RIP.CInt
     {- ^

          [__@x@__]: Input value

          __C declaration:__ @x@
     -}
  -> IO ()
nested_inline_format = hs_bindgen_6f311771e5e063e6

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_tagged_code_example@
foreign import ccall safe "hs_bindgen_34473a588f1157e4" hs_bindgen_34473a588f1157e4_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_tagged_code_example@
hs_bindgen_34473a588f1157e4 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_34473a588f1157e4 =
  RIP.fromFFIType hs_bindgen_34473a588f1157e4_base

{-| Language-tagged code block.

    Example usage:

    @
    int result = tagged_code_example(42);
    printf("Result: %d\n", result);
    @

    [__@x@__]: Input value

    __Returns:__ Processed value

    __C declaration:__ @tagged_code_example@

    __defined at:__ @documentation\/doxygen_docs.h 559:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
tagged_code_example ::
     RIP.CInt
     {- ^

          [__@x@__]: Input value

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
tagged_code_example = hs_bindgen_34473a588f1157e4

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_backslash_syntax@
foreign import ccall safe "hs_bindgen_471f4a3c2db31252" hs_bindgen_471f4a3c2db31252_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_documentationdoxygen_docs_Example_Safe_backslash_syntax@
hs_bindgen_471f4a3c2db31252 ::
     PtrConst.PtrConst RIP.CChar
  -> RIP.Ptr RIP.CChar
  -> IO RIP.CInt
hs_bindgen_471f4a3c2db31252 =
  RIP.fromFFIType hs_bindgen_471f4a3c2db31252_base

{-| Function documented with backslash syntax.

    This function uses backslash commands instead of @ commands to verify both syntaxes are handled equivalently.

    [__@input@__]: The input string

    [__@output@__]: The output buffer

    __Returns:__ Number of bytes written

    __See:__ 'auto_brief_func'

    @since 2.0

    __C declaration:__ @backslash_syntax@

    __defined at:__ @documentation\/doxygen_docs.h 574:5@

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
backslash_syntax = hs_bindgen_471f4a3c2db31252
