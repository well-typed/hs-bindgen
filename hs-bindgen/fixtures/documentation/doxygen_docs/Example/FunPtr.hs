{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( -- * Function Definitions
      Example.FunPtr.process_data
    , Example.FunPtr.process_file
    , Example.FunPtr.calculate_value
    , Example.FunPtr.html_example
    , Example.FunPtr.list_example
    , Example.FunPtr.dangerous_function
    , Example.FunPtr.detailed_return_codes
    , Example.FunPtr.old_function
    , Example.FunPtr.versioned_function
    , Example.FunPtr.process_buffer
    , Example.FunPtr.my_memcpy
    , Example.FunPtr.double_value
      -- ** I/O Helpers
    , Example.FunPtr.read_chunk
    , Example.FunPtr.write_chunk
      -- * Advanced Features
    , Example.FunPtr.complex_function
    , Example.FunPtr.hash
    , Example.FunPtr.square
      -- * Extra Doxygen Coverage
    , Example.FunPtr.auto_brief_func
    , Example.FunPtr.multi_paragraph_details
    , Example.FunPtr.todo_remark_attention
    , Example.FunPtr.html_entities_func
    , Example.FunPtr.nested_inline_format
    , Example.FunPtr.tagged_code_example
    , Example.FunPtr.backslash_syntax
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
  , "  char *arg1,"
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
  , "/* test_documentationdoxygen_docs_Example_get_read_chunk */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_373de63fa8345f66 (void)) ("
  , "  signed int arg1,"
  , "  void *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &read_chunk;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_write_chunk */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_78a029fc9807a27f (void)) ("
  , "  signed int arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &write_chunk;"
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
  , "/* test_documentationdoxygen_docs_Example_get_auto_brief_func */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_11a28326d5a28a43 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &auto_brief_func;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_multi_paragraph_details */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_05c7a7832fb84e2e (void)) ("
  , "  char *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &multi_paragraph_details;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_todo_remark_attention */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_092b9697f2154ac5 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &todo_remark_attention;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_html_entities_func */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2e8b5e895e58fc3d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &html_entities_func;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_nested_inline_format */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_db292926f05c7b36 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &nested_inline_format;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_tagged_code_example */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_492a4fd860afb02a (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &tagged_code_example;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_backslash_syntax */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9ef09ad63fac68e2 (void)) ("
  , "  char const *arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return &backslash_syntax;"
  , "}"
  ]))

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_data@
foreign import ccall unsafe "hs_bindgen_4a5cd66a4f26f8d5" hs_bindgen_4a5cd66a4f26f8d5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_data@
hs_bindgen_4a5cd66a4f26f8d5 :: IO (RIP.FunPtr (PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8 -> RIP.Ptr HsBindgen.Runtime.LibC.Word8 -> RIP.Ptr HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt))
hs_bindgen_4a5cd66a4f26f8d5 =
  RIP.fromFFIType hs_bindgen_4a5cd66a4f26f8d5_base

{-# NOINLINE process_data #-}
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
process_data :: RIP.FunPtr (PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8 -> RIP.Ptr HsBindgen.Runtime.LibC.Word8 -> RIP.Ptr HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt)
process_data =
  RIP.unsafePerformIO hs_bindgen_4a5cd66a4f26f8d5

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_file@
foreign import ccall unsafe "hs_bindgen_c8a059c65f18fea9" hs_bindgen_c8a059c65f18fea9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_file@
hs_bindgen_c8a059c65f18fea9 :: IO (RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> IO RIP.CBool))
hs_bindgen_c8a059c65f18fea9 =
  RIP.fromFFIType hs_bindgen_c8a059c65f18fea9_base

{-# NOINLINE process_file #-}
{-| Function with inline commands and formatting.

    This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

    [__@filename@__]: The @char*@ filename to process

    __Returns:__ @true@ if successful, @false@ otherwise

    __C declaration:__ @process_file@

    __defined at:__ @documentation\/doxygen_docs.h 122:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
process_file :: RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> IO RIP.CBool)
process_file =
  RIP.unsafePerformIO hs_bindgen_c8a059c65f18fea9

-- __unique:__ @test_documentationdoxygen_docs_Example_get_calculate_value@
foreign import ccall unsafe "hs_bindgen_5ab7e06724867ab3" hs_bindgen_5ab7e06724867ab3_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_calculate_value@
hs_bindgen_5ab7e06724867ab3 :: IO (RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO RIP.CInt))
hs_bindgen_5ab7e06724867ab3 =
  RIP.fromFFIType hs_bindgen_5ab7e06724867ab3_base

{-# NOINLINE calculate_value #-}
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
calculate_value :: RIP.FunPtr (RIP.CInt -> RIP.CInt -> IO RIP.CInt)
calculate_value =
  RIP.unsafePerformIO hs_bindgen_5ab7e06724867ab3

-- __unique:__ @test_documentationdoxygen_docs_Example_get_html_example@
foreign import ccall unsafe "hs_bindgen_3f74fb834b0cd46b" hs_bindgen_3f74fb834b0cd46b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_html_example@
hs_bindgen_3f74fb834b0cd46b :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CBool))
hs_bindgen_3f74fb834b0cd46b =
  RIP.fromFFIType hs_bindgen_3f74fb834b0cd46b_base

{-# NOINLINE html_example #-}
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
html_example :: RIP.FunPtr (RIP.CInt -> IO RIP.CBool)
html_example =
  RIP.unsafePerformIO hs_bindgen_3f74fb834b0cd46b

-- __unique:__ @test_documentationdoxygen_docs_Example_get_list_example@
foreign import ccall unsafe "hs_bindgen_2175b3c627db39cf" hs_bindgen_2175b3c627db39cf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_list_example@
hs_bindgen_2175b3c627db39cf :: IO (RIP.FunPtr (RIP.Ptr (PtrConst.PtrConst RIP.CChar) -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CBool))
hs_bindgen_2175b3c627db39cf =
  RIP.fromFFIType hs_bindgen_2175b3c627db39cf_base

{-# NOINLINE list_example #-}
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
list_example :: RIP.FunPtr (RIP.Ptr (PtrConst.PtrConst RIP.CChar) -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CBool)
list_example =
  RIP.unsafePerformIO hs_bindgen_2175b3c627db39cf

-- __unique:__ @test_documentationdoxygen_docs_Example_get_dangerous_function@
foreign import ccall unsafe "hs_bindgen_b7a4debd18827a19" hs_bindgen_b7a4debd18827a19_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_dangerous_function@
hs_bindgen_b7a4debd18827a19 :: IO (RIP.FunPtr (RIP.Ptr RIP.Void -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_b7a4debd18827a19 =
  RIP.fromFFIType hs_bindgen_b7a4debd18827a19_base

{-# NOINLINE dangerous_function #-}
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
dangerous_function :: RIP.FunPtr (RIP.Ptr RIP.Void -> IO (RIP.Ptr RIP.Void))
dangerous_function =
  RIP.unsafePerformIO hs_bindgen_b7a4debd18827a19

-- __unique:__ @test_documentationdoxygen_docs_Example_get_detailed_return_codes@
foreign import ccall unsafe "hs_bindgen_9700b22f82eedafe" hs_bindgen_9700b22f82eedafe_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_detailed_return_codes@
hs_bindgen_9700b22f82eedafe :: IO (RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> IO RIP.CInt))
hs_bindgen_9700b22f82eedafe =
  RIP.fromFFIType hs_bindgen_9700b22f82eedafe_base

{-# NOINLINE detailed_return_codes #-}
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
detailed_return_codes :: RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> IO RIP.CInt)
detailed_return_codes =
  RIP.unsafePerformIO hs_bindgen_9700b22f82eedafe

-- __unique:__ @test_documentationdoxygen_docs_Example_get_old_function@
foreign import ccall unsafe "hs_bindgen_49e0d34a627c6c19" hs_bindgen_49e0d34a627c6c19_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_old_function@
hs_bindgen_49e0d34a627c6c19 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_49e0d34a627c6c19 =
  RIP.fromFFIType hs_bindgen_49e0d34a627c6c19_base

{-# NOINLINE old_function #-}
{-| Function with deprecated annotation.

    __Deprecated:__ Use new_function() instead

    [__@old_param@__]: Legacy parameter

    __Returns:__ Legacy result

    __C declaration:__ @old_function@

    __defined at:__ @documentation\/doxygen_docs.h 212:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
old_function :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
old_function =
  RIP.unsafePerformIO hs_bindgen_49e0d34a627c6c19

-- __unique:__ @test_documentationdoxygen_docs_Example_get_versioned_function@
foreign import ccall unsafe "hs_bindgen_180cd7537e40ce99" hs_bindgen_180cd7537e40ce99_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_versioned_function@
hs_bindgen_180cd7537e40ce99 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_180cd7537e40ce99 =
  RIP.fromFFIType hs_bindgen_180cd7537e40ce99_base

{-# NOINLINE versioned_function #-}
{-| Function with version information.

    @since 1.0

    __Version:__ 1.2

    [__@data@__]: Input data

    __Returns:__ Processed data

    __C declaration:__ @versioned_function@

    __defined at:__ @documentation\/doxygen_docs.h 222:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
versioned_function :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
versioned_function =
  RIP.unsafePerformIO hs_bindgen_180cd7537e40ce99

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_buffer@
foreign import ccall unsafe "hs_bindgen_f4ccc6d90e8d3ebd" hs_bindgen_f4ccc6d90e8d3ebd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_process_buffer@
hs_bindgen_f4ccc6d90e8d3ebd :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem (CA.ConstantArray 64 RIP.CChar)) -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt))
hs_bindgen_f4ccc6d90e8d3ebd =
  RIP.fromFFIType hs_bindgen_f4ccc6d90e8d3ebd_base

{-# NOINLINE process_buffer #-}
{-| Static array parameter.

    [__@buffer@__]: Buffer with minimum size

    [__@size@__]: Actual buffer size

    __Returns:__ Number of bytes written

    __C declaration:__ @process_buffer@

    __defined at:__ @documentation\/doxygen_docs.h 338:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
process_buffer :: RIP.FunPtr (RIP.Ptr (IsA.Elem (CA.ConstantArray 64 RIP.CChar)) -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt)
process_buffer =
  RIP.unsafePerformIO hs_bindgen_f4ccc6d90e8d3ebd

-- __unique:__ @test_documentationdoxygen_docs_Example_get_my_memcpy@
foreign import ccall unsafe "hs_bindgen_6a74d376c901b531" hs_bindgen_6a74d376c901b531_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_my_memcpy@
hs_bindgen_6a74d376c901b531 :: IO (RIP.FunPtr (RIP.Ptr RIP.Void -> PtrConst.PtrConst RIP.Void -> HsBindgen.Runtime.LibC.CSize -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_6a74d376c901b531 =
  RIP.fromFFIType hs_bindgen_6a74d376c901b531_base

{-# NOINLINE my_memcpy #-}
{-| Function with restrict pointers.

    [__@dest@__]: Destination buffer (restrict)

    [__@src@__]: Source buffer (restrict)

    [__@n@__]: Number of bytes

    __Returns:__ Destination pointer

    __C declaration:__ @my_memcpy@

    __defined at:__ @documentation\/doxygen_docs.h 348:7@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
my_memcpy :: RIP.FunPtr (RIP.Ptr RIP.Void -> PtrConst.PtrConst RIP.Void -> HsBindgen.Runtime.LibC.CSize -> IO (RIP.Ptr RIP.Void))
my_memcpy =
  RIP.unsafePerformIO hs_bindgen_6a74d376c901b531

-- __unique:__ @test_documentationdoxygen_docs_Example_get_double_value@
foreign import ccall unsafe "hs_bindgen_156a92f3c5176105" hs_bindgen_156a92f3c5176105_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_double_value@
hs_bindgen_156a92f3c5176105 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_156a92f3c5176105 =
  RIP.fromFFIType hs_bindgen_156a92f3c5176105_base

{-# NOINLINE double_value #-}
{-| Inline function.

    [__@x@__]: Input value

    __Returns:__ Doubled value

    __C declaration:__ @double_value@

    __defined at:__ @documentation\/doxygen_docs.h 356:19@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
double_value :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
double_value =
  RIP.unsafePerformIO hs_bindgen_156a92f3c5176105

-- __unique:__ @test_documentationdoxygen_docs_Example_get_read_chunk@
foreign import ccall unsafe "hs_bindgen_373de63fa8345f66" hs_bindgen_373de63fa8345f66_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_read_chunk@
hs_bindgen_373de63fa8345f66 :: IO (RIP.FunPtr (RIP.CInt -> RIP.Ptr RIP.Void -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt))
hs_bindgen_373de63fa8345f66 =
  RIP.fromFFIType hs_bindgen_373de63fa8345f66_base

{-# NOINLINE read_chunk #-}
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
read_chunk :: RIP.FunPtr (RIP.CInt -> RIP.Ptr RIP.Void -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt)
read_chunk =
  RIP.unsafePerformIO hs_bindgen_373de63fa8345f66

-- __unique:__ @test_documentationdoxygen_docs_Example_get_write_chunk@
foreign import ccall unsafe "hs_bindgen_78a029fc9807a27f" hs_bindgen_78a029fc9807a27f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_write_chunk@
hs_bindgen_78a029fc9807a27f :: IO (RIP.FunPtr (RIP.CInt -> PtrConst.PtrConst RIP.Void -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt))
hs_bindgen_78a029fc9807a27f =
  RIP.fromFFIType hs_bindgen_78a029fc9807a27f_base

{-# NOINLINE write_chunk #-}
{-| Write a chunk of bytes from the provided buffer.

    [__@fd@__]: File descriptor

    [__@buf@__]: Input buffer

    [__@count@__]: Number of bytes to write

    __Returns:__ Number of bytes written

    __C declaration:__ @write_chunk@

    __defined at:__ @documentation\/doxygen_docs.h 397:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
write_chunk :: RIP.FunPtr (RIP.CInt -> PtrConst.PtrConst RIP.Void -> HsBindgen.Runtime.LibC.CSize -> IO RIP.CInt)
write_chunk =
  RIP.unsafePerformIO hs_bindgen_78a029fc9807a27f

-- __unique:__ @test_documentationdoxygen_docs_Example_get_complex_function@
foreign import ccall unsafe "hs_bindgen_dfa39b6be50cb2ca" hs_bindgen_dfa39b6be50cb2ca_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_complex_function@
hs_bindgen_dfa39b6be50cb2ca :: IO (RIP.FunPtr (RIP.Ptr Config_t -> PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8 -> HsBindgen.Runtime.LibC.CSize -> IO Status_code_t))
hs_bindgen_dfa39b6be50cb2ca =
  RIP.fromFFIType hs_bindgen_dfa39b6be50cb2ca_base

{-# NOINLINE complex_function #-}
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
complex_function :: RIP.FunPtr (RIP.Ptr Config_t -> PtrConst.PtrConst HsBindgen.Runtime.LibC.Word8 -> HsBindgen.Runtime.LibC.CSize -> IO Status_code_t)
complex_function =
  RIP.unsafePerformIO hs_bindgen_dfa39b6be50cb2ca

-- __unique:__ @test_documentationdoxygen_docs_Example_get_hash@
foreign import ccall unsafe "hs_bindgen_8ad88b79fd71f9d4" hs_bindgen_8ad88b79fd71f9d4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_hash@
hs_bindgen_8ad88b79fd71f9d4 :: IO (RIP.FunPtr (RIP.Ptr RIP.CChar -> IO RIP.CInt))
hs_bindgen_8ad88b79fd71f9d4 =
  RIP.fromFFIType hs_bindgen_8ad88b79fd71f9d4_base

{-# NOINLINE hash #-}
{-| __C declaration:__ @hash@

    __defined at:__ @documentation\/doxygen_docs.h 463:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
hash :: RIP.FunPtr (RIP.Ptr RIP.CChar -> IO RIP.CInt)
hash =
  RIP.unsafePerformIO hs_bindgen_8ad88b79fd71f9d4

-- __unique:__ @test_documentationdoxygen_docs_Example_get_square@
foreign import ccall unsafe "hs_bindgen_7ede0f7ec1b30650" hs_bindgen_7ede0f7ec1b30650_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_square@
hs_bindgen_7ede0f7ec1b30650 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_7ede0f7ec1b30650 =
  RIP.fromFFIType hs_bindgen_7ede0f7ec1b30650_base

{-# NOINLINE square #-}
{-| __C declaration:__ @square@

    __defined at:__ @documentation\/doxygen_docs.h 465:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
square :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
square =
  RIP.unsafePerformIO hs_bindgen_7ede0f7ec1b30650

-- __unique:__ @test_documentationdoxygen_docs_Example_get_auto_brief_func@
foreign import ccall unsafe "hs_bindgen_11a28326d5a28a43" hs_bindgen_11a28326d5a28a43_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_auto_brief_func@
hs_bindgen_11a28326d5a28a43 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_11a28326d5a28a43 =
  RIP.fromFFIType hs_bindgen_11a28326d5a28a43_base

{-# NOINLINE auto_brief_func #-}
{-| Auto-brief function without explicit @brief tag.

    This tests that the first sentence is used as the brief description when no explicit @brief is present.

    [__@x@__]: The input value

    __Returns:__ The negated value

    __C declaration:__ @auto_brief_func@

    __defined at:__ @documentation\/doxygen_docs.h 486:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
auto_brief_func :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
auto_brief_func =
  RIP.unsafePerformIO hs_bindgen_11a28326d5a28a43

-- __unique:__ @test_documentationdoxygen_docs_Example_get_multi_paragraph_details@
foreign import ccall unsafe "hs_bindgen_05c7a7832fb84e2e" hs_bindgen_05c7a7832fb84e2e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_multi_paragraph_details@
hs_bindgen_05c7a7832fb84e2e :: IO (RIP.FunPtr (RIP.Ptr RIP.CChar -> HsBindgen.Runtime.LibC.CSize -> IO ()))
hs_bindgen_05c7a7832fb84e2e =
  RIP.fromFFIType hs_bindgen_05c7a7832fb84e2e_base

{-# NOINLINE multi_paragraph_details #-}
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
multi_paragraph_details :: RIP.FunPtr (RIP.Ptr RIP.CChar -> HsBindgen.Runtime.LibC.CSize -> IO ())
multi_paragraph_details =
  RIP.unsafePerformIO hs_bindgen_05c7a7832fb84e2e

-- __unique:__ @test_documentationdoxygen_docs_Example_get_todo_remark_attention@
foreign import ccall unsafe "hs_bindgen_092b9697f2154ac5" hs_bindgen_092b9697f2154ac5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_todo_remark_attention@
hs_bindgen_092b9697f2154ac5 :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt)))
hs_bindgen_092b9697f2154ac5 =
  RIP.fromFFIType hs_bindgen_092b9697f2154ac5_base

{-# NOINLINE todo_remark_attention #-}
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
todo_remark_attention :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr RIP.CInt))
todo_remark_attention =
  RIP.unsafePerformIO hs_bindgen_092b9697f2154ac5

-- __unique:__ @test_documentationdoxygen_docs_Example_get_html_entities_func@
foreign import ccall unsafe "hs_bindgen_2e8b5e895e58fc3d" hs_bindgen_2e8b5e895e58fc3d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_html_entities_func@
hs_bindgen_2e8b5e895e58fc3d :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_2e8b5e895e58fc3d =
  RIP.fromFFIType hs_bindgen_2e8b5e895e58fc3d_base

{-# NOINLINE html_entities_func #-}
{-| HTML entities: & means AND, <tag> is a tag.

    Handles values < 0 and values > 100 differently. Copyright 2025.

    [__@x@__]: Input (> 0 required)

    __Returns:__ Result code

    __C declaration:__ @html_entities_func@

    __defined at:__ @documentation\/doxygen_docs.h 538:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
html_entities_func :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
html_entities_func =
  RIP.unsafePerformIO hs_bindgen_2e8b5e895e58fc3d

-- __unique:__ @test_documentationdoxygen_docs_Example_get_nested_inline_format@
foreign import ccall unsafe "hs_bindgen_db292926f05c7b36" hs_bindgen_db292926f05c7b36_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_nested_inline_format@
hs_bindgen_db292926f05c7b36 :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_db292926f05c7b36 =
  RIP.fromFFIType hs_bindgen_db292926f05c7b36_base

{-# NOINLINE nested_inline_format #-}
{-| Nested inline: __bold__ , @code@ , __@bold_code@ ,__ /__emph_bold__ ./

    [__@x@__]: Input value

    __C declaration:__ @nested_inline_format@

    __defined at:__ @documentation\/doxygen_docs.h 545:6@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
nested_inline_format :: RIP.FunPtr (RIP.CInt -> IO ())
nested_inline_format =
  RIP.unsafePerformIO hs_bindgen_db292926f05c7b36

-- __unique:__ @test_documentationdoxygen_docs_Example_get_tagged_code_example@
foreign import ccall unsafe "hs_bindgen_492a4fd860afb02a" hs_bindgen_492a4fd860afb02a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_tagged_code_example@
hs_bindgen_492a4fd860afb02a :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_492a4fd860afb02a =
  RIP.fromFFIType hs_bindgen_492a4fd860afb02a_base

{-# NOINLINE tagged_code_example #-}
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
tagged_code_example :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
tagged_code_example =
  RIP.unsafePerformIO hs_bindgen_492a4fd860afb02a

-- __unique:__ @test_documentationdoxygen_docs_Example_get_backslash_syntax@
foreign import ccall unsafe "hs_bindgen_9ef09ad63fac68e2" hs_bindgen_9ef09ad63fac68e2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_backslash_syntax@
hs_bindgen_9ef09ad63fac68e2 :: IO (RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> RIP.Ptr RIP.CChar -> IO RIP.CInt))
hs_bindgen_9ef09ad63fac68e2 =
  RIP.fromFFIType hs_bindgen_9ef09ad63fac68e2_base

{-# NOINLINE backslash_syntax #-}
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
backslash_syntax :: RIP.FunPtr (PtrConst.PtrConst RIP.CChar -> RIP.Ptr RIP.CChar -> IO RIP.CInt)
backslash_syntax =
  RIP.unsafePerformIO hs_bindgen_9ef09ad63fac68e2
