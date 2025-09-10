{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.Array.Byte
import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FlexibleArrayMember
import qualified HsBindgen.Runtime.Prelude
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include <doxygen_docs.h>\n/* get_global_counter_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_doxygen_docs_7c3b3bc7cc470742 (void) { return &global_counter; } \n/* get_version_string_ptr */ __attribute__ ((const)) char const **hs_bindgen_test_doxygen_docs_a8499810bd5203c6 (void) { return &version_string; } \nsigned int hs_bindgen_test_doxygen_docs_d3011436e073569c (uint8_t const *arg1, uint8_t *arg2, size_t *arg3) { return process_data(arg1, arg2, arg3); }\n/* get_process_data_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_36ecba874daa1325 (void)) (uint8_t const *arg1, uint8_t *arg2, size_t *arg3) { return &process_data; } \n_Bool hs_bindgen_test_doxygen_docs_4e9b999ed81440c0 (char *arg1) { return process_file(arg1); }\n/* get_process_file_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_doxygen_docs_25908c11fd21c934 (void)) (char *arg1) { return &process_file; } \nsigned int hs_bindgen_test_doxygen_docs_788bfbb9e824261c (signed int arg1, signed int arg2) { return calculate_value(arg1, arg2); }\n/* get_calculate_value_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_bfa9c55857b9f9a4 (void)) (signed int arg1, signed int arg2) { return &calculate_value; } \n_Bool hs_bindgen_test_doxygen_docs_3763cc54606bc611 (signed int arg1) { return html_example(arg1); }\n/* get_html_example_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_doxygen_docs_be37db4c71dfaad8 (void)) (signed int arg1) { return &html_example; } \n_Bool hs_bindgen_test_doxygen_docs_9d6d039971edcd60 (char const **arg1, size_t arg2) { return list_example(arg1, arg2); }\n/* get_list_example_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_doxygen_docs_c40a51053a97fb29 (void)) (char const **arg1, size_t arg2) { return &list_example; } \nvoid *hs_bindgen_test_doxygen_docs_b9c683fb9a695cc9 (void *arg1) { return dangerous_function(arg1); }\n/* get_dangerous_function_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_doxygen_docs_291c4151bd4c3637 (void)) (void *arg1) { return &dangerous_function; } \nsigned int hs_bindgen_test_doxygen_docs_fb3e3158714e01f5 (char const *arg1) { return detailed_return_codes(arg1); }\n/* get_detailed_return_codes_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_a92bbc7b93300d3b (void)) (char const *arg1) { return &detailed_return_codes; } \nsigned int hs_bindgen_test_doxygen_docs_84140a44d1bd8380 (signed int arg1) { return old_function(arg1); }\n/* get_old_function_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_985c4be1f80d7557 (void)) (signed int arg1) { return &old_function; } \nsigned int hs_bindgen_test_doxygen_docs_7549924ba8d1e7b8 (signed int arg1) { return versioned_function(arg1); }\n/* get_versioned_function_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_6b616614abe7d3f0 (void)) (signed int arg1) { return &versioned_function; } \nsigned int hs_bindgen_test_doxygen_docs_1ce818d0ed47ea3e (char *arg1, size_t arg2) { return process_buffer(arg1, arg2); }\n/* get_process_buffer_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_fdf2d8f3abafc975 (void)) (char arg1[64], size_t arg2) { return &process_buffer; } \nvoid *hs_bindgen_test_doxygen_docs_bcbe640b60445a4f (void *arg1, void const *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }\n/* get_my_memcpy_ptr */ __attribute__ ((const)) void *(*hs_bindgen_test_doxygen_docs_de9d3228e8bac25c (void)) (void *arg1, void const *arg2, size_t arg3) { return &my_memcpy; } \nsigned int hs_bindgen_test_doxygen_docs_1c952f01cc07bb64 (signed int arg1) { return double_value(arg1); }\n/* get_double_value_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_faec60e7f49d446c (void)) (signed int arg1) { return &double_value; } \nstatus_code_t hs_bindgen_test_doxygen_docs_a9d86cb54c7498fe (config_t *arg1, uint8_t const *arg2, size_t arg3) { return complex_function(arg1, arg2, arg3); }\n/* get_complex_function_ptr */ __attribute__ ((const)) status_code_t (*hs_bindgen_test_doxygen_docs_b7ae5186dd939781 (void)) (config_t *arg1, uint8_t const *arg2, size_t arg3) { return &complex_function; } \nsigned int hs_bindgen_test_doxygen_docs_71214e4420f53a0e (char *arg1) { return hash(arg1); }\n/* get_hash_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_241bedb74b8016f3 (void)) (char *arg1) { return &hash; } \nsigned int hs_bindgen_test_doxygen_docs_8effe939268709e4 (signed int arg1) { return square(arg1); }\n/* get_square_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_doxygen_docs_631c7b52d4d4fe3a (void)) (signed int arg1) { return &square; } \n")

{-| __C declaration:__ @MAX_NAME_LENGTH@

    __defined at:__ @doxygen_docs.h:39:9@

    __exported by:__ @doxygen_docs.h@
-}
mAX_NAME_LENGTH :: FC.CInt
mAX_NAME_LENGTH = (64 :: FC.CInt)

{-| This is the comment __title__

  > size_type

  Size type for this library

__C declaration:__ @size_type@

__defined at:__ @doxygen_docs.h:54:16@

__exported by:__ @doxygen_docs.h@
-}
newtype Size_type = Size_type
  { un_Size_type :: HsBindgen.Runtime.Prelude.CSize
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-|

  > extern int global_counter

  Global counter variable

  This variable tracks the number of operations performed.

__C declaration:__ @global_counter@

__defined at:__ @doxygen_docs.h:61:12@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_7c3b3bc7cc470742" hs_bindgen_test_doxygen_docs_7c3b3bc7cc470742
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE global_counter_ptr #-}

global_counter_ptr :: Ptr.Ptr FC.CInt
global_counter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_7c3b3bc7cc470742

{-|

  > extern const char* version_string

  Version string constant

__C declaration:__ @version_string@

__defined at:__ @doxygen_docs.h:67:20@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_a8499810bd5203c6" hs_bindgen_test_doxygen_docs_a8499810bd5203c6
  :: IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE version_string_ptr #-}

version_string_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
version_string_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_a8499810bd5203c6

{-| This is the comment @title@

  Forward declaration with documentation

__C declaration:__ @forward_declared_struct@

__defined at:__ @doxygen_docs.h:72:8@

__exported by:__ @doxygen_docs.h@
-}
data Forward_declared_struct

{-|

  Forward declaration of union

__C declaration:__ @forward_declared_union@

__defined at:__ @doxygen_docs.h:77:7@

__exported by:__ @doxygen_docs.h@
-}
data Forward_declared_union

{-|

  > color_enum

  Color enumeration without typedef

__C declaration:__ @color_enum@

__defined at:__ @doxygen_docs.h:83:6@

__exported by:__ @doxygen_docs.h@
-}
newtype Color_enum = Color_enum
  { un_Color_enum :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Color_enum where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Color_enum
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Color_enum un_Color_enum2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Color_enum2

instance HsBindgen.Runtime.CEnum.CEnum Color_enum where

  type CEnumZ Color_enum = FC.CUInt

  toCEnum = Color_enum

  fromCEnum = un_Color_enum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "COLOR_RED")
                                                     , (1, Data.List.NonEmpty.singleton "COLOR_GREEN")
                                                     , (2, Data.List.NonEmpty.singleton "COLOR_BLUE")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Color_enum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Color_enum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Color_enum where

  minDeclaredValue = COLOR_RED

  maxDeclaredValue = COLOR_BLUE

instance Show Color_enum where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Color_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern COLOR_RED :: Color_enum
pattern COLOR_RED = Color_enum 0

pattern COLOR_GREEN :: Color_enum
pattern COLOR_GREEN = Color_enum 1

pattern COLOR_BLUE :: Color_enum
pattern COLOR_BLUE = Color_enum 2

{-|

  Function with detailed parameter documentation

  This function shows different parameter directions and types.

  [__@input_data@ /(input)/__]: Input data buffer

  [__@output_data@ /(output)/__]: Output data buffer

  [__@size@ /(input,output)/__]: Size of data, updated on return

  __returns:__ Status code (0 = success, -1 = error)

__C declaration:__ @process_data@

__defined at:__ @doxygen_docs.h:105:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_d3011436e073569c" process_data
  :: Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
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

  Function with detailed parameter documentation

  This function shows different parameter directions and types.

  [__@input_data@ /(input)/__]: Input data buffer

  [__@output_data@ /(output)/__]: Output data buffer

  [__@size@ /(input,output)/__]: Size of data, updated on return

  __returns:__ Status code (0 = success, -1 = error)

__C declaration:__ @process_data@

__defined at:__ @doxygen_docs.h:105:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_36ecba874daa1325" hs_bindgen_test_doxygen_docs_36ecba874daa1325
  :: IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CSize) -> IO FC.CInt))

{-# NOINLINE process_data_ptr #-}

process_data_ptr :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.CSize) -> IO FC.CInt)
process_data_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_36ecba874daa1325

{-|

  Function with inline commands and formatting

  This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

  [__@filename@ /(input)/__]: The @char*@ filename to process

  __returns:__ @true@ if successful, @false@ otherwise

__C declaration:__ @process_file@

__defined at:__ @doxygen_docs.h:116:6@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_4e9b999ed81440c0" process_file
  :: Ptr.Ptr FC.CChar
     {- ^

        [__@filename@ /(input)/__]: The @char*@ filename to process

     __C declaration:__ @filename@
     -}
  -> IO FC.CBool

{-|

  Function with inline commands and formatting

  This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

  [__@filename@ /(input)/__]: The @char*@ filename to process

  __returns:__ @true@ if successful, @false@ otherwise

__C declaration:__ @process_file@

__defined at:__ @doxygen_docs.h:116:6@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_25908c11fd21c934" hs_bindgen_test_doxygen_docs_25908c11fd21c934
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CBool))

{-# NOINLINE process_file_ptr #-}

process_file_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CBool)
process_file_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_25908c11fd21c934

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

__defined at:__ @doxygen_docs.h:131:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_788bfbb9e824261c" calculate_value
  :: FC.CInt
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

__defined at:__ @doxygen_docs.h:131:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_bfa9c55857b9f9a4" hs_bindgen_test_doxygen_docs_bfa9c55857b9f9a4
  :: IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE calculate_value_ptr #-}

calculate_value_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
calculate_value_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_bfa9c55857b9f9a4

{-|

  Function with HTML formatting

  This function demonstrates HTML bold and italic text. It also shows HTML code formatting.

  Input Output 0 false 1 true

  [__@value@ /(input)/__]: Input value

  __returns:__ Boolean result

__C declaration:__ @html_example@

__defined at:__ @doxygen_docs.h:148:6@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_3763cc54606bc611" html_example
  :: FC.CInt
     {- ^

        [__@value@ /(input)/__]: Input value

     __C declaration:__ @value@
     -}
  -> IO FC.CBool

{-|

  Function with HTML formatting

  This function demonstrates HTML bold and italic text. It also shows HTML code formatting.

  Input Output 0 false 1 true

  [__@value@ /(input)/__]: Input value

  __returns:__ Boolean result

__C declaration:__ @html_example@

__defined at:__ @doxygen_docs.h:148:6@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_be37db4c71dfaad8" hs_bindgen_test_doxygen_docs_be37db4c71dfaad8
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CBool))

{-# NOINLINE html_example_ptr #-}

html_example_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CBool)
html_example_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_be37db4c71dfaad8

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

__defined at:__ @doxygen_docs.h:174:6@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_9d6d039971edcd60" list_example
  :: Ptr.Ptr (Ptr.Ptr FC.CChar)
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

__defined at:__ @doxygen_docs.h:174:6@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_c40a51053a97fb29" hs_bindgen_test_doxygen_docs_c40a51053a97fb29
  :: IO (Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool))

{-# NOINLINE list_example_ptr #-}

list_example_ptr :: Ptr.FunPtr ((Ptr.Ptr (Ptr.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool)
list_example_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_c40a51053a97fb29

{-|

  Function with warnings and notes

  __/WARNING:/__ This function may cause side effects

  __Note:__ Use with caution in multithreaded environments

  __see:__ related_function() for similar functionality

  [__@ptr@ /(input)/__]: Pointer to data

  __returns:__ Modified pointer

__C declaration:__ @dangerous_function@

__defined at:__ @doxygen_docs.h:186:7@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_b9c683fb9a695cc9" dangerous_function
  :: Ptr.Ptr Void
     {- ^

        [__@ptr@ /(input)/__]: Pointer to data

     __C declaration:__ @ptr@
     -}
  -> IO (Ptr.Ptr Void)

{-|

  Function with warnings and notes

  __/WARNING:/__ This function may cause side effects

  __Note:__ Use with caution in multithreaded environments

  __see:__ related_function() for similar functionality

  [__@ptr@ /(input)/__]: Pointer to data

  __returns:__ Modified pointer

__C declaration:__ @dangerous_function@

__defined at:__ @doxygen_docs.h:186:7@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_291c4151bd4c3637" hs_bindgen_test_doxygen_docs_291c4151bd4c3637
  :: IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void)))

{-# NOINLINE dangerous_function_ptr #-}

dangerous_function_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> IO (Ptr.Ptr Void))
dangerous_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_291c4151bd4c3637

{-|

  Function with return value details

  [__@input@ /(input)/__]: Input string

  __returns:__ 0 Success

  __returns:__ -1 Invalid input

  __returns:__ -2 Memory allocation failed

  __returns:__ -3 Processing error

__C declaration:__ @detailed_return_codes@

__defined at:__ @doxygen_docs.h:197:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_fb3e3158714e01f5" detailed_return_codes
  :: Ptr.Ptr FC.CChar
     {- ^

        [__@input@ /(input)/__]: Input string

     __C declaration:__ @input@
     -}
  -> IO FC.CInt

{-|

  Function with return value details

  [__@input@ /(input)/__]: Input string

  __returns:__ 0 Success

  __returns:__ -1 Invalid input

  __returns:__ -2 Memory allocation failed

  __returns:__ -3 Processing error

__C declaration:__ @detailed_return_codes@

__defined at:__ @doxygen_docs.h:197:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_a92bbc7b93300d3b" hs_bindgen_test_doxygen_docs_a92bbc7b93300d3b
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE detailed_return_codes_ptr #-}

detailed_return_codes_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
detailed_return_codes_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_a92bbc7b93300d3b

{-|

  Function with deprecated annotation

  __deprecated:__ Use new_function() instead

  [__@old_param@ /(input)/__]: Legacy parameter

  __returns:__ Legacy result

__C declaration:__ @old_function@

__defined at:__ @doxygen_docs.h:206:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_84140a44d1bd8380" old_function
  :: FC.CInt
     {- ^

        [__@old_param@ /(input)/__]: Legacy parameter

     __C declaration:__ @old_param@
     -}
  -> IO FC.CInt

{-|

  Function with deprecated annotation

  __deprecated:__ Use new_function() instead

  [__@old_param@ /(input)/__]: Legacy parameter

  __returns:__ Legacy result

__C declaration:__ @old_function@

__defined at:__ @doxygen_docs.h:206:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_985c4be1f80d7557" hs_bindgen_test_doxygen_docs_985c4be1f80d7557
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE old_function_ptr #-}

old_function_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
old_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_985c4be1f80d7557

{-|

  Function with version information

  @since:  1.0

  [__@data@ /(input)/__]: Input data

  __returns:__ Processed data

__C declaration:__ @versioned_function@

__defined at:__ @doxygen_docs.h:216:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_7549924ba8d1e7b8" versioned_function
  :: FC.CInt
     {- ^ __C declaration:__ @data'@
     -}
  -> IO FC.CInt

{-|

  Function with version information

  @since:  1.0

  [__@data@ /(input)/__]: Input data

  __returns:__ Processed data

__C declaration:__ @versioned_function@

__defined at:__ @doxygen_docs.h:216:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_6b616614abe7d3f0" hs_bindgen_test_doxygen_docs_6b616614abe7d3f0
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE versioned_function_ptr #-}

versioned_function_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
versioned_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_6b616614abe7d3f0

{-|

  Callback function type

  [__@event_type@ /(input)/__]: Type of event

  [__@user_data@ /(input)/__]: User-provided data

  __returns:__ Handling result

__C declaration:__ @event_callback_t@

__defined at:__ @doxygen_docs.h:225:15@

__exported by:__ @doxygen_docs.h@
-}
newtype Event_callback_t = Event_callback_t
  { un_Event_callback_t :: Ptr.FunPtr (FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Structure with documented fields

  This structure demonstrates field documentation.

__C declaration:__ @config_t@

__defined at:__ @doxygen_docs.h:232:9@

__exported by:__ @doxygen_docs.h@
-}
data Config_t = Config_t
  { config_t_id :: HsBindgen.Runtime.Prelude.Word32
    {- ^

       Unique identifier

    __C declaration:__ @id@

    __defined at:__ @doxygen_docs.h:234:14@

    __exported by:__ @doxygen_docs.h@
    -}
  , config_t_name :: (HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar
    {- ^

       Human-readable name

    __C declaration:__ @name@

    __defined at:__ @doxygen_docs.h:237:10@

    __exported by:__ @doxygen_docs.h@
    -}
  , config_t_flags :: HsBindgen.Runtime.Prelude.Word32
    {- ^

       Configuration flags

    __C declaration:__ @flags@

    __defined at:__ @doxygen_docs.h:240:14@

    __exported by:__ @doxygen_docs.h@
    -}
  , config_t_callback :: Event_callback_t
    {- ^

       Optional callback function

       See also: 'Event_callback_t'

    __C declaration:__ @callback@

    __defined at:__ @doxygen_docs.h:247:22@

    __exported by:__ @doxygen_docs.h@
    -}
  , config_t_user_data :: Ptr.Ptr Void
    {- ^

       User data for callback

    __C declaration:__ @user_data@

    __defined at:__ @doxygen_docs.h:250:11@

    __exported by:__ @doxygen_docs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Config_t where

  sizeOf = \_ -> (88 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Config_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (68 :: Int)
      <*> F.peekByteOff ptr0 (72 :: Int)
      <*> F.peekByteOff ptr0 (80 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config_t
            config_t_id2
            config_t_name3
            config_t_flags4
            config_t_callback5
            config_t_user_data6 ->
                 F.pokeByteOff ptr0 (0 :: Int) config_t_id2
              >> F.pokeByteOff ptr0 (4 :: Int) config_t_name3
              >> F.pokeByteOff ptr0 (68 :: Int) config_t_flags4
              >> F.pokeByteOff ptr0 (72 :: Int) config_t_callback5
              >> F.pokeByteOff ptr0 (80 :: Int) config_t_user_data6

{-|

  Enumeration with documented values

  This enum shows different status codes.

__C declaration:__ @status_code_t@

__defined at:__ @doxygen_docs.h:258:9@

__exported by:__ @doxygen_docs.h@
-}
newtype Status_code_t = Status_code_t
  { un_Status_code_t :: FC.CInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Status_code_t where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Status_code_t
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Status_code_t un_Status_code_t2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Status_code_t2

instance HsBindgen.Runtime.CEnum.CEnum Status_code_t where

  type CEnumZ Status_code_t = FC.CInt

  toCEnum = Status_code_t

  fromCEnum = un_Status_code_t

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (-99, Data.List.NonEmpty.singleton "STATUS_ERROR")
                                                     , (-3, Data.List.NonEmpty.singleton "STATUS_TIMEOUT")
                                                     , (-2, Data.List.NonEmpty.singleton "STATUS_NO_MEMORY")
                                                     , (-1, Data.List.NonEmpty.singleton "STATUS_INVALID_PARAM")
                                                     , (0, Data.List.NonEmpty.singleton "STATUS_OK")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Status_code_t"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Status_code_t"

instance Show Status_code_t where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Status_code_t where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-|

  Operation successful

__C declaration:__ @STATUS_OK@

__defined at:__ @doxygen_docs.h:260:5@

__exported by:__ @doxygen_docs.h@
-}
pattern STATUS_OK :: Status_code_t
pattern STATUS_OK = Status_code_t 0

{-|

  Invalid parameter provided

__C declaration:__ @STATUS_INVALID_PARAM@

__defined at:__ @doxygen_docs.h:263:5@

__exported by:__ @doxygen_docs.h@
-}
pattern STATUS_INVALID_PARAM :: Status_code_t
pattern STATUS_INVALID_PARAM = Status_code_t (-1)

{-|

  Memory allocation failed

__C declaration:__ @STATUS_NO_MEMORY@

__defined at:__ @doxygen_docs.h:266:5@

__exported by:__ @doxygen_docs.h@
-}
pattern STATUS_NO_MEMORY :: Status_code_t
pattern STATUS_NO_MEMORY = Status_code_t (-2)

{-|

  Operation timed out

__C declaration:__ @STATUS_TIMEOUT@

__defined at:__ @doxygen_docs.h:269:5@

__exported by:__ @doxygen_docs.h@
-}
pattern STATUS_TIMEOUT :: Status_code_t
pattern STATUS_TIMEOUT = Status_code_t (-3)

{-|

  Generic error

__C declaration:__ @STATUS_ERROR@

__defined at:__ @doxygen_docs.h:272:5@

__exported by:__ @doxygen_docs.h@
-}
pattern STATUS_ERROR :: Status_code_t
pattern STATUS_ERROR = Status_code_t (-99)

{-|

  Structured representation

  Allows access to high and low parts separately

__C declaration:__ @data_union_t_as_parts@

__defined at:__ @doxygen_docs.h:290:5@

__exported by:__ @doxygen_docs.h@
-}
data Data_union_t_as_parts = Data_union_t_as_parts
  { data_union_t_as_parts_low :: HsBindgen.Runtime.Prelude.Word16
    {- ^

       Low 16 bits

    __C declaration:__ @low@

    __defined at:__ @doxygen_docs.h:291:18@

    __exported by:__ @doxygen_docs.h@
    -}
  , data_union_t_as_parts_high :: HsBindgen.Runtime.Prelude.Word16
    {- ^

       High 16 bits

    __C declaration:__ @high@

    __defined at:__ @doxygen_docs.h:292:18@

    __exported by:__ @doxygen_docs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Data_union_t_as_parts where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Data_union_t_as_parts
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Data_union_t_as_parts data_union_t_as_parts_low2 data_union_t_as_parts_high3 ->
               F.pokeByteOff ptr0 (0 :: Int) data_union_t_as_parts_low2
            >> F.pokeByteOff ptr0 (2 :: Int) data_union_t_as_parts_high3

{-|

  > data_union_t

  Union with documented fields

  This union demonstrates different data representations.

__C declaration:__ @data_union_t@

__defined at:__ @doxygen_docs.h:281:9@

__exported by:__ @doxygen_docs.h@
-}
newtype Data_union_t = Data_union_t
  { un_Data_union_t :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Data_union_t

{-|

  Integer representation

  __See:__ 'set_data_union_t_as_int'

__C declaration:__ @as_int@

__defined at:__ @doxygen_docs.h:282:13@

__exported by:__ @doxygen_docs.h@
-}
get_data_union_t_as_int :: Data_union_t -> HsBindgen.Runtime.Prelude.Int32
get_data_union_t_as_int =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_int'

-}
set_data_union_t_as_int :: HsBindgen.Runtime.Prelude.Int32 -> Data_union_t
set_data_union_t_as_int =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  Float representation

  __See:__ 'set_data_union_t_as_float'

__C declaration:__ @as_float@

__defined at:__ @doxygen_docs.h:283:11@

__exported by:__ @doxygen_docs.h@
-}
get_data_union_t_as_float :: Data_union_t -> FC.CFloat
get_data_union_t_as_float =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_float'

-}
set_data_union_t_as_float :: FC.CFloat -> Data_union_t
set_data_union_t_as_float =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  Byte array representation

  __See:__ 'set_data_union_t_as_bytes'

__C declaration:__ @as_bytes@

__defined at:__ @doxygen_docs.h:284:13@

__exported by:__ @doxygen_docs.h@
-}
get_data_union_t_as_bytes :: Data_union_t -> (HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8
get_data_union_t_as_bytes =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_bytes'

-}
set_data_union_t_as_bytes :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8) -> Data_union_t
set_data_union_t_as_bytes =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| As Parts Struct

  __See:__ 'set_data_union_t_as_parts'

__C declaration:__ @as_parts@

__defined at:__ @doxygen_docs.h:293:30@

__exported by:__ @doxygen_docs.h@
-}
get_data_union_t_as_parts :: Data_union_t -> Data_union_t_as_parts
get_data_union_t_as_parts =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_parts'

-}
set_data_union_t_as_parts :: Data_union_t_as_parts -> Data_union_t
set_data_union_t_as_parts =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  > bitfield_t

  Bit field structure

  Demonstrates bit field documentation.

__C declaration:__ @bitfield_t@

__defined at:__ @doxygen_docs.h:302:9@

__exported by:__ @doxygen_docs.h@
-}
data Bitfield_t = Bitfield_t
  { bitfield_t_flag1 :: FC.CUInt
    {- ^

       First flag (1 bit)

    __C declaration:__ @flag1@

    __defined at:__ @doxygen_docs.h:303:14@

    __exported by:__ @doxygen_docs.h@
    -}
  , bitfield_t_flag2 :: FC.CUInt
    {- ^

       Second flag (1 bit)

    __C declaration:__ @flag2@

    __defined at:__ @doxygen_docs.h:304:14@

    __exported by:__ @doxygen_docs.h@
    -}
  , bitfield_t_counter :: FC.CUInt
    {- ^

       Counter value (6 bits)

    __C declaration:__ @counter@

    __defined at:__ @doxygen_docs.h:305:14@

    __exported by:__ @doxygen_docs.h@
    -}
  , bitfield_t_reserved :: FC.CUInt
    {- ^

       Reserved bits (24 bits)

    __C declaration:__ @reserved@

    __defined at:__ @doxygen_docs.h:306:14@

    __exported by:__ @doxygen_docs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bitfield_t where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bitfield_t
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (0 :: Int) (1 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (1 :: Int) (1 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (2 :: Int) (6 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (8 :: Int) (24 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bitfield_t
            bitfield_t_flag12
            bitfield_t_flag23
            bitfield_t_counter4
            bitfield_t_reserved5 ->
                 HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (0 :: Int) (1 :: Int) bitfield_t_flag12
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (1 :: Int) (1 :: Int) bitfield_t_flag23
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (2 :: Int) (6 :: Int) bitfield_t_counter4
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (8 :: Int) (24 :: Int) bitfield_t_reserved5

{-|

  > processor_fn_t

  Function pointer typedef

  [__@input@ /(input)/__]: Input value

  [__@context@ /(input)/__]: Context pointer

  __returns:__ Processed value

__C declaration:__ @processor_fn_t@

__defined at:__ @doxygen_docs.h:317:15@

__exported by:__ @doxygen_docs.h@
-}
newtype Processor_fn_t = Processor_fn_t
  { un_Processor_fn_t :: Ptr.FunPtr (FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  > filename_t

  Array typedef with size

__C declaration:__ @filename_t@

__defined at:__ @doxygen_docs.h:323:14@

__exported by:__ @doxygen_docs.h@
-}
newtype Filename_t = Filename_t
  { un_Filename_t :: (HsBindgen.Runtime.ConstantArray.ConstantArray 256) FC.CChar
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-|

  Static array parameter

  [__@buffer@ /(input)/__]: Buffer with minimum size

  [__@size@ /(input)/__]: Actual buffer size

  __returns:__ Number of bytes written

__C declaration:__ @process_buffer@

__defined at:__ @doxygen_docs.h:332:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_1ce818d0ed47ea3e" process_buffer_wrapper
  :: Ptr.Ptr FC.CChar
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

process_buffer :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt
process_buffer =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr2 ->
                                                    process_buffer_wrapper ptr2 x1)

{-|

  Static array parameter

  [__@buffer@ /(input)/__]: Buffer with minimum size

  [__@size@ /(input)/__]: Actual buffer size

  __returns:__ Number of bytes written

__C declaration:__ @process_buffer@

__defined at:__ @doxygen_docs.h:332:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_fdf2d8f3abafc975" hs_bindgen_test_doxygen_docs_fdf2d8f3abafc975
  :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt))

{-# NOINLINE process_buffer_ptr #-}

process_buffer_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt)
process_buffer_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_fdf2d8f3abafc975

{-|

  Function with restrict pointers

  [__@dest@ /(input)/__]: Destination buffer (restrict)

  [__@src@ /(input)/__]: Source buffer (restrict)

  [__@n@ /(input)/__]: Number of bytes

  __returns:__ Destination pointer

__C declaration:__ @my_memcpy@

__defined at:__ @doxygen_docs.h:342:7@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_bcbe640b60445a4f" my_memcpy
  :: Ptr.Ptr Void
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

  Function with restrict pointers

  [__@dest@ /(input)/__]: Destination buffer (restrict)

  [__@src@ /(input)/__]: Source buffer (restrict)

  [__@n@ /(input)/__]: Number of bytes

  __returns:__ Destination pointer

__C declaration:__ @my_memcpy@

__defined at:__ @doxygen_docs.h:342:7@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_de9d3228e8bac25c" hs_bindgen_test_doxygen_docs_de9d3228e8bac25c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memcpy_ptr #-}

my_memcpy_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (Ptr.Ptr Void))
my_memcpy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_de9d3228e8bac25c

{-|

  Inline function

  [__@x@ /(input)/__]: Input value

  __returns:__ Doubled value

__C declaration:__ @double_value@

__defined at:__ @doxygen_docs.h:350:19@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_1c952f01cc07bb64" double_value
  :: FC.CInt
     {- ^

        [__@x@ /(input)/__]: Input value

     __C declaration:__ @x@
     -}
  -> IO FC.CInt

{-|

  Inline function

  [__@x@ /(input)/__]: Input value

  __returns:__ Doubled value

__C declaration:__ @double_value@

__defined at:__ @doxygen_docs.h:350:19@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_faec60e7f49d446c" hs_bindgen_test_doxygen_docs_faec60e7f49d446c
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE double_value_ptr #-}

double_value_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
double_value_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_faec60e7f49d446c

{-|

  Function with flexible array member

  [__@count@ /(input)/__]: Number of elements

  __returns:__ Allocated structure

__C declaration:__ @flexible_array@

__defined at:__ @doxygen_docs.h:360:8@

__exported by:__ @doxygen_docs.h@
-}
data Flexible_array = Flexible_array
  { flexible_array_count :: HsBindgen.Runtime.Prelude.CSize
    {- ^

       Number of elements

    __C declaration:__ @count@

    __defined at:__ @doxygen_docs.h:361:12@

    __exported by:__ @doxygen_docs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Flexible_array where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Flexible_array
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Flexible_array flexible_array_count2 ->
            F.pokeByteOff ptr0 (0 :: Int) flexible_array_count2

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember FC.CInt Flexible_array where

  flexibleArrayMemberOffset = \_ty0 -> 8

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

__defined at:__ @doxygen_docs.h:423:15@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_a9d86cb54c7498fe" complex_function
  :: Ptr.Ptr Config_t
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

__defined at:__ @doxygen_docs.h:423:15@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_b7ae5186dd939781" hs_bindgen_test_doxygen_docs_b7ae5186dd939781
  :: IO (Ptr.FunPtr ((Ptr.Ptr Config_t) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> IO Status_code_t))

{-# NOINLINE complex_function_ptr #-}

complex_function_ptr :: Ptr.FunPtr ((Ptr.Ptr Config_t) -> (Ptr.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> IO Status_code_t)
complex_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_b7ae5186dd939781

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @doxygen_docs.h:427:5@

__exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_71214e4420f53a0e" hash
  :: Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @s@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @hash@

    __defined at:__ @doxygen_docs.h:427:5@

    __exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_241bedb74b8016f3" hs_bindgen_test_doxygen_docs_241bedb74b8016f3
  :: IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE hash_ptr #-}

hash_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
hash_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_241bedb74b8016f3

{-| __C declaration:__ @square@

    __defined at:__ @doxygen_docs.h:429:5@

    __exported by:__ @doxygen_docs.h@
-}
foreign import ccall safe "hs_bindgen_test_doxygen_docs_8effe939268709e4" square
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt

{-| __C declaration:__ @square@

    __defined at:__ @doxygen_docs.h:429:5@

    __exported by:__ @doxygen_docs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_631c7b52d4d4fe3a" hs_bindgen_test_doxygen_docs_631c7b52d4d4fe3a
  :: IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_631c7b52d4d4fe3a
