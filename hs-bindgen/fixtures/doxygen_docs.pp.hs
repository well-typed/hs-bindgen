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

$(CAPI.addCSource "#include \"doxygen_docs.h\"\n__attribute__ ((const)) signed int *get_global_counter_ptr (void) { return &global_counter; } \n__attribute__ ((const)) char **get_version_string_ptr (void) { return &version_string; } \nsigned int testmodule_process_data (uint8_t *arg1, uint8_t *arg2, size_t *arg3) { return process_data(arg1, arg2, arg3); }\n_Bool testmodule_process_file (char *arg1) { return process_file(arg1); }\nsigned int testmodule_calculate_value (signed int arg1, signed int arg2) { return calculate_value(arg1, arg2); }\n_Bool testmodule_html_example (signed int arg1) { return html_example(arg1); }\n_Bool testmodule_list_example (char **arg1, size_t arg2) { return list_example(arg1, arg2); }\nvoid *testmodule_dangerous_function (void *arg1) { return dangerous_function(arg1); }\nsigned int testmodule_detailed_return_codes (char *arg1) { return detailed_return_codes(arg1); }\nsigned int testmodule_old_function (signed int arg1) { return old_function(arg1); }\nsigned int testmodule_versioned_function (signed int arg1) { return versioned_function(arg1); }\nsigned int testmodule_process_buffer (char *arg1, size_t arg2) { return process_buffer(arg1, arg2); }\nvoid *testmodule_my_memcpy (void *arg1, void *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }\nsigned int testmodule_double_value (signed int arg1) { return double_value(arg1); }\nstatus_code_t testmodule_complex_function (config_t *arg1, uint8_t *arg2, size_t arg3) { return complex_function(arg1, arg2, arg3); }\nsigned int testmodule_hash (char *arg1) { return hash(arg1); }\nsigned int testmodule_square (signed int arg1) { return square(arg1); }\n")

mAX_NAME_LENGTH :: FC.CInt
mAX_NAME_LENGTH = (64 :: FC.CInt)

{-| This is the comment __title__

  > size_type

  Size type for this library

  __from C:__ @size_type@
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

  __from C:__ @global_counter@
-}
foreign import ccall safe "get_global_counter_ptr" global_counter :: F.Ptr FC.CInt

{-|

  > extern const char* version_string

  Version string constant

  __from C:__ @version_string@
-}
foreign import ccall safe "get_version_string_ptr" version_string :: F.Ptr (F.Ptr FC.CChar)

{-| This is the comment @title@

  Forward declaration with documentation

  __from C:__ @forward_declared_struct@
-}
data Forward_declared_struct

{-|

  Forward declaration of union

  __from C:__ @forward_declared_union@
-}
data Forward_declared_union

{-|

  > color_enum

  Color enumeration without typedef

  __from C:__ @color_enum@
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

{-| Red color

  __from C:__ @COLOR_RED@
-}
pattern COLOR_RED :: Color_enum
pattern COLOR_RED = Color_enum 0

{-| Green color

  __from C:__ @COLOR_GREEN@
-}
pattern COLOR_GREEN :: Color_enum
pattern COLOR_GREEN = Color_enum 1

{-| Blue color

  __from C:__ @COLOR_BLUE@
-}
pattern COLOR_BLUE :: Color_enum
pattern COLOR_BLUE = Color_enum 2

{-|

  Function with detailed parameter documentation

  This function shows different parameter directions and types.

  [__@input_data@ /(input)/__]: Input data buffer

  [__@output_data@ /(output)/__]: Output data buffer

  [__@size@ /(input,output)/__]: Size of data, updated on return

  __returns:__ Status code (0 = success, -1 = error)

  __from C:__ @process_data(const uint8_t *, uint8_t *, size_t *)@
-}
foreign import ccall safe "testmodule_process_data" process_data :: (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> (F.Ptr HsBindgen.Runtime.Prelude.CSize) -> IO FC.CInt

{-|

  Function with inline commands and formatting

  This function uses @inline@ @code@ formatting and __bold__ text. It also demonstrates /emphasized/ text.

  [__@filename@ /(input)/__]: The @char*@ filename to process

  __returns:__ @true@ if successful, @false@ otherwise

  __from C:__ @process_file(const char *)@
-}
foreign import ccall safe "testmodule_process_file" process_file :: (F.Ptr FC.CChar) -> IO FC.CBool

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

  __from C:__ @calculate_value(int, int)@
-}
foreign import ccall safe "testmodule_calculate_value" calculate_value :: FC.CInt -> FC.CInt -> IO FC.CInt

{-|

  Function with HTML formatting

  This function demonstrates HTML bold and italic text. It also shows HTML code formatting.

  Input Output 0 false 1 true

  [__@value@ /(input)/__]: Input value

  __returns:__ Boolean result

  __from C:__ @html_example(int)@
-}
foreign import ccall safe "testmodule_html_example" html_example :: FC.CInt -> IO FC.CBool

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

  __from C:__ @list_example(const char **, size_t)@
-}
foreign import ccall safe "testmodule_list_example" list_example :: (F.Ptr (F.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool

{-|

  Function with warnings and notes

  __/WARNING:/__ This function may cause side effects

  __Note:__ Use with caution in multithreaded environments

  __see:__ related_function() for similar functionality

  [__@ptr@ /(input)/__]: Pointer to data

  __returns:__ Modified pointer

  __from C:__ @dangerous_function(void *)@
-}
foreign import ccall safe "testmodule_dangerous_function" dangerous_function :: (F.Ptr Void) -> IO (F.Ptr Void)

{-|

  Function with return value details

  [__@input@ /(input)/__]: Input string

  __returns:__ 0 Success

  __returns:__ -1 Invalid input

  __returns:__ -2 Memory allocation failed

  __returns:__ -3 Processing error

  __from C:__ @detailed_return_codes(const char *)@
-}
foreign import ccall safe "testmodule_detailed_return_codes" detailed_return_codes :: (F.Ptr FC.CChar) -> IO FC.CInt

{-|

  Function with deprecated annotation

  __deprecated:__ Use new_function() instead

  [__@old_param@ /(input)/__]: Legacy parameter

  __returns:__ Legacy result

  __from C:__ @old_function(int)@
-}
foreign import ccall safe "testmodule_old_function" old_function :: FC.CInt -> IO FC.CInt

{-|

  Function with version information

  @since:  1.0

  [__@data@ /(input)/__]: Input data

  __returns:__ Processed data

  __from C:__ @versioned_function(int)@
-}
foreign import ccall safe "testmodule_versioned_function" versioned_function :: FC.CInt -> IO FC.CInt

{-|

  Callback function type

  [__@event_type@ /(input)/__]: Type of event

  [__@user_data@ /(input)/__]: User-provided data

  __returns:__ Handling result

  __from C:__ @event_callback_t@
-}
newtype Event_callback_t = Event_callback_t
  { un_Event_callback_t :: F.FunPtr (FC.CInt -> (F.Ptr Void) -> IO FC.CInt)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  Structure with documented fields

  This structure demonstrates field documentation.

  __from C:__ @config_t@
-}
data Config_t = Config_t
  { config_t_id :: HsBindgen.Runtime.Prelude.Word32
    {- ^

       Unique identifier

       __from C:__ @id@
    -}
  , config_t_name :: (HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar
    {- ^

       Human-readable name

       __from C:__ @name@
    -}
  , config_t_flags :: HsBindgen.Runtime.Prelude.Word32
    {- ^

       Configuration flags

       __from C:__ @flags@
    -}
  , config_t_callback :: Event_callback_t
    {- ^

       Optional callback function

       __from C:__ @callback@
    -}
  , config_t_user_data :: F.Ptr Void
    {- ^

       User data for callback

       __from C:__ @user_data@
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

  __from C:__ @status_code_t@
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

  __from C:__ @STATUS_OK@
-}
pattern STATUS_OK :: Status_code_t
pattern STATUS_OK = Status_code_t 0

{-|

  Invalid parameter provided

  __from C:__ @STATUS_INVALID_PARAM@
-}
pattern STATUS_INVALID_PARAM :: Status_code_t
pattern STATUS_INVALID_PARAM = Status_code_t (-1)

{-|

  Memory allocation failed

  __from C:__ @STATUS_NO_MEMORY@
-}
pattern STATUS_NO_MEMORY :: Status_code_t
pattern STATUS_NO_MEMORY = Status_code_t (-2)

{-|

  Operation timed out

  __from C:__ @STATUS_TIMEOUT@
-}
pattern STATUS_TIMEOUT :: Status_code_t
pattern STATUS_TIMEOUT = Status_code_t (-3)

{-|

  Generic error

  __from C:__ @STATUS_ERROR@
-}
pattern STATUS_ERROR :: Status_code_t
pattern STATUS_ERROR = Status_code_t (-99)

{-|

  Structured representation

  Allows access to high and low parts separately

  __from C:__ @struct (unnamed at doxygen_docs.h:286:5)@
-}
data Data_union_t_as_parts = Data_union_t_as_parts
  { data_union_t_as_parts_low :: HsBindgen.Runtime.Prelude.Word16
    {- ^

       Low 16 bits

       __from C:__ @low@
    -}
  , data_union_t_as_parts_high :: HsBindgen.Runtime.Prelude.Word16
    {- ^

       High 16 bits

       __from C:__ @high@
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

  __from C:__ @data_union_t@
-}
newtype Data_union_t = Data_union_t
  { un_Data_union_t :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Data_union_t

{-|

  Integer representation

  __See:__ 'set_data_union_t_as_int'

  __from C:__ @as_int@
-}
get_data_union_t_as_int :: Data_union_t -> HsBindgen.Runtime.Prelude.Int32
get_data_union_t_as_int =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_data_union_t_as_int :: HsBindgen.Runtime.Prelude.Int32 -> Data_union_t
set_data_union_t_as_int =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  Float representation

  __See:__ 'set_data_union_t_as_float'

  __from C:__ @as_float@
-}
get_data_union_t_as_float :: Data_union_t -> FC.CFloat
get_data_union_t_as_float =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_data_union_t_as_float :: FC.CFloat -> Data_union_t
set_data_union_t_as_float =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  Byte array representation

  __See:__ 'set_data_union_t_as_bytes'

  __from C:__ @as_bytes@
-}
get_data_union_t_as_bytes :: Data_union_t -> (HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8
get_data_union_t_as_bytes =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_data_union_t_as_bytes :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8) -> Data_union_t
set_data_union_t_as_bytes =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| As Parts Struct

  __See:__ 'set_data_union_t_as_parts'

  __from C:__ @as_parts@
-}
get_data_union_t_as_parts :: Data_union_t -> Data_union_t_as_parts
get_data_union_t_as_parts =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_data_union_t_as_parts :: Data_union_t_as_parts -> Data_union_t
set_data_union_t_as_parts =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  > bitfield_t

  Bit field structure

  Demonstrates bit field documentation.

  __from C:__ @bitfield_t@
-}
data Bitfield_t = Bitfield_t
  { bitfield_t_flag1 :: FC.CUInt
    {- ^

       First flag (1 bit)

       __from C:__ @flag1@
    -}
  , bitfield_t_flag2 :: FC.CUInt
    {- ^

       Second flag (1 bit)

       __from C:__ @flag2@
    -}
  , bitfield_t_counter :: FC.CUInt
    {- ^

       Counter value (6 bits)

       __from C:__ @counter@
    -}
  , bitfield_t_reserved :: FC.CUInt
    {- ^

       Reserved bits (24 bits)

       __from C:__ @reserved@
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

  __from C:__ @processor_fn_t@
-}
newtype Processor_fn_t = Processor_fn_t
  { un_Processor_fn_t :: F.FunPtr (FC.CInt -> (F.Ptr Void) -> IO FC.CInt)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-|

  > filename_t

  Array typedef with size

  __from C:__ @filename_t@
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

  __from C:__ @process_buffer(char *, size_t)@
-}
foreign import ccall safe "testmodule_process_buffer" process_buffer_wrapper :: (F.Ptr FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt

process_buffer :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CInt
process_buffer =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr2 ->
                                                    process_buffer_wrapper ptr2 x1)

{-|

  Function with restrict pointers

  [__@dest@ /(input)/__]: Destination buffer (restrict)

  [__@src@ /(input)/__]: Source buffer (restrict)

  [__@n@ /(input)/__]: Number of bytes

  __returns:__ Destination pointer

  __from C:__ @my_memcpy(void *restrict, const void *restrict, size_t)@
-}
foreign import ccall safe "testmodule_my_memcpy" my_memcpy :: (F.Ptr Void) -> (F.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO (F.Ptr Void)

{-|

  Inline function

  [__@x@ /(input)/__]: Input value

  __returns:__ Doubled value

  __from C:__ @double_value(int)@
-}
foreign import ccall safe "testmodule_double_value" double_value :: FC.CInt -> IO FC.CInt

{-|

  Function with flexible array member

  [__@count@ /(input)/__]: Number of elements

  __returns:__ Allocated structure

  __from C:__ @flexible_array@
-}
data Flexible_array = Flexible_array
  { flexible_array_count :: HsBindgen.Runtime.Prelude.CSize
    {- ^

       Number of elements

       __from C:__ @count@
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

  [__@config@ /(input)/__]: Configuration structure

  [__@data@ /(input)/__]: Input data buffer

  [__@size@ /(input)/__]: Size of input data

  __returns:__ Status code indicating success or failure

  __pre condition:__ config must not be NULL

  __pre condition:__ data must not be NULL if size > 0

  __post condition:__ Output data is written to config->user_data

  __/WARNING:/__ May return NULL if memory allocation fails

  __/WARNING:/__ Sets errno to EINVAL if parameters are invalid

  __from C:__ @complex_function(config_t *, const uint8_t *, size_t)@
-}
foreign import ccall safe "testmodule_complex_function" complex_function :: (F.Ptr Config_t) -> (F.Ptr HsBindgen.Runtime.Prelude.Word8) -> HsBindgen.Runtime.Prelude.CSize -> IO Status_code_t

foreign import ccall safe "testmodule_hash" hash :: (F.Ptr FC.CChar) -> IO FC.CInt

foreign import ccall safe "testmodule_square" square :: FC.CInt -> FC.CInt
