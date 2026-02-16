{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @MAX_NAME_LENGTH@

    __defined at:__ @documentation\/doxygen_docs.h 39:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
mAX_NAME_LENGTH :: RIP.CInt
mAX_NAME_LENGTH = (64 :: RIP.CInt)

{-| This is the comment __title__

  > size_type

  Size type for this library

__C declaration:__ @size_type@

__defined at:__ @documentation\/doxygen_docs.h 54:16@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Size_type = Size_type
  { unwrapSize_type :: HsBindgen.Runtime.LibC.CSize
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "unwrapSize_type" (RIP.Ptr Size_type) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSize_type")

instance HasCField.HasCField Size_type "unwrapSize_type" where

  type CFieldType Size_type "unwrapSize_type" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 0

{-| This is the comment @title@

  Forward declaration with documentation

__C declaration:__ @struct forward_declared_struct@

__defined at:__ @documentation\/doxygen_docs.h 72:8@

__exported by:__ @documentation\/doxygen_docs.h@
-}
data Forward_declared_struct

{-|

  Forward declaration of union

__C declaration:__ @union forward_declared_union@

__defined at:__ @documentation\/doxygen_docs.h 77:7@

__exported by:__ @documentation\/doxygen_docs.h@
-}
data Forward_declared_union

{-|

  > color_enum

  Color enumeration without typedef

__C declaration:__ @enum color_enum@

__defined at:__ @documentation\/doxygen_docs.h 83:6@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Color_enum = Color_enum
  { unwrapColor_enum :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Color_enum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Color_enum where

  readRaw =
    \ptr0 ->
          pure Color_enum
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Color_enum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Color_enum unwrapColor_enum2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapColor_enum2

deriving via Marshal.EquivStorable Color_enum instance RIP.Storable Color_enum

deriving via RIP.CUInt instance RIP.Prim Color_enum

instance CEnum.CEnum Color_enum where

  type CEnumZ Color_enum = RIP.CUInt

  toCEnum = Color_enum

  fromCEnum = unwrapColor_enum

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (0, RIP.singleton "COLOR_RED")
                                   , (1, RIP.singleton "COLOR_GREEN")
                                   , (2, RIP.singleton "COLOR_BLUE")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Color_enum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Color_enum"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Color_enum where

  minDeclaredValue = COLOR_RED

  maxDeclaredValue = COLOR_BLUE

instance Show Color_enum where

  showsPrec = CEnum.shows

instance Read Color_enum where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapColor_enum" (RIP.Ptr Color_enum) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapColor_enum")

instance HasCField.HasCField Color_enum "unwrapColor_enum" where

  type CFieldType Color_enum "unwrapColor_enum" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| Red color

__C declaration:__ @COLOR_RED@

__defined at:__ @documentation\/doxygen_docs.h 84:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern COLOR_RED :: Color_enum
pattern COLOR_RED = Color_enum 0

{-| Green color

__C declaration:__ @COLOR_GREEN@

__defined at:__ @documentation\/doxygen_docs.h 85:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern COLOR_GREEN :: Color_enum
pattern COLOR_GREEN = Color_enum 1

{-| Blue color

__C declaration:__ @COLOR_BLUE@

__defined at:__ @documentation\/doxygen_docs.h 86:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern COLOR_BLUE :: Color_enum
pattern COLOR_BLUE = Color_enum 2

{-| Auxiliary type used by 'Event_callback_t'

__C declaration:__ @event_callback_t@

__defined at:__ @documentation\/doxygen_docs.h 225:15@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Event_callback_t_Aux = Event_callback_t_Aux
  { unwrapEvent_callback_t_Aux :: RIP.CInt -> (RIP.Ptr RIP.Void) -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_111918b0aee2a7fb_base ::
     (RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32))

-- __unique:__ @toEvent_callback_t_Aux@
hs_bindgen_111918b0aee2a7fb ::
     Event_callback_t_Aux
  -> IO (RIP.FunPtr Event_callback_t_Aux)
hs_bindgen_111918b0aee2a7fb =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_111918b0aee2a7fb_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_9e9d478c2d75628c_base ::
     RIP.FunPtr (RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32

-- __unique:__ @fromEvent_callback_t_Aux@
hs_bindgen_9e9d478c2d75628c ::
     RIP.FunPtr Event_callback_t_Aux
  -> Event_callback_t_Aux
hs_bindgen_9e9d478c2d75628c =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_9e9d478c2d75628c_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Event_callback_t_Aux where

  toFunPtr = hs_bindgen_111918b0aee2a7fb

instance RIP.FromFunPtr Event_callback_t_Aux where

  fromFunPtr = hs_bindgen_9e9d478c2d75628c

instance ( ((~) ty) (RIP.CInt -> (RIP.Ptr RIP.Void) -> IO RIP.CInt)
         ) => RIP.HasField "unwrapEvent_callback_t_Aux" (RIP.Ptr Event_callback_t_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEvent_callback_t_Aux")

instance HasCField.HasCField Event_callback_t_Aux "unwrapEvent_callback_t_Aux" where

  type CFieldType Event_callback_t_Aux "unwrapEvent_callback_t_Aux" =
    RIP.CInt -> (RIP.Ptr RIP.Void) -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-|

  Callback function type

  [__@event_type@ /(input)/__]: Type of event

  [__@user_data@ /(input)/__]: User-provided data

  __returns:__ Handling result

__C declaration:__ @event_callback_t@

__defined at:__ @documentation\/doxygen_docs.h 225:15@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Event_callback_t = Event_callback_t
  { unwrapEvent_callback_t :: RIP.FunPtr Event_callback_t_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Event_callback_t_Aux)
         ) => RIP.HasField "unwrapEvent_callback_t" (RIP.Ptr Event_callback_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEvent_callback_t")

instance HasCField.HasCField Event_callback_t "unwrapEvent_callback_t" where

  type CFieldType Event_callback_t "unwrapEvent_callback_t" =
    RIP.FunPtr Event_callback_t_Aux

  offset# = \_ -> \_ -> 0

{-|

  Structure with documented fields

  This structure demonstrates field documentation.

__C declaration:__ @struct config_t@

__defined at:__ @documentation\/doxygen_docs.h 232:9@

__exported by:__ @documentation\/doxygen_docs.h@
-}
data Config_t = Config_t
  { config_t_id :: HsBindgen.Runtime.LibC.Word32
    {- ^

       Unique identifier

    __C declaration:__ @id@

    __defined at:__ @documentation\/doxygen_docs.h 234:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_name :: (CA.ConstantArray 64) RIP.CChar
    {- ^

       Human-readable name

    __C declaration:__ @name@

    __defined at:__ @documentation\/doxygen_docs.h 237:10@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_flags :: HsBindgen.Runtime.LibC.Word32
    {- ^

       Configuration flags

    __C declaration:__ @flags@

    __defined at:__ @documentation\/doxygen_docs.h 240:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_callback :: Event_callback_t
    {- ^

       Optional callback function

       See also: 'Event_callback_t'

    __C declaration:__ @callback@

    __defined at:__ @documentation\/doxygen_docs.h 247:22@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_user_data :: RIP.Ptr RIP.Void
    {- ^

       User data for callback

    __C declaration:__ @user_data@

    __defined at:__ @documentation\/doxygen_docs.h 250:11@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Config_t where

  staticSizeOf = \_ -> (88 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Config_t where

  readRaw =
    \ptr0 ->
          pure Config_t
      <*> HasCField.readRaw (RIP.Proxy @"config_t_id") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"config_t_name") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"config_t_flags") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"config_t_callback") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"config_t_user_data") ptr0

instance Marshal.WriteRaw Config_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config_t
            config_t_id2
            config_t_name3
            config_t_flags4
            config_t_callback5
            config_t_user_data6 ->
                 HasCField.writeRaw (RIP.Proxy @"config_t_id") ptr0 config_t_id2
              >> HasCField.writeRaw (RIP.Proxy @"config_t_name") ptr0 config_t_name3
              >> HasCField.writeRaw (RIP.Proxy @"config_t_flags") ptr0 config_t_flags4
              >> HasCField.writeRaw (RIP.Proxy @"config_t_callback") ptr0 config_t_callback5
              >> HasCField.writeRaw (RIP.Proxy @"config_t_user_data") ptr0 config_t_user_data6

deriving via Marshal.EquivStorable Config_t instance RIP.Storable Config_t

instance HasCField.HasCField Config_t "config_t_id" where

  type CFieldType Config_t "config_t_id" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word32
         ) => RIP.HasField "config_t_id" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_id")

instance HasCField.HasCField Config_t "config_t_name" where

  type CFieldType Config_t "config_t_name" =
    (CA.ConstantArray 64) RIP.CChar

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) ((CA.ConstantArray 64) RIP.CChar)
         ) => RIP.HasField "config_t_name" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_name")

instance HasCField.HasCField Config_t "config_t_flags" where

  type CFieldType Config_t "config_t_flags" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 68

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word32
         ) => RIP.HasField "config_t_flags" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_flags")

instance HasCField.HasCField Config_t "config_t_callback" where

  type CFieldType Config_t "config_t_callback" =
    Event_callback_t

  offset# = \_ -> \_ -> 72

instance ( ((~) ty) Event_callback_t
         ) => RIP.HasField "config_t_callback" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_callback")

instance HasCField.HasCField Config_t "config_t_user_data" where

  type CFieldType Config_t "config_t_user_data" =
    RIP.Ptr RIP.Void

  offset# = \_ -> \_ -> 80

instance ( ((~) ty) (RIP.Ptr RIP.Void)
         ) => RIP.HasField "config_t_user_data" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_user_data")

{-|

  Enumeration with documented values

  This enum shows different status codes.

__C declaration:__ @enum status_code_t@

__defined at:__ @documentation\/doxygen_docs.h 258:9@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Status_code_t = Status_code_t
  { unwrapStatus_code_t :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Status_code_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Status_code_t where

  readRaw =
    \ptr0 ->
          pure Status_code_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Status_code_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Status_code_t unwrapStatus_code_t2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapStatus_code_t2

deriving via Marshal.EquivStorable Status_code_t instance RIP.Storable Status_code_t

deriving via RIP.CInt instance RIP.Prim Status_code_t

instance CEnum.CEnum Status_code_t where

  type CEnumZ Status_code_t = RIP.CInt

  toCEnum = Status_code_t

  fromCEnum = unwrapStatus_code_t

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [ (-99, RIP.singleton "STATUS_ERROR")
                                   , (-3, RIP.singleton "STATUS_TIMEOUT")
                                   , (-2, RIP.singleton "STATUS_NO_MEMORY")
                                   , (-1, RIP.singleton "STATUS_INVALID_PARAM")
                                   , (0, RIP.singleton "STATUS_OK")
                                   ]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Status_code_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Status_code_t"

instance Show Status_code_t where

  showsPrec = CEnum.shows

instance Read Status_code_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapStatus_code_t" (RIP.Ptr Status_code_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStatus_code_t")

instance HasCField.HasCField Status_code_t "unwrapStatus_code_t" where

  type CFieldType Status_code_t "unwrapStatus_code_t" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-|

  Operation successful

__C declaration:__ @STATUS_OK@

__defined at:__ @documentation\/doxygen_docs.h 260:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_OK :: Status_code_t
pattern STATUS_OK = Status_code_t 0

{-|

  Invalid parameter provided

__C declaration:__ @STATUS_INVALID_PARAM@

__defined at:__ @documentation\/doxygen_docs.h 263:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_INVALID_PARAM :: Status_code_t
pattern STATUS_INVALID_PARAM = Status_code_t (-1)

{-|

  Memory allocation failed

__C declaration:__ @STATUS_NO_MEMORY@

__defined at:__ @documentation\/doxygen_docs.h 266:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_NO_MEMORY :: Status_code_t
pattern STATUS_NO_MEMORY = Status_code_t (-2)

{-|

  Operation timed out

__C declaration:__ @STATUS_TIMEOUT@

__defined at:__ @documentation\/doxygen_docs.h 269:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_TIMEOUT :: Status_code_t
pattern STATUS_TIMEOUT = Status_code_t (-3)

{-|

  Generic error

__C declaration:__ @STATUS_ERROR@

__defined at:__ @documentation\/doxygen_docs.h 272:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_ERROR :: Status_code_t
pattern STATUS_ERROR = Status_code_t (-99)

{-|

  Structured representation

  Allows access to high and low parts separately

__C declaration:__ @struct \@data_union_t_as_parts@

__defined at:__ @documentation\/doxygen_docs.h 290:5@

__exported by:__ @documentation\/doxygen_docs.h@
-}
data Data_union_t_as_parts = Data_union_t_as_parts
  { data_union_t_as_parts_low :: HsBindgen.Runtime.LibC.Word16
    {- ^

       Low 16 bits

    __C declaration:__ @low@

    __defined at:__ @documentation\/doxygen_docs.h 291:18@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , data_union_t_as_parts_high :: HsBindgen.Runtime.LibC.Word16
    {- ^

       High 16 bits

    __C declaration:__ @high@

    __defined at:__ @documentation\/doxygen_docs.h 292:18@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Data_union_t_as_parts where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance Marshal.ReadRaw Data_union_t_as_parts where

  readRaw =
    \ptr0 ->
          pure Data_union_t_as_parts
      <*> HasCField.readRaw (RIP.Proxy @"data_union_t_as_parts_low") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"data_union_t_as_parts_high") ptr0

instance Marshal.WriteRaw Data_union_t_as_parts where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Data_union_t_as_parts data_union_t_as_parts_low2 data_union_t_as_parts_high3 ->
               HasCField.writeRaw (RIP.Proxy @"data_union_t_as_parts_low") ptr0 data_union_t_as_parts_low2
            >> HasCField.writeRaw (RIP.Proxy @"data_union_t_as_parts_high") ptr0 data_union_t_as_parts_high3

deriving via Marshal.EquivStorable Data_union_t_as_parts instance RIP.Storable Data_union_t_as_parts

instance HasCField.HasCField Data_union_t_as_parts "data_union_t_as_parts_low" where

  type CFieldType Data_union_t_as_parts "data_union_t_as_parts_low" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "data_union_t_as_parts_low" (RIP.Ptr Data_union_t_as_parts) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_parts_low")

instance HasCField.HasCField Data_union_t_as_parts "data_union_t_as_parts_high" where

  type CFieldType Data_union_t_as_parts "data_union_t_as_parts_high" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

instance ( ((~) ty) HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "data_union_t_as_parts_high" (RIP.Ptr Data_union_t_as_parts) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_parts_high")

{-|

  > data_union_t

  Union with documented fields

  This union demonstrates different data representations.

__C declaration:__ @union data_union_t@

__defined at:__ @documentation\/doxygen_docs.h 281:9@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Data_union_t = Data_union_t
  { unwrapData_union_t :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize Data_union_t

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw Data_union_t

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw Data_union_t

deriving via Marshal.EquivStorable Data_union_t instance RIP.Storable Data_union_t

{-|

  Integer representation

  __See:__ 'set_data_union_t_as_int'

__C declaration:__ @as_int@

__defined at:__ @documentation\/doxygen_docs.h 282:13@

__exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_int ::
     Data_union_t
  -> HsBindgen.Runtime.LibC.Int32
get_data_union_t_as_int = RIP.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_int'

-}
set_data_union_t_as_int ::
     HsBindgen.Runtime.LibC.Int32
  -> Data_union_t
set_data_union_t_as_int = RIP.setUnionPayload

{-|

  Float representation

  __See:__ 'set_data_union_t_as_float'

__C declaration:__ @as_float@

__defined at:__ @documentation\/doxygen_docs.h 283:11@

__exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_float ::
     Data_union_t
  -> RIP.CFloat
get_data_union_t_as_float = RIP.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_float'

-}
set_data_union_t_as_float ::
     RIP.CFloat
  -> Data_union_t
set_data_union_t_as_float = RIP.setUnionPayload

{-|

  Byte array representation

  __See:__ 'set_data_union_t_as_bytes'

__C declaration:__ @as_bytes@

__defined at:__ @documentation\/doxygen_docs.h 284:13@

__exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_bytes ::
     Data_union_t
  -> (CA.ConstantArray 4) HsBindgen.Runtime.LibC.Word8
get_data_union_t_as_bytes = RIP.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_bytes'

-}
set_data_union_t_as_bytes ::
     (CA.ConstantArray 4) HsBindgen.Runtime.LibC.Word8
  -> Data_union_t
set_data_union_t_as_bytes = RIP.setUnionPayload

{-| As Parts Struct

  __See:__ 'set_data_union_t_as_parts'

__C declaration:__ @as_parts@

__defined at:__ @documentation\/doxygen_docs.h 293:30@

__exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_parts ::
     Data_union_t
  -> Data_union_t_as_parts
get_data_union_t_as_parts = RIP.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_parts'

-}
set_data_union_t_as_parts ::
     Data_union_t_as_parts
  -> Data_union_t
set_data_union_t_as_parts = RIP.setUnionPayload

instance HasCField.HasCField Data_union_t "data_union_t_as_int" where

  type CFieldType Data_union_t "data_union_t_as_int" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) HsBindgen.Runtime.LibC.Int32
         ) => RIP.HasField "data_union_t_as_int" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_int")

instance HasCField.HasCField Data_union_t "data_union_t_as_float" where

  type CFieldType Data_union_t "data_union_t_as_float" =
    RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CFloat
         ) => RIP.HasField "data_union_t_as_float" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_float")

instance HasCField.HasCField Data_union_t "data_union_t_as_bytes" where

  type CFieldType Data_union_t "data_union_t_as_bytes" =
    (CA.ConstantArray 4) HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) ((CA.ConstantArray 4) HsBindgen.Runtime.LibC.Word8)
         ) => RIP.HasField "data_union_t_as_bytes" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_bytes")

instance HasCField.HasCField Data_union_t "data_union_t_as_parts" where

  type CFieldType Data_union_t "data_union_t_as_parts" =
    Data_union_t_as_parts

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Data_union_t_as_parts
         ) => RIP.HasField "data_union_t_as_parts" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_parts")

{-|

  > bitfield_t

  Bit field structure

  Demonstrates bit field documentation.

__C declaration:__ @struct bitfield_t@

__defined at:__ @documentation\/doxygen_docs.h 302:9@

__exported by:__ @documentation\/doxygen_docs.h@
-}
data Bitfield_t = Bitfield_t
  { bitfield_t_flag1 :: RIP.CUInt
    {- ^

       First flag (1 bit)

    __C declaration:__ @flag1@

    __defined at:__ @documentation\/doxygen_docs.h 303:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_flag2 :: RIP.CUInt
    {- ^

       Second flag (1 bit)

    __C declaration:__ @flag2@

    __defined at:__ @documentation\/doxygen_docs.h 304:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_counter :: RIP.CUInt
    {- ^

       Counter value (6 bits)

    __C declaration:__ @counter@

    __defined at:__ @documentation\/doxygen_docs.h 305:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_reserved :: RIP.CUInt
    {- ^

       Reserved bits (24 bits)

    __C declaration:__ @reserved@

    __defined at:__ @documentation\/doxygen_docs.h 306:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bitfield_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bitfield_t where

  readRaw =
    \ptr0 ->
          pure Bitfield_t
      <*> HasCBitfield.peek (RIP.Proxy @"bitfield_t_flag1") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bitfield_t_flag2") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bitfield_t_counter") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"bitfield_t_reserved") ptr0

instance Marshal.WriteRaw Bitfield_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bitfield_t
            bitfield_t_flag12
            bitfield_t_flag23
            bitfield_t_counter4
            bitfield_t_reserved5 ->
                 HasCBitfield.poke (RIP.Proxy @"bitfield_t_flag1") ptr0 bitfield_t_flag12
              >> HasCBitfield.poke (RIP.Proxy @"bitfield_t_flag2") ptr0 bitfield_t_flag23
              >> HasCBitfield.poke (RIP.Proxy @"bitfield_t_counter") ptr0 bitfield_t_counter4
              >> HasCBitfield.poke (RIP.Proxy @"bitfield_t_reserved") ptr0 bitfield_t_reserved5

deriving via Marshal.EquivStorable Bitfield_t instance RIP.Storable Bitfield_t

instance HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_flag1" where

  type CBitfieldType Bitfield_t "bitfield_t_flag1" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "bitfield_t_flag1" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_flag1")

instance HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_flag2" where

  type CBitfieldType Bitfield_t "bitfield_t_flag2" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 1

  bitfieldWidth# = \_ -> \_ -> 1

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "bitfield_t_flag2" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_flag2")

instance HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_counter" where

  type CBitfieldType Bitfield_t "bitfield_t_counter" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 6

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "bitfield_t_counter" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_counter")

instance HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_reserved" where

  type CBitfieldType Bitfield_t "bitfield_t_reserved" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 24

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "bitfield_t_reserved" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_reserved")

{-| Auxiliary type used by 'Processor_fn_t'

__C declaration:__ @processor_fn_t@

__defined at:__ @documentation\/doxygen_docs.h 317:15@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Processor_fn_t_Aux = Processor_fn_t_Aux
  { unwrapProcessor_fn_t_Aux :: RIP.CInt -> (RIP.Ptr RIP.Void) -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d4e16471c82d5df0_base ::
     (RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32))

-- __unique:__ @toProcessor_fn_t_Aux@
hs_bindgen_d4e16471c82d5df0 ::
     Processor_fn_t_Aux
  -> IO (RIP.FunPtr Processor_fn_t_Aux)
hs_bindgen_d4e16471c82d5df0 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_d4e16471c82d5df0_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0d4b3d0461629423_base ::
     RIP.FunPtr (RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> RIP.Int32 -> (RIP.Ptr RIP.Void) -> IO RIP.Int32

-- __unique:__ @fromProcessor_fn_t_Aux@
hs_bindgen_0d4b3d0461629423 ::
     RIP.FunPtr Processor_fn_t_Aux
  -> Processor_fn_t_Aux
hs_bindgen_0d4b3d0461629423 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_0d4b3d0461629423_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Processor_fn_t_Aux where

  toFunPtr = hs_bindgen_d4e16471c82d5df0

instance RIP.FromFunPtr Processor_fn_t_Aux where

  fromFunPtr = hs_bindgen_0d4b3d0461629423

instance ( ((~) ty) (RIP.CInt -> (RIP.Ptr RIP.Void) -> IO RIP.CInt)
         ) => RIP.HasField "unwrapProcessor_fn_t_Aux" (RIP.Ptr Processor_fn_t_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapProcessor_fn_t_Aux")

instance HasCField.HasCField Processor_fn_t_Aux "unwrapProcessor_fn_t_Aux" where

  type CFieldType Processor_fn_t_Aux "unwrapProcessor_fn_t_Aux" =
    RIP.CInt -> (RIP.Ptr RIP.Void) -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-|

  > processor_fn_t

  Function pointer typedef

  [__@input@ /(input)/__]: Input value

  [__@context@ /(input)/__]: Context pointer

  __returns:__ Processed value

__C declaration:__ @processor_fn_t@

__defined at:__ @documentation\/doxygen_docs.h 317:15@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Processor_fn_t = Processor_fn_t
  { unwrapProcessor_fn_t :: RIP.FunPtr Processor_fn_t_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Processor_fn_t_Aux)
         ) => RIP.HasField "unwrapProcessor_fn_t" (RIP.Ptr Processor_fn_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapProcessor_fn_t")

instance HasCField.HasCField Processor_fn_t "unwrapProcessor_fn_t" where

  type CFieldType Processor_fn_t "unwrapProcessor_fn_t" =
    RIP.FunPtr Processor_fn_t_Aux

  offset# = \_ -> \_ -> 0

{-|

  > filename_t

  Array typedef with size

__C declaration:__ @filename_t@

__defined at:__ @documentation\/doxygen_docs.h 323:14@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Filename_t = Filename_t
  { unwrapFilename_t :: (CA.ConstantArray 256) RIP.CChar
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 256) RIP.CChar)
         ) => RIP.HasField "unwrapFilename_t" (RIP.Ptr Filename_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFilename_t")

instance HasCField.HasCField Filename_t "unwrapFilename_t" where

  type CFieldType Filename_t "unwrapFilename_t" =
    (CA.ConstantArray 256) RIP.CChar

  offset# = \_ -> \_ -> 0

{-|

  Function with flexible array member

  [__@count@ /(input)/__]: Number of elements

  __returns:__ Allocated structure

__C declaration:__ @struct flexible_array@

__defined at:__ @documentation\/doxygen_docs.h 360:8@

__exported by:__ @documentation\/doxygen_docs.h@
-}
data Flexible_array_Aux = Flexible_array
  { flexible_array_count :: HsBindgen.Runtime.LibC.CSize
    {- ^

       Number of elements

    __C declaration:__ @count@

    __defined at:__ @documentation\/doxygen_docs.h 361:12@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Flexible_array_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Flexible_array_Aux where

  readRaw =
    \ptr0 ->
          pure Flexible_array
      <*> HasCField.readRaw (RIP.Proxy @"flexible_array_count") ptr0

instance Marshal.WriteRaw Flexible_array_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Flexible_array flexible_array_count2 ->
            HasCField.writeRaw (RIP.Proxy @"flexible_array_count") ptr0 flexible_array_count2

deriving via Marshal.EquivStorable Flexible_array_Aux instance RIP.Storable Flexible_array_Aux

instance HasCField.HasCField Flexible_array_Aux "flexible_array_count" where

  type CFieldType Flexible_array_Aux "flexible_array_count" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "flexible_array_count" (RIP.Ptr Flexible_array_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"flexible_array_count")

instance FLAM.Offset RIP.CInt Flexible_array_Aux where

  offset = \_ty0 -> 8

{-|

  Function with flexible array member

  [__@count@ /(input)/__]: Number of elements

  __returns:__ Allocated structure

__C declaration:__ @struct flexible_array@

__defined at:__ @documentation\/doxygen_docs.h 360:8@

__exported by:__ @documentation\/doxygen_docs.h@
-}
type Flexible_array =
  (FLAM.WithFlam RIP.CInt) Flexible_array_Aux
