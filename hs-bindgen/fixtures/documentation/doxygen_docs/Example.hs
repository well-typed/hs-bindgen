{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.BitfieldPtr
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FLAM
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasCBitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Prelude as P
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @MAX_NAME_LENGTH@

    __defined at:__ @documentation\/doxygen_docs.h 39:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
mAX_NAME_LENGTH :: FC.CInt
mAX_NAME_LENGTH = (64 :: FC.CInt)

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
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Size_type) "unwrapSize_type")
         ) => GHC.Records.HasField "unwrapSize_type" (Ptr.Ptr Size_type) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSize_type")

instance HsBindgen.Runtime.HasCField.HasCField Size_type "unwrapSize_type" where

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
  { unwrapColor_enum :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Color_enum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Color_enum where

  readRaw =
    \ptr0 ->
          pure Color_enum
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Color_enum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Color_enum unwrapColor_enum2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapColor_enum2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Color_enum instance F.Storable Color_enum

deriving via FC.CUInt instance Data.Primitive.Types.Prim Color_enum

instance HsBindgen.Runtime.CEnum.CEnum Color_enum where

  type CEnumZ Color_enum = FC.CUInt

  toCEnum = Color_enum

  fromCEnum = unwrapColor_enum

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

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Color_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Color_enum) "unwrapColor_enum")
         ) => GHC.Records.HasField "unwrapColor_enum" (Ptr.Ptr Color_enum) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapColor_enum")

instance HsBindgen.Runtime.HasCField.HasCField Color_enum "unwrapColor_enum" where

  type CFieldType Color_enum "unwrapColor_enum" =
    FC.CUInt

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
  { unwrapEvent_callback_t_Aux :: FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_111918b0aee2a7fb_base ::
     (GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32))

-- __unique:__ @toEvent_callback_t_Aux@
hs_bindgen_111918b0aee2a7fb ::
     Event_callback_t_Aux
  -> IO (Ptr.FunPtr Event_callback_t_Aux)
hs_bindgen_111918b0aee2a7fb =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_111918b0aee2a7fb_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_9e9d478c2d75628c_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32

-- __unique:__ @fromEvent_callback_t_Aux@
hs_bindgen_9e9d478c2d75628c ::
     Ptr.FunPtr Event_callback_t_Aux
  -> Event_callback_t_Aux
hs_bindgen_9e9d478c2d75628c =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_9e9d478c2d75628c_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Event_callback_t_Aux where

  toFunPtr = hs_bindgen_111918b0aee2a7fb

instance HsBindgen.Runtime.FunPtr.FromFunPtr Event_callback_t_Aux where

  fromFunPtr = hs_bindgen_9e9d478c2d75628c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Event_callback_t_Aux) "unwrapEvent_callback_t_Aux")
         ) => GHC.Records.HasField "unwrapEvent_callback_t_Aux" (Ptr.Ptr Event_callback_t_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapEvent_callback_t_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Event_callback_t_Aux "unwrapEvent_callback_t_Aux" where

  type CFieldType Event_callback_t_Aux "unwrapEvent_callback_t_Aux" =
    FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt

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
  { unwrapEvent_callback_t :: Ptr.FunPtr Event_callback_t_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Event_callback_t) "unwrapEvent_callback_t")
         ) => GHC.Records.HasField "unwrapEvent_callback_t" (Ptr.Ptr Event_callback_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapEvent_callback_t")

instance HsBindgen.Runtime.HasCField.HasCField Event_callback_t "unwrapEvent_callback_t" where

  type CFieldType Event_callback_t "unwrapEvent_callback_t" =
    Ptr.FunPtr Event_callback_t_Aux

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
  , config_t_name :: (HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar
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
  , config_t_user_data :: Ptr.Ptr Void
    {- ^

       User data for callback

    __C declaration:__ @user_data@

    __defined at:__ @documentation\/doxygen_docs.h 250:11@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Config_t where

  staticSizeOf = \_ -> (88 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Config_t where

  readRaw =
    \ptr0 ->
          pure Config_t
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"config_t_id") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"config_t_name") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"config_t_flags") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"config_t_callback") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"config_t_user_data") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Config_t where

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
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"config_t_id") ptr0 config_t_id2
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"config_t_name") ptr0 config_t_name3
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"config_t_flags") ptr0 config_t_flags4
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"config_t_callback") ptr0 config_t_callback5
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"config_t_user_data") ptr0 config_t_user_data6

deriving via HsBindgen.Runtime.Marshal.EquivStorable Config_t instance F.Storable Config_t

instance HsBindgen.Runtime.HasCField.HasCField Config_t "config_t_id" where

  type CFieldType Config_t "config_t_id" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Config_t) "config_t_id")
         ) => GHC.Records.HasField "config_t_id" (Ptr.Ptr Config_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_t_id")

instance HsBindgen.Runtime.HasCField.HasCField Config_t "config_t_name" where

  type CFieldType Config_t "config_t_name" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 64) FC.CChar

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Config_t) "config_t_name")
         ) => GHC.Records.HasField "config_t_name" (Ptr.Ptr Config_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_t_name")

instance HsBindgen.Runtime.HasCField.HasCField Config_t "config_t_flags" where

  type CFieldType Config_t "config_t_flags" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 68

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Config_t) "config_t_flags")
         ) => GHC.Records.HasField "config_t_flags" (Ptr.Ptr Config_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_t_flags")

instance HsBindgen.Runtime.HasCField.HasCField Config_t "config_t_callback" where

  type CFieldType Config_t "config_t_callback" =
    Event_callback_t

  offset# = \_ -> \_ -> 72

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Config_t) "config_t_callback")
         ) => GHC.Records.HasField "config_t_callback" (Ptr.Ptr Config_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_t_callback")

instance HsBindgen.Runtime.HasCField.HasCField Config_t "config_t_user_data" where

  type CFieldType Config_t "config_t_user_data" =
    Ptr.Ptr Void

  offset# = \_ -> \_ -> 80

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Config_t) "config_t_user_data")
         ) => GHC.Records.HasField "config_t_user_data" (Ptr.Ptr Config_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"config_t_user_data")

{-|

  Enumeration with documented values

  This enum shows different status codes.

__C declaration:__ @enum status_code_t@

__defined at:__ @documentation\/doxygen_docs.h 258:9@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Status_code_t = Status_code_t
  { unwrapStatus_code_t :: FC.CInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Status_code_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Status_code_t where

  readRaw =
    \ptr0 ->
          pure Status_code_t
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Status_code_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Status_code_t unwrapStatus_code_t2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapStatus_code_t2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Status_code_t instance F.Storable Status_code_t

deriving via FC.CInt instance Data.Primitive.Types.Prim Status_code_t

instance HsBindgen.Runtime.CEnum.CEnum Status_code_t where

  type CEnumZ Status_code_t = FC.CInt

  toCEnum = Status_code_t

  fromCEnum = unwrapStatus_code_t

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

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Status_code_t where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Status_code_t) "unwrapStatus_code_t")
         ) => GHC.Records.HasField "unwrapStatus_code_t" (Ptr.Ptr Status_code_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapStatus_code_t")

instance HsBindgen.Runtime.HasCField.HasCField Status_code_t "unwrapStatus_code_t" where

  type CFieldType Status_code_t "unwrapStatus_code_t" =
    FC.CInt

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
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Data_union_t_as_parts where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (2 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Data_union_t_as_parts where

  readRaw =
    \ptr0 ->
          pure Data_union_t_as_parts
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"data_union_t_as_parts_low") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"data_union_t_as_parts_high") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Data_union_t_as_parts where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Data_union_t_as_parts data_union_t_as_parts_low2 data_union_t_as_parts_high3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"data_union_t_as_parts_low") ptr0 data_union_t_as_parts_low2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"data_union_t_as_parts_high") ptr0 data_union_t_as_parts_high3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Data_union_t_as_parts instance F.Storable Data_union_t_as_parts

instance Data.Primitive.Types.Prim Data_union_t_as_parts where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (2#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Data_union_t_as_parts (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Data_union_t_as_parts v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Data_union_t_as_parts data_union_t_as_parts_low4 data_union_t_as_parts_high5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) data_union_t_as_parts_low4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) data_union_t_as_parts_high5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Data_union_t_as_parts (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Data_union_t_as_parts v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Data_union_t_as_parts data_union_t_as_parts_low4 data_union_t_as_parts_high5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) data_union_t_as_parts_low4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) data_union_t_as_parts_high5 s6

instance HsBindgen.Runtime.HasCField.HasCField Data_union_t_as_parts "data_union_t_as_parts_low" where

  type CFieldType Data_union_t_as_parts "data_union_t_as_parts_low" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Data_union_t_as_parts) "data_union_t_as_parts_low")
         ) => GHC.Records.HasField "data_union_t_as_parts_low" (Ptr.Ptr Data_union_t_as_parts) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"data_union_t_as_parts_low")

instance HsBindgen.Runtime.HasCField.HasCField Data_union_t_as_parts "data_union_t_as_parts_high" where

  type CFieldType Data_union_t_as_parts "data_union_t_as_parts_high" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Data_union_t_as_parts) "data_union_t_as_parts_high")
         ) => GHC.Records.HasField "data_union_t_as_parts_high" (Ptr.Ptr Data_union_t_as_parts) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"data_union_t_as_parts_high")

{-|

  > data_union_t

  Union with documented fields

  This union demonstrates different data representations.

__C declaration:__ @union data_union_t@

__defined at:__ @documentation\/doxygen_docs.h 281:9@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Data_union_t = Data_union_t
  { unwrapData_union_t :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.StaticSize Data_union_t

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.ReadRaw Data_union_t

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.WriteRaw Data_union_t

deriving via HsBindgen.Runtime.Marshal.EquivStorable Data_union_t instance F.Storable Data_union_t

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim Data_union_t

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
get_data_union_t_as_int =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_int'

-}
set_data_union_t_as_int ::
     HsBindgen.Runtime.LibC.Int32
  -> Data_union_t
set_data_union_t_as_int =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  Float representation

  __See:__ 'set_data_union_t_as_float'

__C declaration:__ @as_float@

__defined at:__ @documentation\/doxygen_docs.h 283:11@

__exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_float ::
     Data_union_t
  -> FC.CFloat
get_data_union_t_as_float =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_float'

-}
set_data_union_t_as_float ::
     FC.CFloat
  -> Data_union_t
set_data_union_t_as_float =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  Byte array representation

  __See:__ 'set_data_union_t_as_bytes'

__C declaration:__ @as_bytes@

__defined at:__ @documentation\/doxygen_docs.h 284:13@

__exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_bytes ::
     Data_union_t
  -> (HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.LibC.Word8
get_data_union_t_as_bytes =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_bytes'

-}
set_data_union_t_as_bytes ::
     (HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.LibC.Word8
  -> Data_union_t
set_data_union_t_as_bytes =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| As Parts Struct

  __See:__ 'set_data_union_t_as_parts'

__C declaration:__ @as_parts@

__defined at:__ @documentation\/doxygen_docs.h 293:30@

__exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_parts ::
     Data_union_t
  -> Data_union_t_as_parts
get_data_union_t_as_parts =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_data_union_t_as_parts'

-}
set_data_union_t_as_parts ::
     Data_union_t_as_parts
  -> Data_union_t
set_data_union_t_as_parts =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Data_union_t "data_union_t_as_int" where

  type CFieldType Data_union_t "data_union_t_as_int" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Data_union_t) "data_union_t_as_int")
         ) => GHC.Records.HasField "data_union_t_as_int" (Ptr.Ptr Data_union_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"data_union_t_as_int")

instance HsBindgen.Runtime.HasCField.HasCField Data_union_t "data_union_t_as_float" where

  type CFieldType Data_union_t "data_union_t_as_float" =
    FC.CFloat

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Data_union_t) "data_union_t_as_float")
         ) => GHC.Records.HasField "data_union_t_as_float" (Ptr.Ptr Data_union_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"data_union_t_as_float")

instance HsBindgen.Runtime.HasCField.HasCField Data_union_t "data_union_t_as_bytes" where

  type CFieldType Data_union_t "data_union_t_as_bytes" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Data_union_t) "data_union_t_as_bytes")
         ) => GHC.Records.HasField "data_union_t_as_bytes" (Ptr.Ptr Data_union_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"data_union_t_as_bytes")

instance HsBindgen.Runtime.HasCField.HasCField Data_union_t "data_union_t_as_parts" where

  type CFieldType Data_union_t "data_union_t_as_parts" =
    Data_union_t_as_parts

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Data_union_t) "data_union_t_as_parts")
         ) => GHC.Records.HasField "data_union_t_as_parts" (Ptr.Ptr Data_union_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"data_union_t_as_parts")

{-|

  > bitfield_t

  Bit field structure

  Demonstrates bit field documentation.

__C declaration:__ @struct bitfield_t@

__defined at:__ @documentation\/doxygen_docs.h 302:9@

__exported by:__ @documentation\/doxygen_docs.h@
-}
data Bitfield_t = Bitfield_t
  { bitfield_t_flag1 :: FC.CUInt
    {- ^

       First flag (1 bit)

    __C declaration:__ @flag1@

    __defined at:__ @documentation\/doxygen_docs.h 303:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_flag2 :: FC.CUInt
    {- ^

       Second flag (1 bit)

    __C declaration:__ @flag2@

    __defined at:__ @documentation\/doxygen_docs.h 304:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_counter :: FC.CUInt
    {- ^

       Counter value (6 bits)

    __C declaration:__ @counter@

    __defined at:__ @documentation\/doxygen_docs.h 305:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_reserved :: FC.CUInt
    {- ^

       Reserved bits (24 bits)

    __C declaration:__ @reserved@

    __defined at:__ @documentation\/doxygen_docs.h 306:14@

    __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Bitfield_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Bitfield_t where

  readRaw =
    \ptr0 ->
          pure Bitfield_t
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"bitfield_t_flag1") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"bitfield_t_flag2") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"bitfield_t_counter") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"bitfield_t_reserved") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Bitfield_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bitfield_t
            bitfield_t_flag12
            bitfield_t_flag23
            bitfield_t_counter4
            bitfield_t_reserved5 ->
                 HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"bitfield_t_flag1") ptr0 bitfield_t_flag12
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"bitfield_t_flag2") ptr0 bitfield_t_flag23
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"bitfield_t_counter") ptr0 bitfield_t_counter4
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"bitfield_t_reserved") ptr0 bitfield_t_reserved5

deriving via HsBindgen.Runtime.Marshal.EquivStorable Bitfield_t instance F.Storable Bitfield_t

instance Data.Primitive.Types.Prim Bitfield_t where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Bitfield_t (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (4#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (4#) i1) (1#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (4#) i1) (2#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (4#) i1) (3#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (4#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (4#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (4#) i1) (2#)) s5 of
                    (# s7, v8 #) ->
                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (4#) i1) (3#)) s7 of
                        (# s9, v10 #) -> (# s9, Bitfield_t v4 v6 v8 v10 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Bitfield_t
                bitfield_t_flag14
                bitfield_t_flag25
                bitfield_t_counter6
                bitfield_t_reserved7 ->
                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (4#) i1) (0#)) bitfield_t_flag14 s3 of
                    s8 ->
                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (4#) i1) (1#)) bitfield_t_flag25 s8 of
                        s9 ->
                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (4#) i1) (2#)) bitfield_t_counter6 s9 of
                            s10 ->
                              Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (4#) i1) (3#)) bitfield_t_reserved7 s10

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Bitfield_t (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (4#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (4#) i1) (1#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (4#) i1) (2#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (4#) i1) (3#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (4#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (4#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (4#) i1) (2#)) s5 of
                    (# s7, v8 #) ->
                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (4#) i1) (3#)) s7 of
                        (# s9, v10 #) -> (# s9, Bitfield_t v4 v6 v8 v10 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Bitfield_t
                bitfield_t_flag14
                bitfield_t_flag25
                bitfield_t_counter6
                bitfield_t_reserved7 ->
                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (4#) i1) (0#)) bitfield_t_flag14 s3 of
                    s8 ->
                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (4#) i1) (1#)) bitfield_t_flag25 s8 of
                        s9 ->
                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (4#) i1) (2#)) bitfield_t_counter6 s9 of
                            s10 ->
                              Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (4#) i1) (3#)) bitfield_t_reserved7 s10

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_flag1" where

  type CBitfieldType Bitfield_t "bitfield_t_flag1" =
    FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Bitfield_t) "bitfield_t_flag1")
         ) => GHC.Records.HasField "bitfield_t_flag1" (Ptr.Ptr Bitfield_t) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"bitfield_t_flag1")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_flag2" where

  type CBitfieldType Bitfield_t "bitfield_t_flag2" =
    FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 1

  bitfieldWidth# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Bitfield_t) "bitfield_t_flag2")
         ) => GHC.Records.HasField "bitfield_t_flag2" (Ptr.Ptr Bitfield_t) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"bitfield_t_flag2")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_counter" where

  type CBitfieldType Bitfield_t "bitfield_t_counter" =
    FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 6

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Bitfield_t) "bitfield_t_counter")
         ) => GHC.Records.HasField "bitfield_t_counter" (Ptr.Ptr Bitfield_t) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"bitfield_t_counter")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_reserved" where

  type CBitfieldType Bitfield_t "bitfield_t_reserved" =
    FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Bitfield_t) "bitfield_t_reserved")
         ) => GHC.Records.HasField "bitfield_t_reserved" (Ptr.Ptr Bitfield_t) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"bitfield_t_reserved")

{-| Auxiliary type used by 'Processor_fn_t'

__C declaration:__ @processor_fn_t@

__defined at:__ @documentation\/doxygen_docs.h 317:15@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Processor_fn_t_Aux = Processor_fn_t_Aux
  { unwrapProcessor_fn_t_Aux :: FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d4e16471c82d5df0_base ::
     (GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32))

-- __unique:__ @toProcessor_fn_t_Aux@
hs_bindgen_d4e16471c82d5df0 ::
     Processor_fn_t_Aux
  -> IO (Ptr.FunPtr Processor_fn_t_Aux)
hs_bindgen_d4e16471c82d5df0 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_d4e16471c82d5df0_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0d4b3d0461629423_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> (Ptr.Ptr Void) -> IO GHC.Int.Int32

-- __unique:__ @fromProcessor_fn_t_Aux@
hs_bindgen_0d4b3d0461629423 ::
     Ptr.FunPtr Processor_fn_t_Aux
  -> Processor_fn_t_Aux
hs_bindgen_0d4b3d0461629423 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_0d4b3d0461629423_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Processor_fn_t_Aux where

  toFunPtr = hs_bindgen_d4e16471c82d5df0

instance HsBindgen.Runtime.FunPtr.FromFunPtr Processor_fn_t_Aux where

  fromFunPtr = hs_bindgen_0d4b3d0461629423

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Processor_fn_t_Aux) "unwrapProcessor_fn_t_Aux")
         ) => GHC.Records.HasField "unwrapProcessor_fn_t_Aux" (Ptr.Ptr Processor_fn_t_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapProcessor_fn_t_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Processor_fn_t_Aux "unwrapProcessor_fn_t_Aux" where

  type CFieldType Processor_fn_t_Aux "unwrapProcessor_fn_t_Aux" =
    FC.CInt -> (Ptr.Ptr Void) -> IO FC.CInt

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
  { unwrapProcessor_fn_t :: Ptr.FunPtr Processor_fn_t_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Processor_fn_t) "unwrapProcessor_fn_t")
         ) => GHC.Records.HasField "unwrapProcessor_fn_t" (Ptr.Ptr Processor_fn_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapProcessor_fn_t")

instance HsBindgen.Runtime.HasCField.HasCField Processor_fn_t "unwrapProcessor_fn_t" where

  type CFieldType Processor_fn_t "unwrapProcessor_fn_t" =
    Ptr.FunPtr Processor_fn_t_Aux

  offset# = \_ -> \_ -> 0

{-|

  > filename_t

  Array typedef with size

__C declaration:__ @filename_t@

__defined at:__ @documentation\/doxygen_docs.h 323:14@

__exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Filename_t = Filename_t
  { unwrapFilename_t :: (HsBindgen.Runtime.ConstantArray.ConstantArray 256) FC.CChar
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Filename_t) "unwrapFilename_t")
         ) => GHC.Records.HasField "unwrapFilename_t" (Ptr.Ptr Filename_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFilename_t")

instance HsBindgen.Runtime.HasCField.HasCField Filename_t "unwrapFilename_t" where

  type CFieldType Filename_t "unwrapFilename_t" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 256) FC.CChar

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
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Flexible_array_Aux where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Flexible_array_Aux where

  readRaw =
    \ptr0 ->
          pure Flexible_array
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"flexible_array_count") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Flexible_array_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Flexible_array flexible_array_count2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"flexible_array_count") ptr0 flexible_array_count2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Flexible_array_Aux instance F.Storable Flexible_array_Aux

instance Data.Primitive.Types.Prim Flexible_array_Aux where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (8#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Flexible_array (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Flexible_array v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Flexible_array flexible_array_count4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 flexible_array_count4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Flexible_array (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Flexible_array v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Flexible_array flexible_array_count4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 flexible_array_count4 s3

instance HsBindgen.Runtime.HasCField.HasCField Flexible_array_Aux "flexible_array_count" where

  type CFieldType Flexible_array_Aux "flexible_array_count" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Flexible_array_Aux) "flexible_array_count")
         ) => GHC.Records.HasField "flexible_array_count" (Ptr.Ptr Flexible_array_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"flexible_array_count")

instance HsBindgen.Runtime.FLAM.Offset FC.CInt Flexible_array_Aux where

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
  (HsBindgen.Runtime.FLAM.WithFlam FC.CInt) Flexible_array_Aux
