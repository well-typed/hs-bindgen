{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Example
    ( Example.Opaque_struct
    , Example.Opaque_union
    , Example.Api_version_t(..)
      -- * Core Data Types
    , Example.mAX_NAME_LENGTH
    , Example.Size_type(..)
    , Example.Color_enum(..)
    , pattern Example.COLOR_RED
    , pattern Example.COLOR_GREEN
    , pattern Example.COLOR_BLUE
      -- * Function Definitions
    , Example.Event_callback_t_Aux(..)
    , Example.Event_callback_t(..)
    , Example.Config_t(..)
    , Example.Status_code_t(..)
    , pattern Example.STATUS_OK
    , pattern Example.STATUS_INVALID_PARAM
    , pattern Example.STATUS_NO_MEMORY
    , pattern Example.STATUS_TIMEOUT
    , pattern Example.STATUS_ERROR
    , Example.Data_union_t_as_parts(..)
    , Example.Data_union_t(..)
    , Example.get_data_union_t_as_int
    , Example.set_data_union_t_as_int
    , Example.get_data_union_t_as_float
    , Example.set_data_union_t_as_float
    , Example.get_data_union_t_as_bytes
    , Example.set_data_union_t_as_bytes
    , Example.get_data_union_t_as_parts
    , Example.set_data_union_t_as_parts
    , Example.Bitfield_t(..)
    , Example.Processor_fn_t_Aux(..)
    , Example.Processor_fn_t(..)
    , Example.Filename_t(..)
    , Example.Flexible_array_Aux(..)
    , Example.Flexible_array
      -- * Extra Doxygen Coverage
    , Example.Dyn_array_t(..)
    , Example.Multi_anon_t_pos(..)
    , Example.Multi_anon_t_dim(..)
    , Example.Multi_anon_t(..)
    , Example.Named_inner(..)
    , Example.Named_outer(..)
    , Example.Deep_mid_anon_field(..)
    , Example.Deep_mid(..)
    , Example.Deep_outer(..)
    , Example.Unnamed_field_t_ua(..)
    , Example.Unnamed_field_t(..)
    )
  where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Maximum length for name strings.

    __C declaration:__ @macro MAX_NAME_LENGTH@

    __defined at:__ @documentation\/doxygen_docs.h 39:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
mAX_NAME_LENGTH :: RIP.CInt
mAX_NAME_LENGTH = (64 :: RIP.CInt)

{-| Size type for this library.

    This is the comment __title__

    __C declaration:__ @size_type@

    __defined at:__ @documentation\/doxygen_docs.h 56:16@

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

instance ( ty ~ HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "unwrapSize_type" (RIP.Ptr Size_type) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSize_type")

instance HasCField.HasCField Size_type "unwrapSize_type" where

  type CFieldType Size_type "unwrapSize_type" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct opaque_struct@

    __defined at:__ @documentation\/doxygen_docs.h 76:8@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Opaque_struct

{-| __C declaration:__ @union opaque_union@

    __defined at:__ @documentation\/doxygen_docs.h 81:7@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Opaque_union

{-| Color enumeration without typedef.

    __C declaration:__ @enum color_enum@

    __defined at:__ @documentation\/doxygen_docs.h 87:6@

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

  fromCEnum = RIP.getField @"unwrapColor_enum"

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

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapColor_enum" (RIP.Ptr Color_enum) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapColor_enum")

instance HasCField.HasCField Color_enum "unwrapColor_enum" where

  type CFieldType Color_enum "unwrapColor_enum" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| Red color

    __C declaration:__ @COLOR_RED@

    __defined at:__ @documentation\/doxygen_docs.h 88:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern COLOR_RED :: Color_enum
pattern COLOR_RED = Color_enum 0

{-| Green color

    __C declaration:__ @COLOR_GREEN@

    __defined at:__ @documentation\/doxygen_docs.h 89:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern COLOR_GREEN :: Color_enum
pattern COLOR_GREEN = Color_enum 1

{-| Blue color

    __C declaration:__ @COLOR_BLUE@

    __defined at:__ @documentation\/doxygen_docs.h 90:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern COLOR_BLUE :: Color_enum
pattern COLOR_BLUE = Color_enum 2

{-| Auxiliary type used by 'Event_callback_t'

    __C declaration:__ @event_callback_t@

    __defined at:__ @documentation\/doxygen_docs.h 231:15@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Event_callback_t_Aux = Event_callback_t_Aux
  { unwrapEvent_callback_t_Aux :: RIP.CInt -> RIP.Ptr RIP.Void -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_111918b0aee2a7fb_base ::
     (RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32))

-- __unique:__ @toEvent_callback_t_Aux@
hs_bindgen_111918b0aee2a7fb ::
     Event_callback_t_Aux
  -> IO (RIP.FunPtr Event_callback_t_Aux)
hs_bindgen_111918b0aee2a7fb =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_111918b0aee2a7fb_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_9e9d478c2d75628c_base ::
     RIP.FunPtr (RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32)
  -> RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32

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

instance ( ty ~ (RIP.CInt -> RIP.Ptr RIP.Void -> IO RIP.CInt)
         ) => RIP.HasField "unwrapEvent_callback_t_Aux" (RIP.Ptr Event_callback_t_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEvent_callback_t_Aux")

instance HasCField.HasCField Event_callback_t_Aux "unwrapEvent_callback_t_Aux" where

  type CFieldType Event_callback_t_Aux "unwrapEvent_callback_t_Aux" =
    RIP.CInt -> RIP.Ptr RIP.Void -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Callback function type.

    [__@event_type@__]: Type of event

    [__@user_data@__]: User-provided data

    __Returns:__ Handling result

    __C declaration:__ @event_callback_t@

    __defined at:__ @documentation\/doxygen_docs.h 231:15@

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

instance ( ty ~ RIP.FunPtr Event_callback_t_Aux
         ) => RIP.HasField "unwrapEvent_callback_t" (RIP.Ptr Event_callback_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapEvent_callback_t")

instance HasCField.HasCField Event_callback_t "unwrapEvent_callback_t" where

  type CFieldType Event_callback_t "unwrapEvent_callback_t" =
    RIP.FunPtr Event_callback_t_Aux

  offset# = \_ -> \_ -> 0

{-| Structure with documented fields.

    This structure demonstrates field documentation.

    __C declaration:__ @struct config_t@

    __defined at:__ @documentation\/doxygen_docs.h 238:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Config_t = Config_t
  { config_t_id :: HsBindgen.Runtime.LibC.Word32
    {- ^ Unique identifier.

         __C declaration:__ @id@

         __defined at:__ @documentation\/doxygen_docs.h 240:14@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_name :: CA.ConstantArray 64 RIP.CChar
    {- ^ Human-readable name.

         __C declaration:__ @name@

         __defined at:__ @documentation\/doxygen_docs.h 243:10@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_flags :: HsBindgen.Runtime.LibC.Word32
    {- ^ Configuration flags.

         __C declaration:__ @flags@

         __defined at:__ @documentation\/doxygen_docs.h 246:14@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_callback :: Event_callback_t
    {- ^ Optional callback function.

         See also: 'Event_callback_t'

         __C declaration:__ @callback@

         __defined at:__ @documentation\/doxygen_docs.h 253:22@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , config_t_user_data :: RIP.Ptr RIP.Void
    {- ^ User data for callback.

         __C declaration:__ @user_data@

         __defined at:__ @documentation\/doxygen_docs.h 256:11@

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

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => RIP.HasField "config_t_id" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_id")

instance HasCField.HasCField Config_t "config_t_name" where

  type CFieldType Config_t "config_t_name" =
    CA.ConstantArray 64 RIP.CChar

  offset# = \_ -> \_ -> 4

instance ( ty ~ CA.ConstantArray 64 RIP.CChar
         ) => RIP.HasField "config_t_name" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_name")

instance HasCField.HasCField Config_t "config_t_flags" where

  type CFieldType Config_t "config_t_flags" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 68

instance ( ty ~ HsBindgen.Runtime.LibC.Word32
         ) => RIP.HasField "config_t_flags" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_flags")

instance HasCField.HasCField Config_t "config_t_callback" where

  type CFieldType Config_t "config_t_callback" =
    Event_callback_t

  offset# = \_ -> \_ -> 72

instance ( ty ~ Event_callback_t
         ) => RIP.HasField "config_t_callback" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_callback")

instance HasCField.HasCField Config_t "config_t_user_data" where

  type CFieldType Config_t "config_t_user_data" =
    RIP.Ptr RIP.Void

  offset# = \_ -> \_ -> 80

instance ( ty ~ RIP.Ptr RIP.Void
         ) => RIP.HasField "config_t_user_data" (RIP.Ptr Config_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"config_t_user_data")

{-| Enumeration with documented values.

    This enum shows different status codes.

    __C declaration:__ @enum status_code_t@

    __defined at:__ @documentation\/doxygen_docs.h 264:9@

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

  fromCEnum = RIP.getField @"unwrapStatus_code_t"

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

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapStatus_code_t" (RIP.Ptr Status_code_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapStatus_code_t")

instance HasCField.HasCField Status_code_t "unwrapStatus_code_t" where

  type CFieldType Status_code_t "unwrapStatus_code_t" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Operation successful.

    __C declaration:__ @STATUS_OK@

    __defined at:__ @documentation\/doxygen_docs.h 266:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_OK :: Status_code_t
pattern STATUS_OK = Status_code_t 0

{-| Invalid parameter provided.

    __C declaration:__ @STATUS_INVALID_PARAM@

    __defined at:__ @documentation\/doxygen_docs.h 269:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_INVALID_PARAM :: Status_code_t
pattern STATUS_INVALID_PARAM = Status_code_t (-1)

{-| Memory allocation failed.

    __C declaration:__ @STATUS_NO_MEMORY@

    __defined at:__ @documentation\/doxygen_docs.h 272:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_NO_MEMORY :: Status_code_t
pattern STATUS_NO_MEMORY = Status_code_t (-2)

{-| Operation timed out.

    __C declaration:__ @STATUS_TIMEOUT@

    __defined at:__ @documentation\/doxygen_docs.h 275:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_TIMEOUT :: Status_code_t
pattern STATUS_TIMEOUT = Status_code_t (-3)

{-| Generic error.

    __C declaration:__ @STATUS_ERROR@

    __defined at:__ @documentation\/doxygen_docs.h 278:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
pattern STATUS_ERROR :: Status_code_t
pattern STATUS_ERROR = Status_code_t (-99)

{-| __C declaration:__ @struct \@data_union_t_as_parts@

    __defined at:__ @documentation\/doxygen_docs.h 296:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Data_union_t_as_parts = Data_union_t_as_parts
  { data_union_t_as_parts_low :: HsBindgen.Runtime.LibC.Word16
    {- ^ Low 16 bits.

         __C declaration:__ @low@

         __defined at:__ @documentation\/doxygen_docs.h 297:18@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , data_union_t_as_parts_high :: HsBindgen.Runtime.LibC.Word16
    {- ^ High 16 bits.

         __C declaration:__ @high@

         __defined at:__ @documentation\/doxygen_docs.h 298:18@

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

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "data_union_t_as_parts_low" (RIP.Ptr Data_union_t_as_parts) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_parts_low")

instance HasCField.HasCField Data_union_t_as_parts "data_union_t_as_parts_high" where

  type CFieldType Data_union_t_as_parts "data_union_t_as_parts_high" =
    HsBindgen.Runtime.LibC.Word16

  offset# = \_ -> \_ -> 2

instance ( ty ~ HsBindgen.Runtime.LibC.Word16
         ) => RIP.HasField "data_union_t_as_parts_high" (RIP.Ptr Data_union_t_as_parts) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_parts_high")

{-| Union with documented fields.

    This union demonstrates different data representations.

    __C declaration:__ @union data_union_t@

    __defined at:__ @documentation\/doxygen_docs.h 287:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Data_union_t = Data_union_t
  { unwrapData_union_t :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize Data_union_t

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw Data_union_t

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw Data_union_t

deriving via Marshal.EquivStorable Data_union_t instance RIP.Storable Data_union_t

{-| Integer representation.

    __See:__ 'set_data_union_t_as_int'

    __C declaration:__ @as_int@

    __defined at:__ @documentation\/doxygen_docs.h 288:13@

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

{-| Float representation.

    __See:__ 'set_data_union_t_as_float'

    __C declaration:__ @as_float@

    __defined at:__ @documentation\/doxygen_docs.h 289:11@

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

{-| Byte array representation.

    __See:__ 'set_data_union_t_as_bytes'

    __C declaration:__ @as_bytes@

    __defined at:__ @documentation\/doxygen_docs.h 290:13@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
get_data_union_t_as_bytes ::
     Data_union_t
  -> CA.ConstantArray 4 HsBindgen.Runtime.LibC.Word8
get_data_union_t_as_bytes = RIP.getUnionPayload

{-|

    __See:__ 'get_data_union_t_as_bytes'

-}
set_data_union_t_as_bytes ::
     CA.ConstantArray 4 HsBindgen.Runtime.LibC.Word8
  -> Data_union_t
set_data_union_t_as_bytes = RIP.setUnionPayload

{-| Structured representation.

    Allows access to high and low parts separately As Parts Struct

    __See:__ 'set_data_union_t_as_parts'

    __C declaration:__ @as_parts@

    __defined at:__ @documentation\/doxygen_docs.h 299:30@

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

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => RIP.HasField "data_union_t_as_int" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_int")

instance HasCField.HasCField Data_union_t "data_union_t_as_float" where

  type CFieldType Data_union_t "data_union_t_as_float" =
    RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CFloat
         ) => RIP.HasField "data_union_t_as_float" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_float")

instance HasCField.HasCField Data_union_t "data_union_t_as_bytes" where

  type CFieldType Data_union_t "data_union_t_as_bytes" =
    CA.ConstantArray 4 HsBindgen.Runtime.LibC.Word8

  offset# = \_ -> \_ -> 0

instance ( ty ~ CA.ConstantArray 4 HsBindgen.Runtime.LibC.Word8
         ) => RIP.HasField "data_union_t_as_bytes" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_bytes")

instance HasCField.HasCField Data_union_t "data_union_t_as_parts" where

  type CFieldType Data_union_t "data_union_t_as_parts" =
    Data_union_t_as_parts

  offset# = \_ -> \_ -> 0

instance ( ty ~ Data_union_t_as_parts
         ) => RIP.HasField "data_union_t_as_parts" (RIP.Ptr Data_union_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"data_union_t_as_parts")

{-| Bit field structure.

    Demonstrates bit field documentation.

    __C declaration:__ @struct bitfield_t@

    __defined at:__ @documentation\/doxygen_docs.h 308:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Bitfield_t = Bitfield_t
  { bitfield_t_flag1 :: RIP.CUInt
    {- ^ First flag (1 bit).

         __C declaration:__ @flag1@

         __defined at:__ @documentation\/doxygen_docs.h 309:14@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_flag2 :: RIP.CUInt
    {- ^ Second flag (1 bit).

         __C declaration:__ @flag2@

         __defined at:__ @documentation\/doxygen_docs.h 310:14@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_counter :: RIP.CUInt
    {- ^ Counter value (6 bits).

         __C declaration:__ @counter@

         __defined at:__ @documentation\/doxygen_docs.h 311:14@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , bitfield_t_reserved :: RIP.CUInt
    {- ^ Reserved bits (24 bits).

         __C declaration:__ @reserved@

         __defined at:__ @documentation\/doxygen_docs.h 312:14@

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

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "bitfield_t_flag1" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_flag1")

instance HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_flag2" where

  type CBitfieldType Bitfield_t "bitfield_t_flag2" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 1

  bitfieldWidth# = \_ -> \_ -> 1

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "bitfield_t_flag2" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_flag2")

instance HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_counter" where

  type CBitfieldType Bitfield_t "bitfield_t_counter" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 6

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "bitfield_t_counter" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_counter")

instance HasCBitfield.HasCBitfield Bitfield_t "bitfield_t_reserved" where

  type CBitfieldType Bitfield_t "bitfield_t_reserved" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 8

  bitfieldWidth# = \_ -> \_ -> 24

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "bitfield_t_reserved" (RIP.Ptr Bitfield_t) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"bitfield_t_reserved")

{-| Auxiliary type used by 'Processor_fn_t'

    __C declaration:__ @processor_fn_t@

    __defined at:__ @documentation\/doxygen_docs.h 323:15@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Processor_fn_t_Aux = Processor_fn_t_Aux
  { unwrapProcessor_fn_t_Aux :: RIP.CInt -> RIP.Ptr RIP.Void -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d4e16471c82d5df0_base ::
     (RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32))

-- __unique:__ @toProcessor_fn_t_Aux@
hs_bindgen_d4e16471c82d5df0 ::
     Processor_fn_t_Aux
  -> IO (RIP.FunPtr Processor_fn_t_Aux)
hs_bindgen_d4e16471c82d5df0 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_d4e16471c82d5df0_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0d4b3d0461629423_base ::
     RIP.FunPtr (RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32)
  -> RIP.Int32 -> RIP.Ptr RIP.Void -> IO RIP.Int32

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

instance ( ty ~ (RIP.CInt -> RIP.Ptr RIP.Void -> IO RIP.CInt)
         ) => RIP.HasField "unwrapProcessor_fn_t_Aux" (RIP.Ptr Processor_fn_t_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapProcessor_fn_t_Aux")

instance HasCField.HasCField Processor_fn_t_Aux "unwrapProcessor_fn_t_Aux" where

  type CFieldType Processor_fn_t_Aux "unwrapProcessor_fn_t_Aux" =
    RIP.CInt -> RIP.Ptr RIP.Void -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| Function pointer typedef.

    [__@input@__]: Input value

    [__@context@__]: Context pointer

    __Returns:__ Processed value

    __C declaration:__ @processor_fn_t@

    __defined at:__ @documentation\/doxygen_docs.h 323:15@

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

instance ( ty ~ RIP.FunPtr Processor_fn_t_Aux
         ) => RIP.HasField "unwrapProcessor_fn_t" (RIP.Ptr Processor_fn_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapProcessor_fn_t")

instance HasCField.HasCField Processor_fn_t "unwrapProcessor_fn_t" where

  type CFieldType Processor_fn_t "unwrapProcessor_fn_t" =
    RIP.FunPtr Processor_fn_t_Aux

  offset# = \_ -> \_ -> 0

{-| Array typedef with size.

    __C declaration:__ @filename_t@

    __defined at:__ @documentation\/doxygen_docs.h 329:14@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Filename_t = Filename_t
  { unwrapFilename_t :: CA.ConstantArray 256 RIP.CChar
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 256 RIP.CChar
         ) => RIP.HasField "unwrapFilename_t" (RIP.Ptr Filename_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFilename_t")

instance HasCField.HasCField Filename_t "unwrapFilename_t" where

  type CFieldType Filename_t "unwrapFilename_t" =
    CA.ConstantArray 256 RIP.CChar

  offset# = \_ -> \_ -> 0

{-| Structure with flexible array member.

    Used for variable-length data buffers.

    __C declaration:__ @struct flexible_array@

    __defined at:__ @documentation\/doxygen_docs.h 365:8@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Flexible_array_Aux = Flexible_array
  { flexible_array_count :: HsBindgen.Runtime.LibC.CSize
    {- ^ Number of elements.

         __C declaration:__ @count@

         __defined at:__ @documentation\/doxygen_docs.h 366:12@

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

instance ( ty ~ HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "flexible_array_count" (RIP.Ptr Flexible_array_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"flexible_array_count")

instance FLAM.Offset RIP.CInt Flexible_array_Aux where

  offset = \_ty0 -> 8

{-| Structure with flexible array member.

    Used for variable-length data buffers.

    __C declaration:__ @struct flexible_array@

    __defined at:__ @documentation\/doxygen_docs.h 365:8@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
type Flexible_array =
  FLAM.WithFlam RIP.CInt Flexible_array_Aux

{-| Struct with invariant.

    __Invariant:__ capacity >= size at all times

    __C declaration:__ @struct dyn_array_t@

    __defined at:__ @documentation\/doxygen_docs.h 523:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Dyn_array_t = Dyn_array_t
  { dyn_array_t_data :: RIP.Ptr RIP.CInt
    {- ^ Pointer to data.

         __C declaration:__ @data@

         __defined at:__ @documentation\/doxygen_docs.h 524:10@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , dyn_array_t_size :: HsBindgen.Runtime.LibC.CSize
    {- ^ Current number of elements.

         __C declaration:__ @size@

         __defined at:__ @documentation\/doxygen_docs.h 525:12@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , dyn_array_t_capacity :: HsBindgen.Runtime.LibC.CSize
    {- ^ Allocated capacity.

         __C declaration:__ @capacity@

         __defined at:__ @documentation\/doxygen_docs.h 526:12@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Dyn_array_t where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Dyn_array_t where

  readRaw =
    \ptr0 ->
          pure Dyn_array_t
      <*> HasCField.readRaw (RIP.Proxy @"dyn_array_t_data") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dyn_array_t_size") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dyn_array_t_capacity") ptr0

instance Marshal.WriteRaw Dyn_array_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dyn_array_t dyn_array_t_data2 dyn_array_t_size3 dyn_array_t_capacity4 ->
               HasCField.writeRaw (RIP.Proxy @"dyn_array_t_data") ptr0 dyn_array_t_data2
            >> HasCField.writeRaw (RIP.Proxy @"dyn_array_t_size") ptr0 dyn_array_t_size3
            >> HasCField.writeRaw (RIP.Proxy @"dyn_array_t_capacity") ptr0 dyn_array_t_capacity4

deriving via Marshal.EquivStorable Dyn_array_t instance RIP.Storable Dyn_array_t

instance HasCField.HasCField Dyn_array_t "dyn_array_t_data" where

  type CFieldType Dyn_array_t "dyn_array_t_data" =
    RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.Ptr RIP.CInt
         ) => RIP.HasField "dyn_array_t_data" (RIP.Ptr Dyn_array_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dyn_array_t_data")

instance HasCField.HasCField Dyn_array_t "dyn_array_t_size" where

  type CFieldType Dyn_array_t "dyn_array_t_size" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 8

instance ( ty ~ HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "dyn_array_t_size" (RIP.Ptr Dyn_array_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dyn_array_t_size")

instance HasCField.HasCField Dyn_array_t "dyn_array_t_capacity" where

  type CFieldType Dyn_array_t "dyn_array_t_capacity" =
    HsBindgen.Runtime.LibC.CSize

  offset# = \_ -> \_ -> 16

instance ( ty ~ HsBindgen.Runtime.LibC.CSize
         ) => RIP.HasField "dyn_array_t_capacity" (RIP.Ptr Dyn_array_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dyn_array_t_capacity")

{-| __C declaration:__ @struct \@multi_anon_t_pos@

    __defined at:__ @documentation\/doxygen_docs.h 586:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Multi_anon_t_pos = Multi_anon_t_pos
  { multi_anon_t_pos_x :: RIP.CFloat
    {- ^ X coordinate.

         __C declaration:__ @x@

         __defined at:__ @documentation\/doxygen_docs.h 587:15@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , multi_anon_t_pos_y :: RIP.CFloat
    {- ^ Y coordinate.

         __C declaration:__ @y@

         __defined at:__ @documentation\/doxygen_docs.h 588:15@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Multi_anon_t_pos where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Multi_anon_t_pos where

  readRaw =
    \ptr0 ->
          pure Multi_anon_t_pos
      <*> HasCField.readRaw (RIP.Proxy @"multi_anon_t_pos_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"multi_anon_t_pos_y") ptr0

instance Marshal.WriteRaw Multi_anon_t_pos where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Multi_anon_t_pos multi_anon_t_pos_x2 multi_anon_t_pos_y3 ->
               HasCField.writeRaw (RIP.Proxy @"multi_anon_t_pos_x") ptr0 multi_anon_t_pos_x2
            >> HasCField.writeRaw (RIP.Proxy @"multi_anon_t_pos_y") ptr0 multi_anon_t_pos_y3

deriving via Marshal.EquivStorable Multi_anon_t_pos instance RIP.Storable Multi_anon_t_pos

instance HasCField.HasCField Multi_anon_t_pos "multi_anon_t_pos_x" where

  type CFieldType Multi_anon_t_pos "multi_anon_t_pos_x" =
    RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CFloat
         ) => RIP.HasField "multi_anon_t_pos_x" (RIP.Ptr Multi_anon_t_pos) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"multi_anon_t_pos_x")

instance HasCField.HasCField Multi_anon_t_pos "multi_anon_t_pos_y" where

  type CFieldType Multi_anon_t_pos "multi_anon_t_pos_y" =
    RIP.CFloat

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CFloat
         ) => RIP.HasField "multi_anon_t_pos_y" (RIP.Ptr Multi_anon_t_pos) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"multi_anon_t_pos_y")

{-| __C declaration:__ @struct \@multi_anon_t_dim@

    __defined at:__ @documentation\/doxygen_docs.h 594:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Multi_anon_t_dim = Multi_anon_t_dim
  { multi_anon_t_dim_w :: RIP.CFloat
    {- ^ Width.

         __C declaration:__ @w@

         __defined at:__ @documentation\/doxygen_docs.h 595:15@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , multi_anon_t_dim_h :: RIP.CFloat
    {- ^ Height.

         __C declaration:__ @h@

         __defined at:__ @documentation\/doxygen_docs.h 596:15@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Multi_anon_t_dim where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Multi_anon_t_dim where

  readRaw =
    \ptr0 ->
          pure Multi_anon_t_dim
      <*> HasCField.readRaw (RIP.Proxy @"multi_anon_t_dim_w") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"multi_anon_t_dim_h") ptr0

instance Marshal.WriteRaw Multi_anon_t_dim where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Multi_anon_t_dim multi_anon_t_dim_w2 multi_anon_t_dim_h3 ->
               HasCField.writeRaw (RIP.Proxy @"multi_anon_t_dim_w") ptr0 multi_anon_t_dim_w2
            >> HasCField.writeRaw (RIP.Proxy @"multi_anon_t_dim_h") ptr0 multi_anon_t_dim_h3

deriving via Marshal.EquivStorable Multi_anon_t_dim instance RIP.Storable Multi_anon_t_dim

instance HasCField.HasCField Multi_anon_t_dim "multi_anon_t_dim_w" where

  type CFieldType Multi_anon_t_dim "multi_anon_t_dim_w" =
    RIP.CFloat

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CFloat
         ) => RIP.HasField "multi_anon_t_dim_w" (RIP.Ptr Multi_anon_t_dim) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"multi_anon_t_dim_w")

instance HasCField.HasCField Multi_anon_t_dim "multi_anon_t_dim_h" where

  type CFieldType Multi_anon_t_dim "multi_anon_t_dim_h" =
    RIP.CFloat

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CFloat
         ) => RIP.HasField "multi_anon_t_dim_h" (RIP.Ptr Multi_anon_t_dim) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"multi_anon_t_dim_h")

{-| Struct with multiple anonymous inner structs.

    Tests that doxygen comment enrichment correctly associates field comments with anonymous inner structs at multiple nesting levels.

    __C declaration:__ @struct multi_anon_t@

    __defined at:__ @documentation\/doxygen_docs.h 582:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Multi_anon_t = Multi_anon_t
  { multi_anon_t_pos :: Multi_anon_t_pos
    {- ^ Position in 2D space.

         Position fields

         __C declaration:__ @pos@

         __defined at:__ @documentation\/doxygen_docs.h 589:30@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , multi_anon_t_dim :: Multi_anon_t_dim
    {- ^ Dimensions.

         Dimension fields

         __C declaration:__ @dim@

         __defined at:__ @documentation\/doxygen_docs.h 597:31@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Multi_anon_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Multi_anon_t where

  readRaw =
    \ptr0 ->
          pure Multi_anon_t
      <*> HasCField.readRaw (RIP.Proxy @"multi_anon_t_pos") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"multi_anon_t_dim") ptr0

instance Marshal.WriteRaw Multi_anon_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Multi_anon_t multi_anon_t_pos2 multi_anon_t_dim3 ->
               HasCField.writeRaw (RIP.Proxy @"multi_anon_t_pos") ptr0 multi_anon_t_pos2
            >> HasCField.writeRaw (RIP.Proxy @"multi_anon_t_dim") ptr0 multi_anon_t_dim3

deriving via Marshal.EquivStorable Multi_anon_t instance RIP.Storable Multi_anon_t

instance HasCField.HasCField Multi_anon_t "multi_anon_t_pos" where

  type CFieldType Multi_anon_t "multi_anon_t_pos" =
    Multi_anon_t_pos

  offset# = \_ -> \_ -> 0

instance ( ty ~ Multi_anon_t_pos
         ) => RIP.HasField "multi_anon_t_pos" (RIP.Ptr Multi_anon_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"multi_anon_t_pos")

instance HasCField.HasCField Multi_anon_t "multi_anon_t_dim" where

  type CFieldType Multi_anon_t "multi_anon_t_dim" =
    Multi_anon_t_dim

  offset# = \_ -> \_ -> 8

instance ( ty ~ Multi_anon_t_dim
         ) => RIP.HasField "multi_anon_t_dim" (RIP.Ptr Multi_anon_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"multi_anon_t_dim")

{-| Named inner struct

    __C declaration:__ @struct named_inner@

    __defined at:__ @documentation\/doxygen_docs.h 608:12@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Named_inner = Named_inner
  { named_inner_nx :: RIP.CInt
    {- ^ Inner field nx

         __C declaration:__ @nx@

         __defined at:__ @documentation\/doxygen_docs.h 610:13@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , named_inner_ny :: RIP.CInt
    {- ^ Inner field ny

         __C declaration:__ @ny@

         __defined at:__ @documentation\/doxygen_docs.h 612:13@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Named_inner where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Named_inner where

  readRaw =
    \ptr0 ->
          pure Named_inner
      <*> HasCField.readRaw (RIP.Proxy @"named_inner_nx") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"named_inner_ny") ptr0

instance Marshal.WriteRaw Named_inner where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named_inner named_inner_nx2 named_inner_ny3 ->
               HasCField.writeRaw (RIP.Proxy @"named_inner_nx") ptr0 named_inner_nx2
            >> HasCField.writeRaw (RIP.Proxy @"named_inner_ny") ptr0 named_inner_ny3

deriving via Marshal.EquivStorable Named_inner instance RIP.Storable Named_inner

instance HasCField.HasCField Named_inner "named_inner_nx" where

  type CFieldType Named_inner "named_inner_nx" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "named_inner_nx" (RIP.Ptr Named_inner) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_inner_nx")

instance HasCField.HasCField Named_inner "named_inner_ny" where

  type CFieldType Named_inner "named_inner_ny" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "named_inner_ny" (RIP.Ptr Named_inner) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_inner_ny")

{-| Struct with a named inner struct.

    Tests that doxygen comment enrichment uses the qualified name "named_outer::named_inner" for lookups.

    __C declaration:__ @struct named_outer@

    __defined at:__ @documentation\/doxygen_docs.h 606:8@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Named_outer = Named_outer
  { named_outer_inner_field :: Named_inner
    {- ^ __C declaration:__ @inner_field@

         __defined at:__ @documentation\/doxygen_docs.h 613:7@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , named_outer_nz :: RIP.CInt
    {- ^ Outer field nz

         __C declaration:__ @nz@

         __defined at:__ @documentation\/doxygen_docs.h 615:9@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Named_outer where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Named_outer where

  readRaw =
    \ptr0 ->
          pure Named_outer
      <*> HasCField.readRaw (RIP.Proxy @"named_outer_inner_field") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"named_outer_nz") ptr0

instance Marshal.WriteRaw Named_outer where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named_outer named_outer_inner_field2 named_outer_nz3 ->
               HasCField.writeRaw (RIP.Proxy @"named_outer_inner_field") ptr0 named_outer_inner_field2
            >> HasCField.writeRaw (RIP.Proxy @"named_outer_nz") ptr0 named_outer_nz3

deriving via Marshal.EquivStorable Named_outer instance RIP.Storable Named_outer

instance HasCField.HasCField Named_outer "named_outer_inner_field" where

  type CFieldType Named_outer "named_outer_inner_field" =
    Named_inner

  offset# = \_ -> \_ -> 0

instance ( ty ~ Named_inner
         ) => RIP.HasField "named_outer_inner_field" (RIP.Ptr Named_outer) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_outer_inner_field")

instance HasCField.HasCField Named_outer "named_outer_nz" where

  type CFieldType Named_outer "named_outer_nz" =
    RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "named_outer_nz" (RIP.Ptr Named_outer) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"named_outer_nz")

{-| __C declaration:__ @struct \@deep_mid_anon_field@

    __defined at:__ @documentation\/doxygen_docs.h 629:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Deep_mid_anon_field = Deep_mid_anon_field
  { deep_mid_anon_field_deep_a :: RIP.CInt
    {- ^ Deep anonymous field

         __C declaration:__ @deep_a@

         __defined at:__ @documentation\/doxygen_docs.h 631:17@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Deep_mid_anon_field where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Deep_mid_anon_field where

  readRaw =
    \ptr0 ->
          pure Deep_mid_anon_field
      <*> HasCField.readRaw (RIP.Proxy @"deep_mid_anon_field_deep_a") ptr0

instance Marshal.WriteRaw Deep_mid_anon_field where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Deep_mid_anon_field deep_mid_anon_field_deep_a2 ->
            HasCField.writeRaw (RIP.Proxy @"deep_mid_anon_field_deep_a") ptr0 deep_mid_anon_field_deep_a2

deriving via Marshal.EquivStorable Deep_mid_anon_field instance RIP.Storable Deep_mid_anon_field

instance HasCField.HasCField Deep_mid_anon_field "deep_mid_anon_field_deep_a" where

  type CFieldType Deep_mid_anon_field "deep_mid_anon_field_deep_a" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "deep_mid_anon_field_deep_a" (RIP.Ptr Deep_mid_anon_field) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"deep_mid_anon_field_deep_a")

{-| The named mid-level struct

    __C declaration:__ @struct deep_mid@

    __defined at:__ @documentation\/doxygen_docs.h 625:12@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Deep_mid = Deep_mid
  { deep_mid_m :: RIP.CInt
    {- ^ Mid-level field

         __C declaration:__ @m@

         __defined at:__ @documentation\/doxygen_docs.h 627:13@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , deep_mid_anon_field :: Deep_mid_anon_field
    {- ^ Anonymous struct inside named mid

         __C declaration:__ @anon_field@

         __defined at:__ @documentation\/doxygen_docs.h 632:11@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Deep_mid where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Deep_mid where

  readRaw =
    \ptr0 ->
          pure Deep_mid
      <*> HasCField.readRaw (RIP.Proxy @"deep_mid_m") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"deep_mid_anon_field") ptr0

instance Marshal.WriteRaw Deep_mid where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Deep_mid deep_mid_m2 deep_mid_anon_field3 ->
               HasCField.writeRaw (RIP.Proxy @"deep_mid_m") ptr0 deep_mid_m2
            >> HasCField.writeRaw (RIP.Proxy @"deep_mid_anon_field") ptr0 deep_mid_anon_field3

deriving via Marshal.EquivStorable Deep_mid instance RIP.Storable Deep_mid

instance HasCField.HasCField Deep_mid "deep_mid_m" where

  type CFieldType Deep_mid "deep_mid_m" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "deep_mid_m" (RIP.Ptr Deep_mid) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"deep_mid_m")

instance HasCField.HasCField Deep_mid "deep_mid_anon_field" where

  type CFieldType Deep_mid "deep_mid_anon_field" =
    Deep_mid_anon_field

  offset# = \_ -> \_ -> 4

instance ( ty ~ Deep_mid_anon_field
         ) => RIP.HasField "deep_mid_anon_field" (RIP.Ptr Deep_mid) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"deep_mid_anon_field")

{-| Deeply nested mix of named and anonymous structs.

    Tests the full genealogy walk through mixed named/anonymous nesting.

    __C declaration:__ @struct deep_outer@

    __defined at:__ @documentation\/doxygen_docs.h 623:8@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Deep_outer = Deep_outer
  { deep_outer_mid_field :: Deep_mid
    {- ^ __C declaration:__ @mid_field@

         __defined at:__ @documentation\/doxygen_docs.h 633:7@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , deep_outer_o :: RIP.CInt
    {- ^ Outer-only field

         __C declaration:__ @o@

         __defined at:__ @documentation\/doxygen_docs.h 635:9@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Deep_outer where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Deep_outer where

  readRaw =
    \ptr0 ->
          pure Deep_outer
      <*> HasCField.readRaw (RIP.Proxy @"deep_outer_mid_field") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"deep_outer_o") ptr0

instance Marshal.WriteRaw Deep_outer where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Deep_outer deep_outer_mid_field2 deep_outer_o3 ->
               HasCField.writeRaw (RIP.Proxy @"deep_outer_mid_field") ptr0 deep_outer_mid_field2
            >> HasCField.writeRaw (RIP.Proxy @"deep_outer_o") ptr0 deep_outer_o3

deriving via Marshal.EquivStorable Deep_outer instance RIP.Storable Deep_outer

instance HasCField.HasCField Deep_outer "deep_outer_mid_field" where

  type CFieldType Deep_outer "deep_outer_mid_field" =
    Deep_mid

  offset# = \_ -> \_ -> 0

instance ( ty ~ Deep_mid
         ) => RIP.HasField "deep_outer_mid_field" (RIP.Ptr Deep_outer) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"deep_outer_mid_field")

instance HasCField.HasCField Deep_outer "deep_outer_o" where

  type CFieldType Deep_outer "deep_outer_o" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "deep_outer_o" (RIP.Ptr Deep_outer) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"deep_outer_o")

{-| __C declaration:__ @struct \@unnamed_field_t_ua@

    __defined at:__ @documentation\/doxygen_docs.h 647:5@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Unnamed_field_t_ua = Unnamed_field_t_ua
  { unnamed_field_t_ua_ua :: RIP.CInt
    {- ^ Unnamed inner a

         __C declaration:__ @ua@

         __defined at:__ @documentation\/doxygen_docs.h 649:13@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , unnamed_field_t_ua_ub :: RIP.CInt
    {- ^ Unnamed inner b

         __C declaration:__ @ub@

         __defined at:__ @documentation\/doxygen_docs.h 651:13@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unnamed_field_t_ua where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Unnamed_field_t_ua where

  readRaw =
    \ptr0 ->
          pure Unnamed_field_t_ua
      <*> HasCField.readRaw (RIP.Proxy @"unnamed_field_t_ua_ua") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"unnamed_field_t_ua_ub") ptr0

instance Marshal.WriteRaw Unnamed_field_t_ua where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unnamed_field_t_ua unnamed_field_t_ua_ua2 unnamed_field_t_ua_ub3 ->
               HasCField.writeRaw (RIP.Proxy @"unnamed_field_t_ua_ua") ptr0 unnamed_field_t_ua_ua2
            >> HasCField.writeRaw (RIP.Proxy @"unnamed_field_t_ua_ub") ptr0 unnamed_field_t_ua_ub3

deriving via Marshal.EquivStorable Unnamed_field_t_ua instance RIP.Storable Unnamed_field_t_ua

instance HasCField.HasCField Unnamed_field_t_ua "unnamed_field_t_ua_ua" where

  type CFieldType Unnamed_field_t_ua "unnamed_field_t_ua_ua" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unnamed_field_t_ua_ua" (RIP.Ptr Unnamed_field_t_ua) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unnamed_field_t_ua_ua")

instance HasCField.HasCField Unnamed_field_t_ua "unnamed_field_t_ua_ub" where

  type CFieldType Unnamed_field_t_ua "unnamed_field_t_ua_ub" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unnamed_field_t_ua_ub" (RIP.Ptr Unnamed_field_t_ua) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unnamed_field_t_ua_ub")

{-| Struct with unnamed anonymous field.

    Tests the case where the anonymous inner struct has no field name. The inner fields are flattened directly into the parent.

    __C declaration:__ @struct unnamed_field_t@

    __defined at:__ @documentation\/doxygen_docs.h 644:9@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
data Unnamed_field_t = Unnamed_field_t
  { unnamed_field_t_before :: RIP.CInt
    {- ^ Before field

         __C declaration:__ @before@

         __defined at:__ @documentation\/doxygen_docs.h 646:9@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , unnamed_field_t_ua :: Unnamed_field_t_ua
    {- ^ Unnamed inner a

         __C declaration:__ @ua@

         __defined at:__ @documentation\/doxygen_docs.h 647:5@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  , unnamed_field_t_after :: RIP.CInt
    {- ^ After field

         __C declaration:__ @after@

         __defined at:__ @documentation\/doxygen_docs.h 654:9@

         __exported by:__ @documentation\/doxygen_docs.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Unnamed_field_t where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Unnamed_field_t where

  readRaw =
    \ptr0 ->
          pure Unnamed_field_t
      <*> HasCField.readRaw (RIP.Proxy @"unnamed_field_t_before") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"unnamed_field_t_ua") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"unnamed_field_t_after") ptr0

instance Marshal.WriteRaw Unnamed_field_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Unnamed_field_t
            unnamed_field_t_before2
            unnamed_field_t_ua3
            unnamed_field_t_after4 ->
                 HasCField.writeRaw (RIP.Proxy @"unnamed_field_t_before") ptr0 unnamed_field_t_before2
              >> HasCField.writeRaw (RIP.Proxy @"unnamed_field_t_ua") ptr0 unnamed_field_t_ua3
              >> HasCField.writeRaw (RIP.Proxy @"unnamed_field_t_after") ptr0 unnamed_field_t_after4

deriving via Marshal.EquivStorable Unnamed_field_t instance RIP.Storable Unnamed_field_t

instance HasCField.HasCField Unnamed_field_t "unnamed_field_t_before" where

  type CFieldType Unnamed_field_t "unnamed_field_t_before" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unnamed_field_t_before" (RIP.Ptr Unnamed_field_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unnamed_field_t_before")

instance HasCField.HasCField Unnamed_field_t "unnamed_field_t_ua" where

  type CFieldType Unnamed_field_t "unnamed_field_t_ua" =
    Unnamed_field_t_ua

  offset# = \_ -> \_ -> 4

instance ( ty ~ Unnamed_field_t_ua
         ) => RIP.HasField "unnamed_field_t_ua" (RIP.Ptr Unnamed_field_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unnamed_field_t_ua")

instance HasCField.HasCField Unnamed_field_t "unnamed_field_t_after" where

  type CFieldType Unnamed_field_t "unnamed_field_t_after" =
    RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unnamed_field_t_after" (RIP.Ptr Unnamed_field_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unnamed_field_t_after")

{-| API version number (not in any group).

    __C declaration:__ @api_version_t@

    __defined at:__ @documentation\/doxygen_docs.h 662:13@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
newtype Api_version_t = Api_version_t
  { unwrapApi_version_t :: RIP.CInt
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

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "unwrapApi_version_t" (RIP.Ptr Api_version_t) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapApi_version_t")

instance HasCField.HasCField Api_version_t "unwrapApi_version_t" where

  type CFieldType Api_version_t "unwrapApi_version_t" =
    RIP.CInt

  offset# = \_ -> \_ -> 0
