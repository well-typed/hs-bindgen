{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst

{-| __C declaration:__ @A@

    __defined at:__ @macros\/reparse.h 3:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype A = A
  { unwrapA :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct some_struct@

    __defined at:__ @macros\/reparse.h 7:8@

    __exported by:__ @macros\/reparse.h@
-}
data Some_struct = Some_struct
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Some_struct where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Some_struct where

  readRaw = \ptr0 -> pure Some_struct

instance Marshal.WriteRaw Some_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct -> return ()

deriving via Marshal.EquivStorable Some_struct instance RIP.Storable Some_struct

{-| __C declaration:__ @union some_union@

    __defined at:__ @macros\/reparse.h 8:7@

    __exported by:__ @macros\/reparse.h@
-}
newtype Some_union = Some_union
  { unwrapSome_union :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 0) 1 instance Marshal.StaticSize Some_union

deriving via (RIP.SizedByteArray 0) 1 instance Marshal.ReadRaw Some_union

deriving via (RIP.SizedByteArray 0) 1 instance Marshal.WriteRaw Some_union

deriving via Marshal.EquivStorable Some_union instance RIP.Storable Some_union

{-| __C declaration:__ @enum some_enum@

    __defined at:__ @macros\/reparse.h 9:6@

    __exported by:__ @macros\/reparse.h@
-}
newtype Some_enum = Some_enum
  { unwrapSome_enum :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Some_enum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Some_enum where

  readRaw =
    \ptr0 ->
          pure Some_enum
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Some_enum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_enum unwrapSome_enum2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapSome_enum2

deriving via Marshal.EquivStorable Some_enum instance RIP.Storable Some_enum

deriving via RIP.CUInt instance RIP.Prim Some_enum

instance CEnum.CEnum Some_enum where

  type CEnumZ Some_enum = RIP.CUInt

  toCEnum = Some_enum

  fromCEnum = unwrapSome_enum

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "ENUM_A")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Some_enum"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Some_enum"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Some_enum where

  minDeclaredValue = ENUM_A

  maxDeclaredValue = ENUM_A

instance Show Some_enum where

  showsPrec = CEnum.shows

instance Read Some_enum where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapSome_enum" (RIP.Ptr Some_enum) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapSome_enum")

instance HasCField.HasCField Some_enum "unwrapSome_enum" where

  type CFieldType Some_enum "unwrapSome_enum" =
    RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @ENUM_A@

    __defined at:__ @macros\/reparse.h 9:18@

    __exported by:__ @macros\/reparse.h@
-}
pattern ENUM_A :: Some_enum
pattern ENUM_A = Some_enum 0

{-| __C declaration:__ @arr_typedef1@

    __defined at:__ @macros\/reparse.h 109:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef1 = Arr_typedef1
  { unwrapArr_typedef1 :: IA.IncompleteArray A
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ((~) ty) (IA.IncompleteArray A)
         ) => RIP.HasField "unwrapArr_typedef1" (RIP.Ptr Arr_typedef1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapArr_typedef1")

instance HasCField.HasCField Arr_typedef1 "unwrapArr_typedef1" where

  type CFieldType Arr_typedef1 "unwrapArr_typedef1" =
    IA.IncompleteArray A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef2@

    __defined at:__ @macros\/reparse.h 110:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef2 = Arr_typedef2
  { unwrapArr_typedef2 :: IA.IncompleteArray (RIP.Ptr A)
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ((~) ty) (IA.IncompleteArray (RIP.Ptr A))
         ) => RIP.HasField "unwrapArr_typedef2" (RIP.Ptr Arr_typedef2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapArr_typedef2")

instance HasCField.HasCField Arr_typedef2 "unwrapArr_typedef2" where

  type CFieldType Arr_typedef2 "unwrapArr_typedef2" =
    IA.IncompleteArray (RIP.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef3@

    __defined at:__ @macros\/reparse.h 111:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef3 = Arr_typedef3
  { unwrapArr_typedef3 :: (CA.ConstantArray 5) A
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 5) A)
         ) => RIP.HasField "unwrapArr_typedef3" (RIP.Ptr Arr_typedef3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapArr_typedef3")

instance HasCField.HasCField Arr_typedef3 "unwrapArr_typedef3" where

  type CFieldType Arr_typedef3 "unwrapArr_typedef3" =
    (CA.ConstantArray 5) A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef4@

    __defined at:__ @macros\/reparse.h 112:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef4 = Arr_typedef4
  { unwrapArr_typedef4 :: (CA.ConstantArray 5) (RIP.Ptr A)
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 5) (RIP.Ptr A))
         ) => RIP.HasField "unwrapArr_typedef4" (RIP.Ptr Arr_typedef4) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapArr_typedef4")

instance HasCField.HasCField Arr_typedef4 "unwrapArr_typedef4" where

  type CFieldType Arr_typedef4 "unwrapArr_typedef4" =
    (CA.ConstantArray 5) (RIP.Ptr A)

  offset# = \_ -> \_ -> 0

{-| Typedefs

__C declaration:__ @typedef1@

__defined at:__ @macros\/reparse.h 118:14@

__exported by:__ @macros\/reparse.h@
-}
newtype Typedef1 = Typedef1
  { unwrapTypedef1 :: A
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

instance ( ((~) ty) A
         ) => RIP.HasField "unwrapTypedef1" (RIP.Ptr Typedef1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTypedef1")

instance HasCField.HasCField Typedef1 "unwrapTypedef1" where

  type CFieldType Typedef1 "unwrapTypedef1" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef2@

    __defined at:__ @macros\/reparse.h 119:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef2 = Typedef2
  { unwrapTypedef2 :: RIP.Ptr A
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr A)
         ) => RIP.HasField "unwrapTypedef2" (RIP.Ptr Typedef2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTypedef2")

instance HasCField.HasCField Typedef2 "unwrapTypedef2" where

  type CFieldType Typedef2 "unwrapTypedef2" = RIP.Ptr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef3@

    __defined at:__ @macros\/reparse.h 120:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef3 = Typedef3
  { unwrapTypedef3 :: RIP.Ptr (RIP.Ptr A)
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr (RIP.Ptr A))
         ) => RIP.HasField "unwrapTypedef3" (RIP.Ptr Typedef3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTypedef3")

instance HasCField.HasCField Typedef3 "unwrapTypedef3" where

  type CFieldType Typedef3 "unwrapTypedef3" =
    RIP.Ptr (RIP.Ptr A)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef1'

__C declaration:__ @funptr_typedef1@

__defined at:__ @macros\/reparse.h 132:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1_Aux = Funptr_typedef1_Aux
  { unwrapFunptr_typedef1_Aux :: IO A
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c584d0f839fd43de_base ::
     IO RIP.Int32
  -> IO (RIP.FunPtr (IO RIP.Int32))

-- __unique:__ @toFunptr_typedef1_Aux@
hs_bindgen_c584d0f839fd43de ::
     Funptr_typedef1_Aux
  -> IO (RIP.FunPtr Funptr_typedef1_Aux)
hs_bindgen_c584d0f839fd43de =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_c584d0f839fd43de_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_806a46dc418a062c_base ::
     RIP.FunPtr (IO RIP.Int32)
  -> IO RIP.Int32

-- __unique:__ @fromFunptr_typedef1_Aux@
hs_bindgen_806a46dc418a062c ::
     RIP.FunPtr Funptr_typedef1_Aux
  -> Funptr_typedef1_Aux
hs_bindgen_806a46dc418a062c =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_806a46dc418a062c_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Funptr_typedef1_Aux where

  toFunPtr = hs_bindgen_c584d0f839fd43de

instance RIP.FromFunPtr Funptr_typedef1_Aux where

  fromFunPtr = hs_bindgen_806a46dc418a062c

instance ( ((~) ty) (IO A)
         ) => RIP.HasField "unwrapFunptr_typedef1_Aux" (RIP.Ptr Funptr_typedef1_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef1_Aux")

instance HasCField.HasCField Funptr_typedef1_Aux "unwrapFunptr_typedef1_Aux" where

  type CFieldType Funptr_typedef1_Aux "unwrapFunptr_typedef1_Aux" =
    IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef1@

    __defined at:__ @macros\/reparse.h 132:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1 = Funptr_typedef1
  { unwrapFunptr_typedef1 :: RIP.FunPtr Funptr_typedef1_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Funptr_typedef1_Aux)
         ) => RIP.HasField "unwrapFunptr_typedef1" (RIP.Ptr Funptr_typedef1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef1")

instance HasCField.HasCField Funptr_typedef1 "unwrapFunptr_typedef1" where

  type CFieldType Funptr_typedef1 "unwrapFunptr_typedef1" =
    RIP.FunPtr Funptr_typedef1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef2'

__C declaration:__ @funptr_typedef2@

__defined at:__ @macros\/reparse.h 133:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2_Aux = Funptr_typedef2_Aux
  { unwrapFunptr_typedef2_Aux :: IO (RIP.Ptr A)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_f174457a161ac5a0_base ::
     IO (RIP.Ptr RIP.Void)
  -> IO (RIP.FunPtr (IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toFunptr_typedef2_Aux@
hs_bindgen_f174457a161ac5a0 ::
     Funptr_typedef2_Aux
  -> IO (RIP.FunPtr Funptr_typedef2_Aux)
hs_bindgen_f174457a161ac5a0 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_f174457a161ac5a0_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_323d07dff85b802c_base ::
     RIP.FunPtr (IO (RIP.Ptr RIP.Void))
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromFunptr_typedef2_Aux@
hs_bindgen_323d07dff85b802c ::
     RIP.FunPtr Funptr_typedef2_Aux
  -> Funptr_typedef2_Aux
hs_bindgen_323d07dff85b802c =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_323d07dff85b802c_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Funptr_typedef2_Aux where

  toFunPtr = hs_bindgen_f174457a161ac5a0

instance RIP.FromFunPtr Funptr_typedef2_Aux where

  fromFunPtr = hs_bindgen_323d07dff85b802c

instance ( ((~) ty) (IO (RIP.Ptr A))
         ) => RIP.HasField "unwrapFunptr_typedef2_Aux" (RIP.Ptr Funptr_typedef2_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef2_Aux")

instance HasCField.HasCField Funptr_typedef2_Aux "unwrapFunptr_typedef2_Aux" where

  type CFieldType Funptr_typedef2_Aux "unwrapFunptr_typedef2_Aux" =
    IO (RIP.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef2@

    __defined at:__ @macros\/reparse.h 133:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2 = Funptr_typedef2
  { unwrapFunptr_typedef2 :: RIP.FunPtr Funptr_typedef2_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Funptr_typedef2_Aux)
         ) => RIP.HasField "unwrapFunptr_typedef2" (RIP.Ptr Funptr_typedef2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef2")

instance HasCField.HasCField Funptr_typedef2 "unwrapFunptr_typedef2" where

  type CFieldType Funptr_typedef2 "unwrapFunptr_typedef2" =
    RIP.FunPtr Funptr_typedef2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef3'

__C declaration:__ @funptr_typedef3@

__defined at:__ @macros\/reparse.h 134:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3_Aux = Funptr_typedef3_Aux
  { unwrapFunptr_typedef3_Aux :: IO (RIP.Ptr (RIP.Ptr A))
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_031d1a7decd790d8_base ::
     IO (RIP.Ptr RIP.Void)
  -> IO (RIP.FunPtr (IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toFunptr_typedef3_Aux@
hs_bindgen_031d1a7decd790d8 ::
     Funptr_typedef3_Aux
  -> IO (RIP.FunPtr Funptr_typedef3_Aux)
hs_bindgen_031d1a7decd790d8 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_031d1a7decd790d8_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_82dc7b932974117e_base ::
     RIP.FunPtr (IO (RIP.Ptr RIP.Void))
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromFunptr_typedef3_Aux@
hs_bindgen_82dc7b932974117e ::
     RIP.FunPtr Funptr_typedef3_Aux
  -> Funptr_typedef3_Aux
hs_bindgen_82dc7b932974117e =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_82dc7b932974117e_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Funptr_typedef3_Aux where

  toFunPtr = hs_bindgen_031d1a7decd790d8

instance RIP.FromFunPtr Funptr_typedef3_Aux where

  fromFunPtr = hs_bindgen_82dc7b932974117e

instance ( ((~) ty) (IO (RIP.Ptr (RIP.Ptr A)))
         ) => RIP.HasField "unwrapFunptr_typedef3_Aux" (RIP.Ptr Funptr_typedef3_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef3_Aux")

instance HasCField.HasCField Funptr_typedef3_Aux "unwrapFunptr_typedef3_Aux" where

  type CFieldType Funptr_typedef3_Aux "unwrapFunptr_typedef3_Aux" =
    IO (RIP.Ptr (RIP.Ptr A))

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef3@

    __defined at:__ @macros\/reparse.h 134:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3 = Funptr_typedef3
  { unwrapFunptr_typedef3 :: RIP.FunPtr Funptr_typedef3_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Funptr_typedef3_Aux)
         ) => RIP.HasField "unwrapFunptr_typedef3" (RIP.Ptr Funptr_typedef3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef3")

instance HasCField.HasCField Funptr_typedef3 "unwrapFunptr_typedef3" where

  type CFieldType Funptr_typedef3 "unwrapFunptr_typedef3" =
    RIP.FunPtr Funptr_typedef3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef4'

__C declaration:__ @funptr_typedef4@

__defined at:__ @macros\/reparse.h 135:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4_Aux = Funptr_typedef4_Aux
  { unwrapFunptr_typedef4_Aux :: RIP.CInt -> RIP.CDouble -> IO A
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_da2336d254667386_base ::
     (RIP.Int32 -> Double -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO RIP.Int32))

-- __unique:__ @toFunptr_typedef4_Aux@
hs_bindgen_da2336d254667386 ::
     Funptr_typedef4_Aux
  -> IO (RIP.FunPtr Funptr_typedef4_Aux)
hs_bindgen_da2336d254667386 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_da2336d254667386_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_d4a97954476da161_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO RIP.Int32)
  -> RIP.Int32 -> Double -> IO RIP.Int32

-- __unique:__ @fromFunptr_typedef4_Aux@
hs_bindgen_d4a97954476da161 ::
     RIP.FunPtr Funptr_typedef4_Aux
  -> Funptr_typedef4_Aux
hs_bindgen_d4a97954476da161 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_d4a97954476da161_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Funptr_typedef4_Aux where

  toFunPtr = hs_bindgen_da2336d254667386

instance RIP.FromFunPtr Funptr_typedef4_Aux where

  fromFunPtr = hs_bindgen_d4a97954476da161

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO A)
         ) => RIP.HasField "unwrapFunptr_typedef4_Aux" (RIP.Ptr Funptr_typedef4_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef4_Aux")

instance HasCField.HasCField Funptr_typedef4_Aux "unwrapFunptr_typedef4_Aux" where

  type CFieldType Funptr_typedef4_Aux "unwrapFunptr_typedef4_Aux" =
    RIP.CInt -> RIP.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef4@

    __defined at:__ @macros\/reparse.h 135:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4 = Funptr_typedef4
  { unwrapFunptr_typedef4 :: RIP.FunPtr Funptr_typedef4_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Funptr_typedef4_Aux)
         ) => RIP.HasField "unwrapFunptr_typedef4" (RIP.Ptr Funptr_typedef4) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef4")

instance HasCField.HasCField Funptr_typedef4 "unwrapFunptr_typedef4" where

  type CFieldType Funptr_typedef4 "unwrapFunptr_typedef4" =
    RIP.FunPtr Funptr_typedef4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef5'

__C declaration:__ @funptr_typedef5@

__defined at:__ @macros\/reparse.h 136:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5_Aux = Funptr_typedef5_Aux
  { unwrapFunptr_typedef5_Aux :: RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr A)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_1f45632f07742a46_base ::
     (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toFunptr_typedef5_Aux@
hs_bindgen_1f45632f07742a46 ::
     Funptr_typedef5_Aux
  -> IO (RIP.FunPtr Funptr_typedef5_Aux)
hs_bindgen_1f45632f07742a46 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_1f45632f07742a46_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0bd1877eaaba0d3e_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromFunptr_typedef5_Aux@
hs_bindgen_0bd1877eaaba0d3e ::
     RIP.FunPtr Funptr_typedef5_Aux
  -> Funptr_typedef5_Aux
hs_bindgen_0bd1877eaaba0d3e =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_0bd1877eaaba0d3e_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Funptr_typedef5_Aux where

  toFunPtr = hs_bindgen_1f45632f07742a46

instance RIP.FromFunPtr Funptr_typedef5_Aux where

  fromFunPtr = hs_bindgen_0bd1877eaaba0d3e

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr A))
         ) => RIP.HasField "unwrapFunptr_typedef5_Aux" (RIP.Ptr Funptr_typedef5_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef5_Aux")

instance HasCField.HasCField Funptr_typedef5_Aux "unwrapFunptr_typedef5_Aux" where

  type CFieldType Funptr_typedef5_Aux "unwrapFunptr_typedef5_Aux" =
    RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef5@

    __defined at:__ @macros\/reparse.h 136:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5 = Funptr_typedef5
  { unwrapFunptr_typedef5 :: RIP.FunPtr Funptr_typedef5_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Funptr_typedef5_Aux)
         ) => RIP.HasField "unwrapFunptr_typedef5" (RIP.Ptr Funptr_typedef5) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunptr_typedef5")

instance HasCField.HasCField Funptr_typedef5 "unwrapFunptr_typedef5" where

  type CFieldType Funptr_typedef5 "unwrapFunptr_typedef5" =
    RIP.FunPtr Funptr_typedef5_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @comments2@

    __defined at:__ @macros\/reparse.h 145:30@

    __exported by:__ @macros\/reparse.h@
-}
newtype Comments2 = Comments2
  { unwrapComments2 :: A
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

instance ( ((~) ty) A
         ) => RIP.HasField "unwrapComments2" (RIP.Ptr Comments2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapComments2")

instance HasCField.HasCField Comments2 "unwrapComments2" where

  type CFieldType Comments2 "unwrapComments2" = A

  offset# = \_ -> \_ -> 0

{-| Struct fields

__C declaration:__ @struct example_struct@

__defined at:__ @macros\/reparse.h 151:8@

__exported by:__ @macros\/reparse.h@
-}
data Example_struct = Example_struct
  { example_struct_field1 :: A
    {- ^ __C declaration:__ @field1@

         __defined at:__ @macros\/reparse.h 152:8@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_field2 :: RIP.Ptr A
    {- ^ __C declaration:__ @field2@

         __defined at:__ @macros\/reparse.h 153:8@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_field3 :: RIP.Ptr (RIP.Ptr A)
    {- ^ __C declaration:__ @field3@

         __defined at:__ @macros\/reparse.h 154:8@

         __exported by:__ @macros\/reparse.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Example_struct where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Example_struct where

  readRaw =
    \ptr0 ->
          pure Example_struct
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_field1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_field2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_field3") ptr0

instance Marshal.WriteRaw Example_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct
            example_struct_field12
            example_struct_field23
            example_struct_field34 ->
                 HasCField.writeRaw (RIP.Proxy @"example_struct_field1") ptr0 example_struct_field12
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_field2") ptr0 example_struct_field23
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_field3") ptr0 example_struct_field34

deriving via Marshal.EquivStorable Example_struct instance RIP.Storable Example_struct

instance HasCField.HasCField Example_struct "example_struct_field1" where

  type CFieldType Example_struct "example_struct_field1" =
    A

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) A
         ) => RIP.HasField "example_struct_field1" (RIP.Ptr Example_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_field1")

instance HasCField.HasCField Example_struct "example_struct_field2" where

  type CFieldType Example_struct "example_struct_field2" =
    RIP.Ptr A

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr A)
         ) => RIP.HasField "example_struct_field2" (RIP.Ptr Example_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_field2")

instance HasCField.HasCField Example_struct "example_struct_field3" where

  type CFieldType Example_struct "example_struct_field3" =
    RIP.Ptr (RIP.Ptr A)

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) (RIP.Ptr (RIP.Ptr A))
         ) => RIP.HasField "example_struct_field3" (RIP.Ptr Example_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_field3")

{-| __C declaration:__ @const_typedef1@

    __defined at:__ @macros\/reparse.h 220:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef1 = Const_typedef1
  { unwrapConst_typedef1 :: A
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

instance ( ((~) ty) A
         ) => RIP.HasField "unwrapConst_typedef1" (RIP.Ptr Const_typedef1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_typedef1")

instance HasCField.HasCField Const_typedef1 "unwrapConst_typedef1" where

  type CFieldType Const_typedef1 "unwrapConst_typedef1" =
    A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef2@

    __defined at:__ @macros\/reparse.h 221:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef2 = Const_typedef2
  { unwrapConst_typedef2 :: A
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

instance ( ((~) ty) A
         ) => RIP.HasField "unwrapConst_typedef2" (RIP.Ptr Const_typedef2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_typedef2")

instance HasCField.HasCField Const_typedef2 "unwrapConst_typedef2" where

  type CFieldType Const_typedef2 "unwrapConst_typedef2" =
    A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef3@

    __defined at:__ @macros\/reparse.h 222:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef3 = Const_typedef3
  { unwrapConst_typedef3 :: PtrConst.PtrConst A
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "unwrapConst_typedef3" (RIP.Ptr Const_typedef3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_typedef3")

instance HasCField.HasCField Const_typedef3 "unwrapConst_typedef3" where

  type CFieldType Const_typedef3 "unwrapConst_typedef3" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef4@

    __defined at:__ @macros\/reparse.h 223:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef4 = Const_typedef4
  { unwrapConst_typedef4 :: PtrConst.PtrConst A
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "unwrapConst_typedef4" (RIP.Ptr Const_typedef4) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_typedef4")

instance HasCField.HasCField Const_typedef4 "unwrapConst_typedef4" where

  type CFieldType Const_typedef4 "unwrapConst_typedef4" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef5@

    __defined at:__ @macros\/reparse.h 224:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef5 = Const_typedef5
  { unwrapConst_typedef5 :: RIP.Ptr A
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr A)
         ) => RIP.HasField "unwrapConst_typedef5" (RIP.Ptr Const_typedef5) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_typedef5")

instance HasCField.HasCField Const_typedef5 "unwrapConst_typedef5" where

  type CFieldType Const_typedef5 "unwrapConst_typedef5" =
    RIP.Ptr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef6@

    __defined at:__ @macros\/reparse.h 225:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef6 = Const_typedef6
  { unwrapConst_typedef6 :: PtrConst.PtrConst A
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "unwrapConst_typedef6" (RIP.Ptr Const_typedef6) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_typedef6")

instance HasCField.HasCField Const_typedef6 "unwrapConst_typedef6" where

  type CFieldType Const_typedef6 "unwrapConst_typedef6" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef7@

    __defined at:__ @macros\/reparse.h 226:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef7 = Const_typedef7
  { unwrapConst_typedef7 :: PtrConst.PtrConst A
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "unwrapConst_typedef7" (RIP.Ptr Const_typedef7) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_typedef7")

instance HasCField.HasCField Const_typedef7 "unwrapConst_typedef7" where

  type CFieldType Const_typedef7 "unwrapConst_typedef7" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct example_struct_with_const@

    __defined at:__ @macros\/reparse.h 228:8@

    __exported by:__ @macros\/reparse.h@
-}
data Example_struct_with_const = Example_struct_with_const
  { example_struct_with_const_const_field1 :: A
    {- ^ __C declaration:__ @const_field1@

         __defined at:__ @macros\/reparse.h 229:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field2 :: A
    {- ^ __C declaration:__ @const_field2@

         __defined at:__ @macros\/reparse.h 230:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field3 :: PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field3@

         __defined at:__ @macros\/reparse.h 231:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field4 :: PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field4@

         __defined at:__ @macros\/reparse.h 232:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field5 :: RIP.Ptr A
    {- ^ __C declaration:__ @const_field5@

         __defined at:__ @macros\/reparse.h 233:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field6 :: PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field6@

         __defined at:__ @macros\/reparse.h 234:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field7 :: PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field7@

         __defined at:__ @macros\/reparse.h 235:19@

         __exported by:__ @macros\/reparse.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Example_struct_with_const where

  staticSizeOf = \_ -> (48 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Example_struct_with_const where

  readRaw =
    \ptr0 ->
          pure Example_struct_with_const
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_with_const_const_field1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_with_const_const_field2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_with_const_const_field3") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_with_const_const_field4") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_with_const_const_field5") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_with_const_const_field6") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"example_struct_with_const_const_field7") ptr0

instance Marshal.WriteRaw Example_struct_with_const where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct_with_const
            example_struct_with_const_const_field12
            example_struct_with_const_const_field23
            example_struct_with_const_const_field34
            example_struct_with_const_const_field45
            example_struct_with_const_const_field56
            example_struct_with_const_const_field67
            example_struct_with_const_const_field78 ->
                 HasCField.writeRaw (RIP.Proxy @"example_struct_with_const_const_field1") ptr0 example_struct_with_const_const_field12
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_with_const_const_field2") ptr0 example_struct_with_const_const_field23
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_with_const_const_field3") ptr0 example_struct_with_const_const_field34
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_with_const_const_field4") ptr0 example_struct_with_const_const_field45
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_with_const_const_field5") ptr0 example_struct_with_const_const_field56
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_with_const_const_field6") ptr0 example_struct_with_const_const_field67
              >> HasCField.writeRaw (RIP.Proxy @"example_struct_with_const_const_field7") ptr0 example_struct_with_const_const_field78

deriving via Marshal.EquivStorable Example_struct_with_const instance RIP.Storable Example_struct_with_const

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field1" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field1" =
    A

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) A
         ) => RIP.HasField "example_struct_with_const_const_field1" (RIP.Ptr Example_struct_with_const) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_with_const_const_field1")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field2" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field2" =
    A

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) A
         ) => RIP.HasField "example_struct_with_const_const_field2" (RIP.Ptr Example_struct_with_const) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_with_const_const_field2")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field3" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field3" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "example_struct_with_const_const_field3" (RIP.Ptr Example_struct_with_const) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_with_const_const_field3")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field4" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field4" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "example_struct_with_const_const_field4" (RIP.Ptr Example_struct_with_const) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_with_const_const_field4")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field5" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field5" =
    RIP.Ptr A

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) (RIP.Ptr A)
         ) => RIP.HasField "example_struct_with_const_const_field5" (RIP.Ptr Example_struct_with_const) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_with_const_const_field5")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field6" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field6" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "example_struct_with_const_const_field6" (RIP.Ptr Example_struct_with_const) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_with_const_const_field6")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field7" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field7" =
    PtrConst.PtrConst A

  offset# = \_ -> \_ -> 40

instance ( ((~) ty) (PtrConst.PtrConst A)
         ) => RIP.HasField "example_struct_with_const_const_field7" (RIP.Ptr Example_struct_with_const) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"example_struct_with_const_const_field7")

{-| Auxiliary type used by 'Const_funptr1'

__C declaration:__ @const_funptr1@

__defined at:__ @macros\/reparse.h 238:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1_Aux = Const_funptr1_Aux
  { unwrapConst_funptr1_Aux :: RIP.CInt -> RIP.CDouble -> IO A
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_7f125e20a9d4075b_base ::
     (RIP.Int32 -> Double -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO RIP.Int32))

-- __unique:__ @toConst_funptr1_Aux@
hs_bindgen_7f125e20a9d4075b ::
     Const_funptr1_Aux
  -> IO (RIP.FunPtr Const_funptr1_Aux)
hs_bindgen_7f125e20a9d4075b =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_7f125e20a9d4075b_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ac4bd8d789bba94b_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO RIP.Int32)
  -> RIP.Int32 -> Double -> IO RIP.Int32

-- __unique:__ @fromConst_funptr1_Aux@
hs_bindgen_ac4bd8d789bba94b ::
     RIP.FunPtr Const_funptr1_Aux
  -> Const_funptr1_Aux
hs_bindgen_ac4bd8d789bba94b =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_ac4bd8d789bba94b_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Const_funptr1_Aux where

  toFunPtr = hs_bindgen_7f125e20a9d4075b

instance RIP.FromFunPtr Const_funptr1_Aux where

  fromFunPtr = hs_bindgen_ac4bd8d789bba94b

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO A)
         ) => RIP.HasField "unwrapConst_funptr1_Aux" (RIP.Ptr Const_funptr1_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr1_Aux")

instance HasCField.HasCField Const_funptr1_Aux "unwrapConst_funptr1_Aux" where

  type CFieldType Const_funptr1_Aux "unwrapConst_funptr1_Aux" =
    RIP.CInt -> RIP.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr1@

    __defined at:__ @macros\/reparse.h 238:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1 = Const_funptr1
  { unwrapConst_funptr1 :: RIP.FunPtr Const_funptr1_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Const_funptr1_Aux)
         ) => RIP.HasField "unwrapConst_funptr1" (RIP.Ptr Const_funptr1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr1")

instance HasCField.HasCField Const_funptr1 "unwrapConst_funptr1" where

  type CFieldType Const_funptr1 "unwrapConst_funptr1" =
    RIP.FunPtr Const_funptr1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr2'

__C declaration:__ @const_funptr2@

__defined at:__ @macros\/reparse.h 239:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2_Aux = Const_funptr2_Aux
  { unwrapConst_funptr2_Aux :: RIP.CInt -> RIP.CDouble -> IO A
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c7b1e36d845634fb_base ::
     (RIP.Int32 -> Double -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO RIP.Int32))

-- __unique:__ @toConst_funptr2_Aux@
hs_bindgen_c7b1e36d845634fb ::
     Const_funptr2_Aux
  -> IO (RIP.FunPtr Const_funptr2_Aux)
hs_bindgen_c7b1e36d845634fb =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_c7b1e36d845634fb_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_352cebf463125ca9_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO RIP.Int32)
  -> RIP.Int32 -> Double -> IO RIP.Int32

-- __unique:__ @fromConst_funptr2_Aux@
hs_bindgen_352cebf463125ca9 ::
     RIP.FunPtr Const_funptr2_Aux
  -> Const_funptr2_Aux
hs_bindgen_352cebf463125ca9 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_352cebf463125ca9_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Const_funptr2_Aux where

  toFunPtr = hs_bindgen_c7b1e36d845634fb

instance RIP.FromFunPtr Const_funptr2_Aux where

  fromFunPtr = hs_bindgen_352cebf463125ca9

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO A)
         ) => RIP.HasField "unwrapConst_funptr2_Aux" (RIP.Ptr Const_funptr2_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr2_Aux")

instance HasCField.HasCField Const_funptr2_Aux "unwrapConst_funptr2_Aux" where

  type CFieldType Const_funptr2_Aux "unwrapConst_funptr2_Aux" =
    RIP.CInt -> RIP.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr2@

    __defined at:__ @macros\/reparse.h 239:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2 = Const_funptr2
  { unwrapConst_funptr2 :: RIP.FunPtr Const_funptr2_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Const_funptr2_Aux)
         ) => RIP.HasField "unwrapConst_funptr2" (RIP.Ptr Const_funptr2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr2")

instance HasCField.HasCField Const_funptr2 "unwrapConst_funptr2" where

  type CFieldType Const_funptr2 "unwrapConst_funptr2" =
    RIP.FunPtr Const_funptr2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr3'

__C declaration:__ @const_funptr3@

__defined at:__ @macros\/reparse.h 240:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3_Aux = Const_funptr3_Aux
  { unwrapConst_funptr3_Aux :: RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_2dcbfe1c2502178c_base ::
     (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toConst_funptr3_Aux@
hs_bindgen_2dcbfe1c2502178c ::
     Const_funptr3_Aux
  -> IO (RIP.FunPtr Const_funptr3_Aux)
hs_bindgen_2dcbfe1c2502178c =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_2dcbfe1c2502178c_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_86738dcfd7c9d33c_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromConst_funptr3_Aux@
hs_bindgen_86738dcfd7c9d33c ::
     RIP.FunPtr Const_funptr3_Aux
  -> Const_funptr3_Aux
hs_bindgen_86738dcfd7c9d33c =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_86738dcfd7c9d33c_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Const_funptr3_Aux where

  toFunPtr = hs_bindgen_2dcbfe1c2502178c

instance RIP.FromFunPtr Const_funptr3_Aux where

  fromFunPtr = hs_bindgen_86738dcfd7c9d33c

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A))
         ) => RIP.HasField "unwrapConst_funptr3_Aux" (RIP.Ptr Const_funptr3_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr3_Aux")

instance HasCField.HasCField Const_funptr3_Aux "unwrapConst_funptr3_Aux" where

  type CFieldType Const_funptr3_Aux "unwrapConst_funptr3_Aux" =
    RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr3@

    __defined at:__ @macros\/reparse.h 240:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3 = Const_funptr3
  { unwrapConst_funptr3 :: RIP.FunPtr Const_funptr3_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Const_funptr3_Aux)
         ) => RIP.HasField "unwrapConst_funptr3" (RIP.Ptr Const_funptr3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr3")

instance HasCField.HasCField Const_funptr3 "unwrapConst_funptr3" where

  type CFieldType Const_funptr3 "unwrapConst_funptr3" =
    RIP.FunPtr Const_funptr3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr4'

__C declaration:__ @const_funptr4@

__defined at:__ @macros\/reparse.h 241:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4_Aux = Const_funptr4_Aux
  { unwrapConst_funptr4_Aux :: RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_5461deeda491de0b_base ::
     (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toConst_funptr4_Aux@
hs_bindgen_5461deeda491de0b ::
     Const_funptr4_Aux
  -> IO (RIP.FunPtr Const_funptr4_Aux)
hs_bindgen_5461deeda491de0b =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_5461deeda491de0b_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_de7846fca3bfd1b6_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromConst_funptr4_Aux@
hs_bindgen_de7846fca3bfd1b6 ::
     RIP.FunPtr Const_funptr4_Aux
  -> Const_funptr4_Aux
hs_bindgen_de7846fca3bfd1b6 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_de7846fca3bfd1b6_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Const_funptr4_Aux where

  toFunPtr = hs_bindgen_5461deeda491de0b

instance RIP.FromFunPtr Const_funptr4_Aux where

  fromFunPtr = hs_bindgen_de7846fca3bfd1b6

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A))
         ) => RIP.HasField "unwrapConst_funptr4_Aux" (RIP.Ptr Const_funptr4_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr4_Aux")

instance HasCField.HasCField Const_funptr4_Aux "unwrapConst_funptr4_Aux" where

  type CFieldType Const_funptr4_Aux "unwrapConst_funptr4_Aux" =
    RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr4@

    __defined at:__ @macros\/reparse.h 241:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4 = Const_funptr4
  { unwrapConst_funptr4 :: RIP.FunPtr Const_funptr4_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Const_funptr4_Aux)
         ) => RIP.HasField "unwrapConst_funptr4" (RIP.Ptr Const_funptr4) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr4")

instance HasCField.HasCField Const_funptr4 "unwrapConst_funptr4" where

  type CFieldType Const_funptr4 "unwrapConst_funptr4" =
    RIP.FunPtr Const_funptr4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr5'

__C declaration:__ @const_funptr5@

__defined at:__ @macros\/reparse.h 242:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5_Aux = Const_funptr5_Aux
  { unwrapConst_funptr5_Aux :: RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr A)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_7b0174fc978a1ce1_base ::
     (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toConst_funptr5_Aux@
hs_bindgen_7b0174fc978a1ce1 ::
     Const_funptr5_Aux
  -> IO (RIP.FunPtr Const_funptr5_Aux)
hs_bindgen_7b0174fc978a1ce1 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_7b0174fc978a1ce1_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_38a21d84bb7115b5_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromConst_funptr5_Aux@
hs_bindgen_38a21d84bb7115b5 ::
     RIP.FunPtr Const_funptr5_Aux
  -> Const_funptr5_Aux
hs_bindgen_38a21d84bb7115b5 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_38a21d84bb7115b5_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Const_funptr5_Aux where

  toFunPtr = hs_bindgen_7b0174fc978a1ce1

instance RIP.FromFunPtr Const_funptr5_Aux where

  fromFunPtr = hs_bindgen_38a21d84bb7115b5

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr A))
         ) => RIP.HasField "unwrapConst_funptr5_Aux" (RIP.Ptr Const_funptr5_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr5_Aux")

instance HasCField.HasCField Const_funptr5_Aux "unwrapConst_funptr5_Aux" where

  type CFieldType Const_funptr5_Aux "unwrapConst_funptr5_Aux" =
    RIP.CInt -> RIP.CDouble -> IO (RIP.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr5@

    __defined at:__ @macros\/reparse.h 242:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5 = Const_funptr5
  { unwrapConst_funptr5 :: RIP.FunPtr Const_funptr5_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Const_funptr5_Aux)
         ) => RIP.HasField "unwrapConst_funptr5" (RIP.Ptr Const_funptr5) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr5")

instance HasCField.HasCField Const_funptr5 "unwrapConst_funptr5" where

  type CFieldType Const_funptr5 "unwrapConst_funptr5" =
    RIP.FunPtr Const_funptr5_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr6'

__C declaration:__ @const_funptr6@

__defined at:__ @macros\/reparse.h 243:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6_Aux = Const_funptr6_Aux
  { unwrapConst_funptr6_Aux :: RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_4e32721222f4df9f_base ::
     (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toConst_funptr6_Aux@
hs_bindgen_4e32721222f4df9f ::
     Const_funptr6_Aux
  -> IO (RIP.FunPtr Const_funptr6_Aux)
hs_bindgen_4e32721222f4df9f =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_4e32721222f4df9f_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_45251216b04aa8b5_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromConst_funptr6_Aux@
hs_bindgen_45251216b04aa8b5 ::
     RIP.FunPtr Const_funptr6_Aux
  -> Const_funptr6_Aux
hs_bindgen_45251216b04aa8b5 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_45251216b04aa8b5_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Const_funptr6_Aux where

  toFunPtr = hs_bindgen_4e32721222f4df9f

instance RIP.FromFunPtr Const_funptr6_Aux where

  fromFunPtr = hs_bindgen_45251216b04aa8b5

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A))
         ) => RIP.HasField "unwrapConst_funptr6_Aux" (RIP.Ptr Const_funptr6_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr6_Aux")

instance HasCField.HasCField Const_funptr6_Aux "unwrapConst_funptr6_Aux" where

  type CFieldType Const_funptr6_Aux "unwrapConst_funptr6_Aux" =
    RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr6@

    __defined at:__ @macros\/reparse.h 243:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6 = Const_funptr6
  { unwrapConst_funptr6 :: RIP.FunPtr Const_funptr6_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Const_funptr6_Aux)
         ) => RIP.HasField "unwrapConst_funptr6" (RIP.Ptr Const_funptr6) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr6")

instance HasCField.HasCField Const_funptr6 "unwrapConst_funptr6" where

  type CFieldType Const_funptr6 "unwrapConst_funptr6" =
    RIP.FunPtr Const_funptr6_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr7'

__C declaration:__ @const_funptr7@

__defined at:__ @macros\/reparse.h 244:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7_Aux = Const_funptr7_Aux
  { unwrapConst_funptr7_Aux :: RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_0d04fc96ffb9de06_base ::
     (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> IO (RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)))

-- __unique:__ @toConst_funptr7_Aux@
hs_bindgen_0d04fc96ffb9de06 ::
     Const_funptr7_Aux
  -> IO (RIP.FunPtr Const_funptr7_Aux)
hs_bindgen_0d04fc96ffb9de06 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_0d04fc96ffb9de06_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_42fbcebf75a973ba_base ::
     RIP.FunPtr (RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void))
  -> RIP.Int32 -> Double -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @fromConst_funptr7_Aux@
hs_bindgen_42fbcebf75a973ba ::
     RIP.FunPtr Const_funptr7_Aux
  -> Const_funptr7_Aux
hs_bindgen_42fbcebf75a973ba =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_42fbcebf75a973ba_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Const_funptr7_Aux where

  toFunPtr = hs_bindgen_0d04fc96ffb9de06

instance RIP.FromFunPtr Const_funptr7_Aux where

  fromFunPtr = hs_bindgen_42fbcebf75a973ba

instance ( ((~) ty) (RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A))
         ) => RIP.HasField "unwrapConst_funptr7_Aux" (RIP.Ptr Const_funptr7_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr7_Aux")

instance HasCField.HasCField Const_funptr7_Aux "unwrapConst_funptr7_Aux" where

  type CFieldType Const_funptr7_Aux "unwrapConst_funptr7_Aux" =
    RIP.CInt -> RIP.CDouble -> IO (PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr7@

    __defined at:__ @macros\/reparse.h 244:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7 = Const_funptr7
  { unwrapConst_funptr7 :: RIP.FunPtr Const_funptr7_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Const_funptr7_Aux)
         ) => RIP.HasField "unwrapConst_funptr7" (RIP.Ptr Const_funptr7) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapConst_funptr7")

instance HasCField.HasCField Const_funptr7 "unwrapConst_funptr7" where

  type CFieldType Const_funptr7 "unwrapConst_funptr7" =
    RIP.FunPtr Const_funptr7_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BOOL@

    __defined at:__ @macros\/reparse.h 280:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype BOOL = BOOL
  { unwrapBOOL :: RIP.CBool
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

instance ( ((~) ty) RIP.CBool
         ) => RIP.HasField "unwrapBOOL" (RIP.Ptr BOOL) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapBOOL")

instance HasCField.HasCField BOOL "unwrapBOOL" where

  type CFieldType BOOL "unwrapBOOL" = RIP.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INT@

    __defined at:__ @macros\/reparse.h 281:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INT = INT
  { unwrapINT :: RIP.CInt
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

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrapINT" (RIP.Ptr INT) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapINT")

instance HasCField.HasCField INT "unwrapINT" where

  type CFieldType INT "unwrapINT" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INTP@

    __defined at:__ @macros\/reparse.h 282:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INTP = INTP
  { unwrapINTP :: RIP.Ptr RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.Ptr RIP.CInt)
         ) => RIP.HasField "unwrapINTP" (RIP.Ptr INTP) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapINTP")

instance HasCField.HasCField INTP "unwrapINTP" where

  type CFieldType INTP "unwrapINTP" = RIP.Ptr RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INTCP@

    __defined at:__ @macros\/reparse.h 283:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INTCP = INTCP
  { unwrapINTCP :: PtrConst.PtrConst RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (PtrConst.PtrConst RIP.CInt)
         ) => RIP.HasField "unwrapINTCP" (RIP.Ptr INTCP) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapINTCP")

instance HasCField.HasCField INTCP "unwrapINTCP" where

  type CFieldType INTCP "unwrapINTCP" =
    PtrConst.PtrConst RIP.CInt

  offset# = \_ -> \_ -> 0
