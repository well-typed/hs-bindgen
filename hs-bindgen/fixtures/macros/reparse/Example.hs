{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Marshal
import qualified HsBindgen.Runtime.PtrConst
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Prelude as P
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Double, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return, showsPrec)

{-| __C declaration:__ @A@

    __defined at:__ @macros\/reparse.h 3:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype A = A
  { unwrapA :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "unwrapA")
         ) => GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct some_struct@

    __defined at:__ @macros\/reparse.h 7:8@

    __exported by:__ @macros\/reparse.h@
-}
data Some_struct = Some_struct
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Some_struct where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Some_struct where

  readRaw = \ptr0 -> pure Some_struct

instance HsBindgen.Runtime.Marshal.WriteRaw Some_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Some_struct instance F.Storable Some_struct

instance Data.Primitive.Types.Prim Some_struct where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

  indexByteArray# = \arr0 -> \i1 -> Some_struct

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Some_struct #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Some_struct -> s3

  indexOffAddr# = \addr0 -> \i1 -> Some_struct

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Some_struct #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Some_struct -> s3

{-| __C declaration:__ @union some_union@

    __defined at:__ @macros\/reparse.h 8:7@

    __exported by:__ @macros\/reparse.h@
-}
newtype Some_union = Some_union
  { unwrapSome_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance HsBindgen.Runtime.Marshal.StaticSize Some_union

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance HsBindgen.Runtime.Marshal.ReadRaw Some_union

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance HsBindgen.Runtime.Marshal.WriteRaw Some_union

deriving via HsBindgen.Runtime.Marshal.EquivStorable Some_union instance F.Storable Some_union

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance Data.Primitive.Types.Prim Some_union

{-| __C declaration:__ @enum some_enum@

    __defined at:__ @macros\/reparse.h 9:6@

    __exported by:__ @macros\/reparse.h@
-}
newtype Some_enum = Some_enum
  { unwrapSome_enum :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Some_enum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Some_enum where

  readRaw =
    \ptr0 ->
          pure Some_enum
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Some_enum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_enum unwrapSome_enum2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapSome_enum2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Some_enum instance F.Storable Some_enum

deriving via FC.CUInt instance Data.Primitive.Types.Prim Some_enum

instance HsBindgen.Runtime.CEnum.CEnum Some_enum where

  type CEnumZ Some_enum = FC.CUInt

  toCEnum = Some_enum

  fromCEnum = unwrapSome_enum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "ENUM_A")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Some_enum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Some_enum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Some_enum where

  minDeclaredValue = ENUM_A

  maxDeclaredValue = ENUM_A

instance Show Some_enum where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Some_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Some_enum) "unwrapSome_enum")
         ) => GHC.Records.HasField "unwrapSome_enum" (Ptr.Ptr Some_enum) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapSome_enum")

instance HsBindgen.Runtime.HasCField.HasCField Some_enum "unwrapSome_enum" where

  type CFieldType Some_enum "unwrapSome_enum" =
    FC.CUInt

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
  { unwrapArr_typedef1 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray A
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef1) "unwrapArr_typedef1")
         ) => GHC.Records.HasField "unwrapArr_typedef1" (Ptr.Ptr Arr_typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapArr_typedef1")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef1 "unwrapArr_typedef1" where

  type CFieldType Arr_typedef1 "unwrapArr_typedef1" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef2@

    __defined at:__ @macros\/reparse.h 110:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef2 = Arr_typedef2
  { unwrapArr_typedef2 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef2) "unwrapArr_typedef2")
         ) => GHC.Records.HasField "unwrapArr_typedef2" (Ptr.Ptr Arr_typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapArr_typedef2")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef2 "unwrapArr_typedef2" where

  type CFieldType Arr_typedef2 "unwrapArr_typedef2" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef3@

    __defined at:__ @macros\/reparse.h 111:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef3 = Arr_typedef3
  { unwrapArr_typedef3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) A
  }
  deriving stock (Eq, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef3) "unwrapArr_typedef3")
         ) => GHC.Records.HasField "unwrapArr_typedef3" (Ptr.Ptr Arr_typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapArr_typedef3")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef3 "unwrapArr_typedef3" where

  type CFieldType Arr_typedef3 "unwrapArr_typedef3" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 5) A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef4@

    __defined at:__ @macros\/reparse.h 112:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef4 = Arr_typedef4
  { unwrapArr_typedef4 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef4) "unwrapArr_typedef4")
         ) => GHC.Records.HasField "unwrapArr_typedef4" (Ptr.Ptr Arr_typedef4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapArr_typedef4")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef4 "unwrapArr_typedef4" where

  type CFieldType Arr_typedef4 "unwrapArr_typedef4" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| Typedefs

__C declaration:__ @typedef1@

__defined at:__ @macros\/reparse.h 118:14@

__exported by:__ @macros\/reparse.h@
-}
newtype Typedef1 = Typedef1
  { unwrapTypedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Typedef1) "unwrapTypedef1")
         ) => GHC.Records.HasField "unwrapTypedef1" (Ptr.Ptr Typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTypedef1")

instance HsBindgen.Runtime.HasCField.HasCField Typedef1 "unwrapTypedef1" where

  type CFieldType Typedef1 "unwrapTypedef1" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef2@

    __defined at:__ @macros\/reparse.h 119:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef2 = Typedef2
  { unwrapTypedef2 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Typedef2) "unwrapTypedef2")
         ) => GHC.Records.HasField "unwrapTypedef2" (Ptr.Ptr Typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTypedef2")

instance HsBindgen.Runtime.HasCField.HasCField Typedef2 "unwrapTypedef2" where

  type CFieldType Typedef2 "unwrapTypedef2" = Ptr.Ptr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef3@

    __defined at:__ @macros\/reparse.h 120:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef3 = Typedef3
  { unwrapTypedef3 :: Ptr.Ptr (Ptr.Ptr A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Typedef3) "unwrapTypedef3")
         ) => GHC.Records.HasField "unwrapTypedef3" (Ptr.Ptr Typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTypedef3")

instance HsBindgen.Runtime.HasCField.HasCField Typedef3 "unwrapTypedef3" where

  type CFieldType Typedef3 "unwrapTypedef3" =
    Ptr.Ptr (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef1'

__C declaration:__ @funptr_typedef1@

__defined at:__ @macros\/reparse.h 132:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1_Aux = Funptr_typedef1_Aux
  { unwrapFunptr_typedef1_Aux :: IO A
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c584d0f839fd43de_base ::
     IO GHC.Int.Int32
  -> IO (Ptr.FunPtr (IO GHC.Int.Int32))

-- __unique:__ @toFunptr_typedef1_Aux@
hs_bindgen_c584d0f839fd43de ::
     Funptr_typedef1_Aux
  -> IO (Ptr.FunPtr Funptr_typedef1_Aux)
hs_bindgen_c584d0f839fd43de =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_c584d0f839fd43de_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_806a46dc418a062c_base ::
     Ptr.FunPtr (IO GHC.Int.Int32)
  -> IO GHC.Int.Int32

-- __unique:__ @fromFunptr_typedef1_Aux@
hs_bindgen_806a46dc418a062c ::
     Ptr.FunPtr Funptr_typedef1_Aux
  -> Funptr_typedef1_Aux
hs_bindgen_806a46dc418a062c =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_806a46dc418a062c_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef1_Aux where

  toFunPtr = hs_bindgen_c584d0f839fd43de

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef1_Aux where

  fromFunPtr = hs_bindgen_806a46dc418a062c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef1_Aux) "unwrapFunptr_typedef1_Aux")
         ) => GHC.Records.HasField "unwrapFunptr_typedef1_Aux" (Ptr.Ptr Funptr_typedef1_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef1_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef1_Aux "unwrapFunptr_typedef1_Aux" where

  type CFieldType Funptr_typedef1_Aux "unwrapFunptr_typedef1_Aux" =
    IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef1@

    __defined at:__ @macros\/reparse.h 132:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1 = Funptr_typedef1
  { unwrapFunptr_typedef1 :: Ptr.FunPtr Funptr_typedef1_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef1) "unwrapFunptr_typedef1")
         ) => GHC.Records.HasField "unwrapFunptr_typedef1" (Ptr.Ptr Funptr_typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef1")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef1 "unwrapFunptr_typedef1" where

  type CFieldType Funptr_typedef1 "unwrapFunptr_typedef1" =
    Ptr.FunPtr Funptr_typedef1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef2'

__C declaration:__ @funptr_typedef2@

__defined at:__ @macros\/reparse.h 133:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2_Aux = Funptr_typedef2_Aux
  { unwrapFunptr_typedef2_Aux :: IO (Ptr.Ptr A)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_f174457a161ac5a0_base ::
     IO (Ptr.Ptr Void)
  -> IO (Ptr.FunPtr (IO (Ptr.Ptr Void)))

-- __unique:__ @toFunptr_typedef2_Aux@
hs_bindgen_f174457a161ac5a0 ::
     Funptr_typedef2_Aux
  -> IO (Ptr.FunPtr Funptr_typedef2_Aux)
hs_bindgen_f174457a161ac5a0 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_f174457a161ac5a0_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_323d07dff85b802c_base ::
     Ptr.FunPtr (IO (Ptr.Ptr Void))
  -> IO (Ptr.Ptr Void)

-- __unique:__ @fromFunptr_typedef2_Aux@
hs_bindgen_323d07dff85b802c ::
     Ptr.FunPtr Funptr_typedef2_Aux
  -> Funptr_typedef2_Aux
hs_bindgen_323d07dff85b802c =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_323d07dff85b802c_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef2_Aux where

  toFunPtr = hs_bindgen_f174457a161ac5a0

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef2_Aux where

  fromFunPtr = hs_bindgen_323d07dff85b802c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef2_Aux) "unwrapFunptr_typedef2_Aux")
         ) => GHC.Records.HasField "unwrapFunptr_typedef2_Aux" (Ptr.Ptr Funptr_typedef2_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef2_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef2_Aux "unwrapFunptr_typedef2_Aux" where

  type CFieldType Funptr_typedef2_Aux "unwrapFunptr_typedef2_Aux" =
    IO (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef2@

    __defined at:__ @macros\/reparse.h 133:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2 = Funptr_typedef2
  { unwrapFunptr_typedef2 :: Ptr.FunPtr Funptr_typedef2_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef2) "unwrapFunptr_typedef2")
         ) => GHC.Records.HasField "unwrapFunptr_typedef2" (Ptr.Ptr Funptr_typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef2")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef2 "unwrapFunptr_typedef2" where

  type CFieldType Funptr_typedef2 "unwrapFunptr_typedef2" =
    Ptr.FunPtr Funptr_typedef2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef3'

__C declaration:__ @funptr_typedef3@

__defined at:__ @macros\/reparse.h 134:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3_Aux = Funptr_typedef3_Aux
  { unwrapFunptr_typedef3_Aux :: IO (Ptr.Ptr (Ptr.Ptr A))
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_031d1a7decd790d8_base ::
     IO (Ptr.Ptr Void)
  -> IO (Ptr.FunPtr (IO (Ptr.Ptr Void)))

-- __unique:__ @toFunptr_typedef3_Aux@
hs_bindgen_031d1a7decd790d8 ::
     Funptr_typedef3_Aux
  -> IO (Ptr.FunPtr Funptr_typedef3_Aux)
hs_bindgen_031d1a7decd790d8 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_031d1a7decd790d8_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_82dc7b932974117e_base ::
     Ptr.FunPtr (IO (Ptr.Ptr Void))
  -> IO (Ptr.Ptr Void)

-- __unique:__ @fromFunptr_typedef3_Aux@
hs_bindgen_82dc7b932974117e ::
     Ptr.FunPtr Funptr_typedef3_Aux
  -> Funptr_typedef3_Aux
hs_bindgen_82dc7b932974117e =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_82dc7b932974117e_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef3_Aux where

  toFunPtr = hs_bindgen_031d1a7decd790d8

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef3_Aux where

  fromFunPtr = hs_bindgen_82dc7b932974117e

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef3_Aux) "unwrapFunptr_typedef3_Aux")
         ) => GHC.Records.HasField "unwrapFunptr_typedef3_Aux" (Ptr.Ptr Funptr_typedef3_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef3_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef3_Aux "unwrapFunptr_typedef3_Aux" where

  type CFieldType Funptr_typedef3_Aux "unwrapFunptr_typedef3_Aux" =
    IO (Ptr.Ptr (Ptr.Ptr A))

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef3@

    __defined at:__ @macros\/reparse.h 134:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3 = Funptr_typedef3
  { unwrapFunptr_typedef3 :: Ptr.FunPtr Funptr_typedef3_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef3) "unwrapFunptr_typedef3")
         ) => GHC.Records.HasField "unwrapFunptr_typedef3" (Ptr.Ptr Funptr_typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef3")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef3 "unwrapFunptr_typedef3" where

  type CFieldType Funptr_typedef3 "unwrapFunptr_typedef3" =
    Ptr.FunPtr Funptr_typedef3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef4'

__C declaration:__ @funptr_typedef4@

__defined at:__ @macros\/reparse.h 135:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4_Aux = Funptr_typedef4_Aux
  { unwrapFunptr_typedef4_Aux :: FC.CInt -> FC.CDouble -> IO A
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_da2336d254667386_base ::
     (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32))

-- __unique:__ @toFunptr_typedef4_Aux@
hs_bindgen_da2336d254667386 ::
     Funptr_typedef4_Aux
  -> IO (Ptr.FunPtr Funptr_typedef4_Aux)
hs_bindgen_da2336d254667386 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_da2336d254667386_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_d4a97954476da161_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> Double -> IO GHC.Int.Int32

-- __unique:__ @fromFunptr_typedef4_Aux@
hs_bindgen_d4a97954476da161 ::
     Ptr.FunPtr Funptr_typedef4_Aux
  -> Funptr_typedef4_Aux
hs_bindgen_d4a97954476da161 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_d4a97954476da161_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef4_Aux where

  toFunPtr = hs_bindgen_da2336d254667386

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef4_Aux where

  fromFunPtr = hs_bindgen_d4a97954476da161

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef4_Aux) "unwrapFunptr_typedef4_Aux")
         ) => GHC.Records.HasField "unwrapFunptr_typedef4_Aux" (Ptr.Ptr Funptr_typedef4_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef4_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef4_Aux "unwrapFunptr_typedef4_Aux" where

  type CFieldType Funptr_typedef4_Aux "unwrapFunptr_typedef4_Aux" =
    FC.CInt -> FC.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef4@

    __defined at:__ @macros\/reparse.h 135:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4 = Funptr_typedef4
  { unwrapFunptr_typedef4 :: Ptr.FunPtr Funptr_typedef4_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef4) "unwrapFunptr_typedef4")
         ) => GHC.Records.HasField "unwrapFunptr_typedef4" (Ptr.Ptr Funptr_typedef4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef4")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef4 "unwrapFunptr_typedef4" where

  type CFieldType Funptr_typedef4 "unwrapFunptr_typedef4" =
    Ptr.FunPtr Funptr_typedef4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef5'

__C declaration:__ @funptr_typedef5@

__defined at:__ @macros\/reparse.h 136:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5_Aux = Funptr_typedef5_Aux
  { unwrapFunptr_typedef5_Aux :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_1f45632f07742a46_base ::
     (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)))

-- __unique:__ @toFunptr_typedef5_Aux@
hs_bindgen_1f45632f07742a46 ::
     Funptr_typedef5_Aux
  -> IO (Ptr.FunPtr Funptr_typedef5_Aux)
hs_bindgen_1f45632f07742a46 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_1f45632f07742a46_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0bd1877eaaba0d3e_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)

-- __unique:__ @fromFunptr_typedef5_Aux@
hs_bindgen_0bd1877eaaba0d3e ::
     Ptr.FunPtr Funptr_typedef5_Aux
  -> Funptr_typedef5_Aux
hs_bindgen_0bd1877eaaba0d3e =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_0bd1877eaaba0d3e_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef5_Aux where

  toFunPtr = hs_bindgen_1f45632f07742a46

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef5_Aux where

  fromFunPtr = hs_bindgen_0bd1877eaaba0d3e

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef5_Aux) "unwrapFunptr_typedef5_Aux")
         ) => GHC.Records.HasField "unwrapFunptr_typedef5_Aux" (Ptr.Ptr Funptr_typedef5_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef5_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef5_Aux "unwrapFunptr_typedef5_Aux" where

  type CFieldType Funptr_typedef5_Aux "unwrapFunptr_typedef5_Aux" =
    FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef5@

    __defined at:__ @macros\/reparse.h 136:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5 = Funptr_typedef5
  { unwrapFunptr_typedef5 :: Ptr.FunPtr Funptr_typedef5_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef5) "unwrapFunptr_typedef5")
         ) => GHC.Records.HasField "unwrapFunptr_typedef5" (Ptr.Ptr Funptr_typedef5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFunptr_typedef5")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef5 "unwrapFunptr_typedef5" where

  type CFieldType Funptr_typedef5 "unwrapFunptr_typedef5" =
    Ptr.FunPtr Funptr_typedef5_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @comments2@

    __defined at:__ @macros\/reparse.h 145:30@

    __exported by:__ @macros\/reparse.h@
-}
newtype Comments2 = Comments2
  { unwrapComments2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Comments2) "unwrapComments2")
         ) => GHC.Records.HasField "unwrapComments2" (Ptr.Ptr Comments2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapComments2")

instance HsBindgen.Runtime.HasCField.HasCField Comments2 "unwrapComments2" where

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
  , example_struct_field2 :: Ptr.Ptr A
    {- ^ __C declaration:__ @field2@

         __defined at:__ @macros\/reparse.h 153:8@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_field3 :: Ptr.Ptr (Ptr.Ptr A)
    {- ^ __C declaration:__ @field3@

         __defined at:__ @macros\/reparse.h 154:8@

         __exported by:__ @macros\/reparse.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Example_struct where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Example_struct where

  readRaw =
    \ptr0 ->
          pure Example_struct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_field1") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_field2") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_field3") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Example_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct
            example_struct_field12
            example_struct_field23
            example_struct_field34 ->
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_field1") ptr0 example_struct_field12
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_field2") ptr0 example_struct_field23
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_field3") ptr0 example_struct_field34

deriving via HsBindgen.Runtime.Marshal.EquivStorable Example_struct instance F.Storable Example_struct

instance HsBindgen.Runtime.HasCField.HasCField Example_struct "example_struct_field1" where

  type CFieldType Example_struct "example_struct_field1" =
    A

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct) "example_struct_field1")
         ) => GHC.Records.HasField "example_struct_field1" (Ptr.Ptr Example_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_field1")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct "example_struct_field2" where

  type CFieldType Example_struct "example_struct_field2" =
    Ptr.Ptr A

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct) "example_struct_field2")
         ) => GHC.Records.HasField "example_struct_field2" (Ptr.Ptr Example_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_field2")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct "example_struct_field3" where

  type CFieldType Example_struct "example_struct_field3" =
    Ptr.Ptr (Ptr.Ptr A)

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct) "example_struct_field3")
         ) => GHC.Records.HasField "example_struct_field3" (Ptr.Ptr Example_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_field3")

{-| __C declaration:__ @const_typedef1@

    __defined at:__ @macros\/reparse.h 220:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef1 = Const_typedef1
  { unwrapConst_typedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef1) "unwrapConst_typedef1")
         ) => GHC.Records.HasField "unwrapConst_typedef1" (Ptr.Ptr Const_typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_typedef1")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef1 "unwrapConst_typedef1" where

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
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef2) "unwrapConst_typedef2")
         ) => GHC.Records.HasField "unwrapConst_typedef2" (Ptr.Ptr Const_typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_typedef2")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef2 "unwrapConst_typedef2" where

  type CFieldType Const_typedef2 "unwrapConst_typedef2" =
    A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef3@

    __defined at:__ @macros\/reparse.h 222:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef3 = Const_typedef3
  { unwrapConst_typedef3 :: HsBindgen.Runtime.PtrConst.PtrConst A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef3) "unwrapConst_typedef3")
         ) => GHC.Records.HasField "unwrapConst_typedef3" (Ptr.Ptr Const_typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_typedef3")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef3 "unwrapConst_typedef3" where

  type CFieldType Const_typedef3 "unwrapConst_typedef3" =
    HsBindgen.Runtime.PtrConst.PtrConst A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef4@

    __defined at:__ @macros\/reparse.h 223:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef4 = Const_typedef4
  { unwrapConst_typedef4 :: HsBindgen.Runtime.PtrConst.PtrConst A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef4) "unwrapConst_typedef4")
         ) => GHC.Records.HasField "unwrapConst_typedef4" (Ptr.Ptr Const_typedef4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_typedef4")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef4 "unwrapConst_typedef4" where

  type CFieldType Const_typedef4 "unwrapConst_typedef4" =
    HsBindgen.Runtime.PtrConst.PtrConst A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef5@

    __defined at:__ @macros\/reparse.h 224:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef5 = Const_typedef5
  { unwrapConst_typedef5 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef5) "unwrapConst_typedef5")
         ) => GHC.Records.HasField "unwrapConst_typedef5" (Ptr.Ptr Const_typedef5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_typedef5")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef5 "unwrapConst_typedef5" where

  type CFieldType Const_typedef5 "unwrapConst_typedef5" =
    Ptr.Ptr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef6@

    __defined at:__ @macros\/reparse.h 225:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef6 = Const_typedef6
  { unwrapConst_typedef6 :: HsBindgen.Runtime.PtrConst.PtrConst A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef6) "unwrapConst_typedef6")
         ) => GHC.Records.HasField "unwrapConst_typedef6" (Ptr.Ptr Const_typedef6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_typedef6")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef6 "unwrapConst_typedef6" where

  type CFieldType Const_typedef6 "unwrapConst_typedef6" =
    HsBindgen.Runtime.PtrConst.PtrConst A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef7@

    __defined at:__ @macros\/reparse.h 226:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef7 = Const_typedef7
  { unwrapConst_typedef7 :: HsBindgen.Runtime.PtrConst.PtrConst A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef7) "unwrapConst_typedef7")
         ) => GHC.Records.HasField "unwrapConst_typedef7" (Ptr.Ptr Const_typedef7) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_typedef7")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef7 "unwrapConst_typedef7" where

  type CFieldType Const_typedef7 "unwrapConst_typedef7" =
    HsBindgen.Runtime.PtrConst.PtrConst A

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
  , example_struct_with_const_const_field3 :: HsBindgen.Runtime.PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field3@

         __defined at:__ @macros\/reparse.h 231:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field4 :: HsBindgen.Runtime.PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field4@

         __defined at:__ @macros\/reparse.h 232:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field5 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field5@

         __defined at:__ @macros\/reparse.h 233:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field6 :: HsBindgen.Runtime.PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field6@

         __defined at:__ @macros\/reparse.h 234:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field7 :: HsBindgen.Runtime.PtrConst.PtrConst A
    {- ^ __C declaration:__ @const_field7@

         __defined at:__ @macros\/reparse.h 235:19@

         __exported by:__ @macros\/reparse.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Example_struct_with_const where

  staticSizeOf = \_ -> (48 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Example_struct_with_const where

  readRaw =
    \ptr0 ->
          pure Example_struct_with_const
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field1") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field2") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field3") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field4") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field5") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field6") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field7") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Example_struct_with_const where

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
                 HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field1") ptr0 example_struct_with_const_const_field12
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field2") ptr0 example_struct_with_const_const_field23
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field3") ptr0 example_struct_with_const_const_field34
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field4") ptr0 example_struct_with_const_const_field45
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field5") ptr0 example_struct_with_const_const_field56
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field6") ptr0 example_struct_with_const_const_field67
              >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"example_struct_with_const_const_field7") ptr0 example_struct_with_const_const_field78

deriving via HsBindgen.Runtime.Marshal.EquivStorable Example_struct_with_const instance F.Storable Example_struct_with_const

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field1" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field1" =
    A

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field1")
         ) => GHC.Records.HasField "example_struct_with_const_const_field1" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_with_const_const_field1")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field2" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field2" =
    A

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field2")
         ) => GHC.Records.HasField "example_struct_with_const_const_field2" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_with_const_const_field2")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field3" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field3" =
    HsBindgen.Runtime.PtrConst.PtrConst A

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field3")
         ) => GHC.Records.HasField "example_struct_with_const_const_field3" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_with_const_const_field3")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field4" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field4" =
    HsBindgen.Runtime.PtrConst.PtrConst A

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field4")
         ) => GHC.Records.HasField "example_struct_with_const_const_field4" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_with_const_const_field4")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field5" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field5" =
    Ptr.Ptr A

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field5")
         ) => GHC.Records.HasField "example_struct_with_const_const_field5" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_with_const_const_field5")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field6" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field6" =
    HsBindgen.Runtime.PtrConst.PtrConst A

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field6")
         ) => GHC.Records.HasField "example_struct_with_const_const_field6" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_with_const_const_field6")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field7" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field7" =
    HsBindgen.Runtime.PtrConst.PtrConst A

  offset# = \_ -> \_ -> 40

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field7")
         ) => GHC.Records.HasField "example_struct_with_const_const_field7" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"example_struct_with_const_const_field7")

{-| Auxiliary type used by 'Const_funptr1'

__C declaration:__ @const_funptr1@

__defined at:__ @macros\/reparse.h 238:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1_Aux = Const_funptr1_Aux
  { unwrapConst_funptr1_Aux :: FC.CInt -> FC.CDouble -> IO A
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_7f125e20a9d4075b_base ::
     (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32))

-- __unique:__ @toConst_funptr1_Aux@
hs_bindgen_7f125e20a9d4075b ::
     Const_funptr1_Aux
  -> IO (Ptr.FunPtr Const_funptr1_Aux)
hs_bindgen_7f125e20a9d4075b =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_7f125e20a9d4075b_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ac4bd8d789bba94b_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> Double -> IO GHC.Int.Int32

-- __unique:__ @fromConst_funptr1_Aux@
hs_bindgen_ac4bd8d789bba94b ::
     Ptr.FunPtr Const_funptr1_Aux
  -> Const_funptr1_Aux
hs_bindgen_ac4bd8d789bba94b =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_ac4bd8d789bba94b_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr1_Aux where

  toFunPtr = hs_bindgen_7f125e20a9d4075b

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr1_Aux where

  fromFunPtr = hs_bindgen_ac4bd8d789bba94b

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr1_Aux) "unwrapConst_funptr1_Aux")
         ) => GHC.Records.HasField "unwrapConst_funptr1_Aux" (Ptr.Ptr Const_funptr1_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr1_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr1_Aux "unwrapConst_funptr1_Aux" where

  type CFieldType Const_funptr1_Aux "unwrapConst_funptr1_Aux" =
    FC.CInt -> FC.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr1@

    __defined at:__ @macros\/reparse.h 238:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1 = Const_funptr1
  { unwrapConst_funptr1 :: Ptr.FunPtr Const_funptr1_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr1) "unwrapConst_funptr1")
         ) => GHC.Records.HasField "unwrapConst_funptr1" (Ptr.Ptr Const_funptr1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr1")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr1 "unwrapConst_funptr1" where

  type CFieldType Const_funptr1 "unwrapConst_funptr1" =
    Ptr.FunPtr Const_funptr1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr2'

__C declaration:__ @const_funptr2@

__defined at:__ @macros\/reparse.h 239:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2_Aux = Const_funptr2_Aux
  { unwrapConst_funptr2_Aux :: FC.CInt -> FC.CDouble -> IO A
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_c7b1e36d845634fb_base ::
     (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32))

-- __unique:__ @toConst_funptr2_Aux@
hs_bindgen_c7b1e36d845634fb ::
     Const_funptr2_Aux
  -> IO (Ptr.FunPtr Const_funptr2_Aux)
hs_bindgen_c7b1e36d845634fb =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_c7b1e36d845634fb_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_352cebf463125ca9_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> Double -> IO GHC.Int.Int32

-- __unique:__ @fromConst_funptr2_Aux@
hs_bindgen_352cebf463125ca9 ::
     Ptr.FunPtr Const_funptr2_Aux
  -> Const_funptr2_Aux
hs_bindgen_352cebf463125ca9 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_352cebf463125ca9_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr2_Aux where

  toFunPtr = hs_bindgen_c7b1e36d845634fb

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr2_Aux where

  fromFunPtr = hs_bindgen_352cebf463125ca9

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr2_Aux) "unwrapConst_funptr2_Aux")
         ) => GHC.Records.HasField "unwrapConst_funptr2_Aux" (Ptr.Ptr Const_funptr2_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr2_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr2_Aux "unwrapConst_funptr2_Aux" where

  type CFieldType Const_funptr2_Aux "unwrapConst_funptr2_Aux" =
    FC.CInt -> FC.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr2@

    __defined at:__ @macros\/reparse.h 239:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2 = Const_funptr2
  { unwrapConst_funptr2 :: Ptr.FunPtr Const_funptr2_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr2) "unwrapConst_funptr2")
         ) => GHC.Records.HasField "unwrapConst_funptr2" (Ptr.Ptr Const_funptr2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr2")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr2 "unwrapConst_funptr2" where

  type CFieldType Const_funptr2 "unwrapConst_funptr2" =
    Ptr.FunPtr Const_funptr2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr3'

__C declaration:__ @const_funptr3@

__defined at:__ @macros\/reparse.h 240:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3_Aux = Const_funptr3_Aux
  { unwrapConst_funptr3_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_2dcbfe1c2502178c_base ::
     (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)))

-- __unique:__ @toConst_funptr3_Aux@
hs_bindgen_2dcbfe1c2502178c ::
     Const_funptr3_Aux
  -> IO (Ptr.FunPtr Const_funptr3_Aux)
hs_bindgen_2dcbfe1c2502178c =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_2dcbfe1c2502178c_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_86738dcfd7c9d33c_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)

-- __unique:__ @fromConst_funptr3_Aux@
hs_bindgen_86738dcfd7c9d33c ::
     Ptr.FunPtr Const_funptr3_Aux
  -> Const_funptr3_Aux
hs_bindgen_86738dcfd7c9d33c =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_86738dcfd7c9d33c_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr3_Aux where

  toFunPtr = hs_bindgen_2dcbfe1c2502178c

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr3_Aux where

  fromFunPtr = hs_bindgen_86738dcfd7c9d33c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr3_Aux) "unwrapConst_funptr3_Aux")
         ) => GHC.Records.HasField "unwrapConst_funptr3_Aux" (Ptr.Ptr Const_funptr3_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr3_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr3_Aux "unwrapConst_funptr3_Aux" where

  type CFieldType Const_funptr3_Aux "unwrapConst_funptr3_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr3@

    __defined at:__ @macros\/reparse.h 240:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3 = Const_funptr3
  { unwrapConst_funptr3 :: Ptr.FunPtr Const_funptr3_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr3) "unwrapConst_funptr3")
         ) => GHC.Records.HasField "unwrapConst_funptr3" (Ptr.Ptr Const_funptr3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr3")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr3 "unwrapConst_funptr3" where

  type CFieldType Const_funptr3 "unwrapConst_funptr3" =
    Ptr.FunPtr Const_funptr3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr4'

__C declaration:__ @const_funptr4@

__defined at:__ @macros\/reparse.h 241:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4_Aux = Const_funptr4_Aux
  { unwrapConst_funptr4_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_5461deeda491de0b_base ::
     (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)))

-- __unique:__ @toConst_funptr4_Aux@
hs_bindgen_5461deeda491de0b ::
     Const_funptr4_Aux
  -> IO (Ptr.FunPtr Const_funptr4_Aux)
hs_bindgen_5461deeda491de0b =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_5461deeda491de0b_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_de7846fca3bfd1b6_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)

-- __unique:__ @fromConst_funptr4_Aux@
hs_bindgen_de7846fca3bfd1b6 ::
     Ptr.FunPtr Const_funptr4_Aux
  -> Const_funptr4_Aux
hs_bindgen_de7846fca3bfd1b6 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_de7846fca3bfd1b6_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr4_Aux where

  toFunPtr = hs_bindgen_5461deeda491de0b

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr4_Aux where

  fromFunPtr = hs_bindgen_de7846fca3bfd1b6

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr4_Aux) "unwrapConst_funptr4_Aux")
         ) => GHC.Records.HasField "unwrapConst_funptr4_Aux" (Ptr.Ptr Const_funptr4_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr4_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr4_Aux "unwrapConst_funptr4_Aux" where

  type CFieldType Const_funptr4_Aux "unwrapConst_funptr4_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr4@

    __defined at:__ @macros\/reparse.h 241:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4 = Const_funptr4
  { unwrapConst_funptr4 :: Ptr.FunPtr Const_funptr4_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr4) "unwrapConst_funptr4")
         ) => GHC.Records.HasField "unwrapConst_funptr4" (Ptr.Ptr Const_funptr4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr4")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr4 "unwrapConst_funptr4" where

  type CFieldType Const_funptr4 "unwrapConst_funptr4" =
    Ptr.FunPtr Const_funptr4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr5'

__C declaration:__ @const_funptr5@

__defined at:__ @macros\/reparse.h 242:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5_Aux = Const_funptr5_Aux
  { unwrapConst_funptr5_Aux :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_7b0174fc978a1ce1_base ::
     (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)))

-- __unique:__ @toConst_funptr5_Aux@
hs_bindgen_7b0174fc978a1ce1 ::
     Const_funptr5_Aux
  -> IO (Ptr.FunPtr Const_funptr5_Aux)
hs_bindgen_7b0174fc978a1ce1 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_7b0174fc978a1ce1_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_38a21d84bb7115b5_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)

-- __unique:__ @fromConst_funptr5_Aux@
hs_bindgen_38a21d84bb7115b5 ::
     Ptr.FunPtr Const_funptr5_Aux
  -> Const_funptr5_Aux
hs_bindgen_38a21d84bb7115b5 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_38a21d84bb7115b5_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr5_Aux where

  toFunPtr = hs_bindgen_7b0174fc978a1ce1

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr5_Aux where

  fromFunPtr = hs_bindgen_38a21d84bb7115b5

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr5_Aux) "unwrapConst_funptr5_Aux")
         ) => GHC.Records.HasField "unwrapConst_funptr5_Aux" (Ptr.Ptr Const_funptr5_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr5_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr5_Aux "unwrapConst_funptr5_Aux" where

  type CFieldType Const_funptr5_Aux "unwrapConst_funptr5_Aux" =
    FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr5@

    __defined at:__ @macros\/reparse.h 242:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5 = Const_funptr5
  { unwrapConst_funptr5 :: Ptr.FunPtr Const_funptr5_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr5) "unwrapConst_funptr5")
         ) => GHC.Records.HasField "unwrapConst_funptr5" (Ptr.Ptr Const_funptr5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr5")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr5 "unwrapConst_funptr5" where

  type CFieldType Const_funptr5 "unwrapConst_funptr5" =
    Ptr.FunPtr Const_funptr5_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr6'

__C declaration:__ @const_funptr6@

__defined at:__ @macros\/reparse.h 243:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6_Aux = Const_funptr6_Aux
  { unwrapConst_funptr6_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_4e32721222f4df9f_base ::
     (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)))

-- __unique:__ @toConst_funptr6_Aux@
hs_bindgen_4e32721222f4df9f ::
     Const_funptr6_Aux
  -> IO (Ptr.FunPtr Const_funptr6_Aux)
hs_bindgen_4e32721222f4df9f =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_4e32721222f4df9f_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_45251216b04aa8b5_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)

-- __unique:__ @fromConst_funptr6_Aux@
hs_bindgen_45251216b04aa8b5 ::
     Ptr.FunPtr Const_funptr6_Aux
  -> Const_funptr6_Aux
hs_bindgen_45251216b04aa8b5 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_45251216b04aa8b5_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr6_Aux where

  toFunPtr = hs_bindgen_4e32721222f4df9f

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr6_Aux where

  fromFunPtr = hs_bindgen_45251216b04aa8b5

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr6_Aux) "unwrapConst_funptr6_Aux")
         ) => GHC.Records.HasField "unwrapConst_funptr6_Aux" (Ptr.Ptr Const_funptr6_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr6_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr6_Aux "unwrapConst_funptr6_Aux" where

  type CFieldType Const_funptr6_Aux "unwrapConst_funptr6_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr6@

    __defined at:__ @macros\/reparse.h 243:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6 = Const_funptr6
  { unwrapConst_funptr6 :: Ptr.FunPtr Const_funptr6_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr6) "unwrapConst_funptr6")
         ) => GHC.Records.HasField "unwrapConst_funptr6" (Ptr.Ptr Const_funptr6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr6")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr6 "unwrapConst_funptr6" where

  type CFieldType Const_funptr6 "unwrapConst_funptr6" =
    Ptr.FunPtr Const_funptr6_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr7'

__C declaration:__ @const_funptr7@

__defined at:__ @macros\/reparse.h 244:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7_Aux = Const_funptr7_Aux
  { unwrapConst_funptr7_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_0d04fc96ffb9de06_base ::
     (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)))

-- __unique:__ @toConst_funptr7_Aux@
hs_bindgen_0d04fc96ffb9de06 ::
     Const_funptr7_Aux
  -> IO (Ptr.FunPtr Const_funptr7_Aux)
hs_bindgen_0d04fc96ffb9de06 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_0d04fc96ffb9de06_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_42fbcebf75a973ba_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void))
  -> GHC.Int.Int32 -> Double -> IO (Ptr.Ptr Void)

-- __unique:__ @fromConst_funptr7_Aux@
hs_bindgen_42fbcebf75a973ba ::
     Ptr.FunPtr Const_funptr7_Aux
  -> Const_funptr7_Aux
hs_bindgen_42fbcebf75a973ba =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_42fbcebf75a973ba_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr7_Aux where

  toFunPtr = hs_bindgen_0d04fc96ffb9de06

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr7_Aux where

  fromFunPtr = hs_bindgen_42fbcebf75a973ba

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr7_Aux) "unwrapConst_funptr7_Aux")
         ) => GHC.Records.HasField "unwrapConst_funptr7_Aux" (Ptr.Ptr Const_funptr7_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr7_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr7_Aux "unwrapConst_funptr7_Aux" where

  type CFieldType Const_funptr7_Aux "unwrapConst_funptr7_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.PtrConst.PtrConst A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr7@

    __defined at:__ @macros\/reparse.h 244:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7 = Const_funptr7
  { unwrapConst_funptr7 :: Ptr.FunPtr Const_funptr7_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr7) "unwrapConst_funptr7")
         ) => GHC.Records.HasField "unwrapConst_funptr7" (Ptr.Ptr Const_funptr7) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapConst_funptr7")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr7 "unwrapConst_funptr7" where

  type CFieldType Const_funptr7 "unwrapConst_funptr7" =
    Ptr.FunPtr Const_funptr7_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BOOL@

    __defined at:__ @macros\/reparse.h 280:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype BOOL = BOOL
  { unwrapBOOL :: FC.CBool
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType BOOL) "unwrapBOOL")
         ) => GHC.Records.HasField "unwrapBOOL" (Ptr.Ptr BOOL) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapBOOL")

instance HsBindgen.Runtime.HasCField.HasCField BOOL "unwrapBOOL" where

  type CFieldType BOOL "unwrapBOOL" = FC.CBool

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INT@

    __defined at:__ @macros\/reparse.h 281:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INT = INT
  { unwrapINT :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType INT) "unwrapINT")
         ) => GHC.Records.HasField "unwrapINT" (Ptr.Ptr INT) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapINT")

instance HsBindgen.Runtime.HasCField.HasCField INT "unwrapINT" where

  type CFieldType INT "unwrapINT" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INTP@

    __defined at:__ @macros\/reparse.h 282:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INTP = INTP
  { unwrapINTP :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType INTP) "unwrapINTP")
         ) => GHC.Records.HasField "unwrapINTP" (Ptr.Ptr INTP) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapINTP")

instance HsBindgen.Runtime.HasCField.HasCField INTP "unwrapINTP" where

  type CFieldType INTP "unwrapINTP" = Ptr.Ptr FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @INTCP@

    __defined at:__ @macros\/reparse.h 283:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INTCP = INTCP
  { unwrapINTCP :: HsBindgen.Runtime.PtrConst.PtrConst FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType INTCP) "unwrapINTCP")
         ) => GHC.Records.HasField "unwrapINTCP" (Ptr.Ptr INTCP) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapINTCP")

instance HsBindgen.Runtime.HasCField.HasCField INTCP "unwrapINTCP" where

  type CFieldType INTCP "unwrapINTCP" =
    HsBindgen.Runtime.PtrConst.PtrConst FC.CInt

  offset# = \_ -> \_ -> 0
