{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
    ( Example.a
    , Example.Some_struct(..)
    , Example.Some_union(..)
    , Example.Some_enum(..)
    , pattern Example.ENUM_A
    , Example.Arr_typedef1(..)
    , Example.Arr_typedef2(..)
    , Example.Arr_typedef3(..)
    , Example.Arr_typedef4(..)
    , Example.Typedef1(..)
    , Example.Typedef2(..)
    , Example.Typedef3(..)
    , Example.Funptr_typedef1_Aux(..)
    , Example.Funptr_typedef1(..)
    , Example.Funptr_typedef2_Aux(..)
    , Example.Funptr_typedef2(..)
    , Example.Funptr_typedef3_Aux(..)
    , Example.Funptr_typedef3(..)
    , Example.Funptr_typedef4_Aux(..)
    , Example.Funptr_typedef4(..)
    , Example.Funptr_typedef5_Aux(..)
    , Example.Funptr_typedef5(..)
    , Example.Comments2(..)
    , Example.Example_struct(..)
    , Example.Const_typedef1(..)
    , Example.Const_typedef2(..)
    , Example.Const_typedef3(..)
    , Example.Const_typedef4(..)
    , Example.Const_typedef5(..)
    , Example.Const_typedef6(..)
    , Example.Const_typedef7(..)
    , Example.Example_struct_with_const(..)
    , Example.Const_funptr1_Aux(..)
    , Example.Const_funptr1(..)
    , Example.Const_funptr2_Aux(..)
    , Example.Const_funptr2(..)
    , Example.Const_funptr3_Aux(..)
    , Example.Const_funptr3(..)
    , Example.Const_funptr4_Aux(..)
    , Example.Const_funptr4(..)
    , Example.Const_funptr5_Aux(..)
    , Example.Const_funptr5(..)
    , Example.Const_funptr6_Aux(..)
    , Example.Const_funptr6(..)
    , Example.Const_funptr7_Aux(..)
    , Example.Const_funptr7(..)
    , Example.bOOL
    , Example.iNT
    , Example.iNTP
    , Example.iNTCP
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @macro A@

    __defined at:__ @macros\/reparse.h 3:9@

    __exported by:__ @macros\/reparse.h@
-}
a :: [String]
a = ["int"]

{-| __C declaration:__ @struct some_struct@

    __defined at:__ @macros\/reparse.h 7:8@

    __exported by:__ @macros\/reparse.h@
-}
data Some_struct = Some_struct
  {}
  deriving stock (Eq, BG.Generic, Show)

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

deriving via Marshal.EquivStorable Some_struct instance BG.Storable Some_struct

{-| __C declaration:__ @union some_union@

    __defined at:__ @macros\/reparse.h 8:7@

    __exported by:__ @macros\/reparse.h@
-}
newtype Some_union = Some_union
  { unwrapSome_union :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 0 1 instance Marshal.StaticSize Some_union

deriving via BG.SizedByteArray 0 1 instance Marshal.ReadRaw Some_union

deriving via BG.SizedByteArray 0 1 instance Marshal.WriteRaw Some_union

deriving via Marshal.EquivStorable Some_union instance BG.Storable Some_union

deriving via BG.SizedByteArray 0 1 instance Union.IsUnion Some_union

{-| __C declaration:__ @enum some_enum@

    __defined at:__ @macros\/reparse.h 9:6@

    __exported by:__ @macros\/reparse.h@
-}
newtype Some_enum = Some_enum
  { unwrapSome_enum :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable Some_enum instance BG.Storable Some_enum

deriving via BG.CUInt instance BG.Prim Some_enum

instance CEnum.CEnum Some_enum where

  type CEnumZ Some_enum = BG.CUInt

  toCEnum = Some_enum

  fromCEnum = BG.getField @"unwrapSome_enum"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "ENUM_A")]

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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapSome_enum" Some_enum ty where

  hasField =
    \x0 ->
      (\y1 ->
         Some_enum {unwrapSome_enum = y1}, BG.getField @"unwrapSome_enum" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapSome_enum" (BG.Ptr Some_enum) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSome_enum")

instance HasCField.HasCField Some_enum "unwrapSome_enum" where

  type CFieldType Some_enum "unwrapSome_enum" =
    BG.CUInt

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
  { unwrapArr_typedef1 :: IA.IncompleteArray BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.CompatHasField.HasField "unwrapArr_typedef1" Arr_typedef1 ty where

  hasField =
    \x0 ->
      ( \y1 -> Arr_typedef1 {unwrapArr_typedef1 = y1}
      , BG.getField @"unwrapArr_typedef1" x0
      )

instance ( ty ~ IA.IncompleteArray BG.CInt
         ) => BG.HasField "unwrapArr_typedef1" (BG.Ptr Arr_typedef1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapArr_typedef1")

instance HasCField.HasCField Arr_typedef1 "unwrapArr_typedef1" where

  type CFieldType Arr_typedef1 "unwrapArr_typedef1" =
    IA.IncompleteArray BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef2@

    __defined at:__ @macros\/reparse.h 110:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef2 = Arr_typedef2
  { unwrapArr_typedef2 :: IA.IncompleteArray (BG.Ptr BG.CInt)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray (BG.Ptr BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapArr_typedef2" Arr_typedef2 ty where

  hasField =
    \x0 ->
      ( \y1 -> Arr_typedef2 {unwrapArr_typedef2 = y1}
      , BG.getField @"unwrapArr_typedef2" x0
      )

instance ( ty ~ IA.IncompleteArray (BG.Ptr BG.CInt)
         ) => BG.HasField "unwrapArr_typedef2" (BG.Ptr Arr_typedef2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapArr_typedef2")

instance HasCField.HasCField Arr_typedef2 "unwrapArr_typedef2" where

  type CFieldType Arr_typedef2 "unwrapArr_typedef2" =
    IA.IncompleteArray (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef3@

    __defined at:__ @macros\/reparse.h 111:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef3 = Arr_typedef3
  { unwrapArr_typedef3 :: CA.ConstantArray 5 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 5 BG.CInt
         ) => BG.CompatHasField.HasField "unwrapArr_typedef3" Arr_typedef3 ty where

  hasField =
    \x0 ->
      ( \y1 -> Arr_typedef3 {unwrapArr_typedef3 = y1}
      , BG.getField @"unwrapArr_typedef3" x0
      )

instance ( ty ~ CA.ConstantArray 5 BG.CInt
         ) => BG.HasField "unwrapArr_typedef3" (BG.Ptr Arr_typedef3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapArr_typedef3")

instance HasCField.HasCField Arr_typedef3 "unwrapArr_typedef3" where

  type CFieldType Arr_typedef3 "unwrapArr_typedef3" =
    CA.ConstantArray 5 BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef4@

    __defined at:__ @macros\/reparse.h 112:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef4 = Arr_typedef4
  { unwrapArr_typedef4 :: CA.ConstantArray 5 (BG.Ptr BG.CInt)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 5 (BG.Ptr BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapArr_typedef4" Arr_typedef4 ty where

  hasField =
    \x0 ->
      ( \y1 -> Arr_typedef4 {unwrapArr_typedef4 = y1}
      , BG.getField @"unwrapArr_typedef4" x0
      )

instance ( ty ~ CA.ConstantArray 5 (BG.Ptr BG.CInt)
         ) => BG.HasField "unwrapArr_typedef4" (BG.Ptr Arr_typedef4) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapArr_typedef4")

instance HasCField.HasCField Arr_typedef4 "unwrapArr_typedef4" where

  type CFieldType Arr_typedef4 "unwrapArr_typedef4" =
    CA.ConstantArray 5 (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| Typedefs

    __C declaration:__ @typedef1@

    __defined at:__ @macros\/reparse.h 118:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef1 = Typedef1
  { unwrapTypedef1 :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapTypedef1" Typedef1 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Typedef1 {unwrapTypedef1 = y1}, BG.getField @"unwrapTypedef1" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapTypedef1" (BG.Ptr Typedef1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTypedef1")

instance HasCField.HasCField Typedef1 "unwrapTypedef1" where

  type CFieldType Typedef1 "unwrapTypedef1" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef2@

    __defined at:__ @macros\/reparse.h 119:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef2 = Typedef2
  { unwrapTypedef2 :: BG.Ptr BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "unwrapTypedef2" Typedef2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Typedef2 {unwrapTypedef2 = y1}, BG.getField @"unwrapTypedef2" x0)

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "unwrapTypedef2" (BG.Ptr Typedef2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTypedef2")

instance HasCField.HasCField Typedef2 "unwrapTypedef2" where

  type CFieldType Typedef2 "unwrapTypedef2" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef3@

    __defined at:__ @macros\/reparse.h 120:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef3 = Typedef3
  { unwrapTypedef3 :: BG.Ptr (BG.Ptr BG.CInt)
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr (BG.Ptr BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapTypedef3" Typedef3 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Typedef3 {unwrapTypedef3 = y1}, BG.getField @"unwrapTypedef3" x0)

instance ( ty ~ BG.Ptr (BG.Ptr BG.CInt)
         ) => BG.HasField "unwrapTypedef3" (BG.Ptr Typedef3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTypedef3")

instance HasCField.HasCField Typedef3 "unwrapTypedef3" where

  type CFieldType Typedef3 "unwrapTypedef3" =
    BG.Ptr (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef1'

    __C declaration:__ @funptr_typedef1@

    __defined at:__ @macros\/reparse.h 132:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1_Aux = Funptr_typedef1_Aux
  { unwrapFunptr_typedef1_Aux :: IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFunptr_typedef1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c584d0f839fd43de_base ::
     IO BG.Int32
  -> IO (BG.FunPtr (IO BG.Int32))

-- __unique:__ @toFunptr_typedef1_Aux@
hs_bindgen_c584d0f839fd43de ::
     Funptr_typedef1_Aux
  -> IO (BG.FunPtr Funptr_typedef1_Aux)
hs_bindgen_c584d0f839fd43de =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_c584d0f839fd43de_base (BG.toFFIType fun0))

-- __unique:__ @fromFunptr_typedef1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_806a46dc418a062c_base ::
     BG.FunPtr (IO BG.Int32)
  -> IO BG.Int32

-- __unique:__ @fromFunptr_typedef1_Aux@
hs_bindgen_806a46dc418a062c ::
     BG.FunPtr Funptr_typedef1_Aux
  -> Funptr_typedef1_Aux
hs_bindgen_806a46dc418a062c =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_806a46dc418a062c_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Funptr_typedef1_Aux where

  toFunPtr = hs_bindgen_c584d0f839fd43de

instance BG.FromFunPtr Funptr_typedef1_Aux where

  fromFunPtr = hs_bindgen_806a46dc418a062c

instance ( ty ~ IO BG.CInt
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef1_Aux" Funptr_typedef1_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Funptr_typedef1_Aux {unwrapFunptr_typedef1_Aux = y1}
      , BG.getField @"unwrapFunptr_typedef1_Aux" x0
      )

instance ( ty ~ IO BG.CInt
         ) => BG.HasField "unwrapFunptr_typedef1_Aux" (BG.Ptr Funptr_typedef1_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef1_Aux")

instance HasCField.HasCField Funptr_typedef1_Aux "unwrapFunptr_typedef1_Aux" where

  type CFieldType Funptr_typedef1_Aux "unwrapFunptr_typedef1_Aux" =
    IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef1@

    __defined at:__ @macros\/reparse.h 132:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1 = Funptr_typedef1
  { unwrapFunptr_typedef1 :: BG.FunPtr Funptr_typedef1_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Funptr_typedef1_Aux
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef1" Funptr_typedef1 ty where

  hasField =
    \x0 ->
      ( \y1 -> Funptr_typedef1 {unwrapFunptr_typedef1 = y1}
      , BG.getField @"unwrapFunptr_typedef1" x0
      )

instance ( ty ~ BG.FunPtr Funptr_typedef1_Aux
         ) => BG.HasField "unwrapFunptr_typedef1" (BG.Ptr Funptr_typedef1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef1")

instance HasCField.HasCField Funptr_typedef1 "unwrapFunptr_typedef1" where

  type CFieldType Funptr_typedef1 "unwrapFunptr_typedef1" =
    BG.FunPtr Funptr_typedef1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef2'

    __C declaration:__ @funptr_typedef2@

    __defined at:__ @macros\/reparse.h 133:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2_Aux = Funptr_typedef2_Aux
  { unwrapFunptr_typedef2_Aux :: IO (BG.Ptr BG.CInt)
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFunptr_typedef2_Aux@
foreign import ccall safe "wrapper" hs_bindgen_f174457a161ac5a0_base ::
     IO (BG.Ptr BG.Void)
  -> IO (BG.FunPtr (IO (BG.Ptr BG.Void)))

-- __unique:__ @toFunptr_typedef2_Aux@
hs_bindgen_f174457a161ac5a0 ::
     Funptr_typedef2_Aux
  -> IO (BG.FunPtr Funptr_typedef2_Aux)
hs_bindgen_f174457a161ac5a0 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_f174457a161ac5a0_base (BG.toFFIType fun0))

-- __unique:__ @fromFunptr_typedef2_Aux@
foreign import ccall safe "dynamic" hs_bindgen_323d07dff85b802c_base ::
     BG.FunPtr (IO (BG.Ptr BG.Void))
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromFunptr_typedef2_Aux@
hs_bindgen_323d07dff85b802c ::
     BG.FunPtr Funptr_typedef2_Aux
  -> Funptr_typedef2_Aux
hs_bindgen_323d07dff85b802c =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_323d07dff85b802c_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Funptr_typedef2_Aux where

  toFunPtr = hs_bindgen_f174457a161ac5a0

instance BG.FromFunPtr Funptr_typedef2_Aux where

  fromFunPtr = hs_bindgen_323d07dff85b802c

instance ( ty ~ IO (BG.Ptr BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef2_Aux" Funptr_typedef2_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Funptr_typedef2_Aux {unwrapFunptr_typedef2_Aux = y1}
      , BG.getField @"unwrapFunptr_typedef2_Aux" x0
      )

instance ( ty ~ IO (BG.Ptr BG.CInt)
         ) => BG.HasField "unwrapFunptr_typedef2_Aux" (BG.Ptr Funptr_typedef2_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef2_Aux")

instance HasCField.HasCField Funptr_typedef2_Aux "unwrapFunptr_typedef2_Aux" where

  type CFieldType Funptr_typedef2_Aux "unwrapFunptr_typedef2_Aux" =
    IO (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef2@

    __defined at:__ @macros\/reparse.h 133:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2 = Funptr_typedef2
  { unwrapFunptr_typedef2 :: BG.FunPtr Funptr_typedef2_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Funptr_typedef2_Aux
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef2" Funptr_typedef2 ty where

  hasField =
    \x0 ->
      ( \y1 -> Funptr_typedef2 {unwrapFunptr_typedef2 = y1}
      , BG.getField @"unwrapFunptr_typedef2" x0
      )

instance ( ty ~ BG.FunPtr Funptr_typedef2_Aux
         ) => BG.HasField "unwrapFunptr_typedef2" (BG.Ptr Funptr_typedef2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef2")

instance HasCField.HasCField Funptr_typedef2 "unwrapFunptr_typedef2" where

  type CFieldType Funptr_typedef2 "unwrapFunptr_typedef2" =
    BG.FunPtr Funptr_typedef2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef3'

    __C declaration:__ @funptr_typedef3@

    __defined at:__ @macros\/reparse.h 134:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3_Aux = Funptr_typedef3_Aux
  { unwrapFunptr_typedef3_Aux :: IO (BG.Ptr (BG.Ptr BG.CInt))
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFunptr_typedef3_Aux@
foreign import ccall safe "wrapper" hs_bindgen_031d1a7decd790d8_base ::
     IO (BG.Ptr BG.Void)
  -> IO (BG.FunPtr (IO (BG.Ptr BG.Void)))

-- __unique:__ @toFunptr_typedef3_Aux@
hs_bindgen_031d1a7decd790d8 ::
     Funptr_typedef3_Aux
  -> IO (BG.FunPtr Funptr_typedef3_Aux)
hs_bindgen_031d1a7decd790d8 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_031d1a7decd790d8_base (BG.toFFIType fun0))

-- __unique:__ @fromFunptr_typedef3_Aux@
foreign import ccall safe "dynamic" hs_bindgen_82dc7b932974117e_base ::
     BG.FunPtr (IO (BG.Ptr BG.Void))
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromFunptr_typedef3_Aux@
hs_bindgen_82dc7b932974117e ::
     BG.FunPtr Funptr_typedef3_Aux
  -> Funptr_typedef3_Aux
hs_bindgen_82dc7b932974117e =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_82dc7b932974117e_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Funptr_typedef3_Aux where

  toFunPtr = hs_bindgen_031d1a7decd790d8

instance BG.FromFunPtr Funptr_typedef3_Aux where

  fromFunPtr = hs_bindgen_82dc7b932974117e

instance ( ty ~ IO (BG.Ptr (BG.Ptr BG.CInt))
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef3_Aux" Funptr_typedef3_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Funptr_typedef3_Aux {unwrapFunptr_typedef3_Aux = y1}
      , BG.getField @"unwrapFunptr_typedef3_Aux" x0
      )

instance ( ty ~ IO (BG.Ptr (BG.Ptr BG.CInt))
         ) => BG.HasField "unwrapFunptr_typedef3_Aux" (BG.Ptr Funptr_typedef3_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef3_Aux")

instance HasCField.HasCField Funptr_typedef3_Aux "unwrapFunptr_typedef3_Aux" where

  type CFieldType Funptr_typedef3_Aux "unwrapFunptr_typedef3_Aux" =
    IO (BG.Ptr (BG.Ptr BG.CInt))

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef3@

    __defined at:__ @macros\/reparse.h 134:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3 = Funptr_typedef3
  { unwrapFunptr_typedef3 :: BG.FunPtr Funptr_typedef3_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Funptr_typedef3_Aux
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef3" Funptr_typedef3 ty where

  hasField =
    \x0 ->
      ( \y1 -> Funptr_typedef3 {unwrapFunptr_typedef3 = y1}
      , BG.getField @"unwrapFunptr_typedef3" x0
      )

instance ( ty ~ BG.FunPtr Funptr_typedef3_Aux
         ) => BG.HasField "unwrapFunptr_typedef3" (BG.Ptr Funptr_typedef3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef3")

instance HasCField.HasCField Funptr_typedef3 "unwrapFunptr_typedef3" where

  type CFieldType Funptr_typedef3 "unwrapFunptr_typedef3" =
    BG.FunPtr Funptr_typedef3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef4'

    __C declaration:__ @funptr_typedef4@

    __defined at:__ @macros\/reparse.h 135:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4_Aux = Funptr_typedef4_Aux
  { unwrapFunptr_typedef4_Aux :: BG.CInt -> BG.CDouble -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFunptr_typedef4_Aux@
foreign import ccall safe "wrapper" hs_bindgen_da2336d254667386_base ::
     (BG.Int32 -> Double -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO BG.Int32))

-- __unique:__ @toFunptr_typedef4_Aux@
hs_bindgen_da2336d254667386 ::
     Funptr_typedef4_Aux
  -> IO (BG.FunPtr Funptr_typedef4_Aux)
hs_bindgen_da2336d254667386 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_da2336d254667386_base (BG.toFFIType fun0))

-- __unique:__ @fromFunptr_typedef4_Aux@
foreign import ccall safe "dynamic" hs_bindgen_d4a97954476da161_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO BG.Int32)
  -> BG.Int32 -> Double -> IO BG.Int32

-- __unique:__ @fromFunptr_typedef4_Aux@
hs_bindgen_d4a97954476da161 ::
     BG.FunPtr Funptr_typedef4_Aux
  -> Funptr_typedef4_Aux
hs_bindgen_d4a97954476da161 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_d4a97954476da161_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Funptr_typedef4_Aux where

  toFunPtr = hs_bindgen_da2336d254667386

instance BG.FromFunPtr Funptr_typedef4_Aux where

  fromFunPtr = hs_bindgen_d4a97954476da161

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef4_Aux" Funptr_typedef4_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Funptr_typedef4_Aux {unwrapFunptr_typedef4_Aux = y1}
      , BG.getField @"unwrapFunptr_typedef4_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO BG.CInt)
         ) => BG.HasField "unwrapFunptr_typedef4_Aux" (BG.Ptr Funptr_typedef4_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef4_Aux")

instance HasCField.HasCField Funptr_typedef4_Aux "unwrapFunptr_typedef4_Aux" where

  type CFieldType Funptr_typedef4_Aux "unwrapFunptr_typedef4_Aux" =
    BG.CInt -> BG.CDouble -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef4@

    __defined at:__ @macros\/reparse.h 135:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4 = Funptr_typedef4
  { unwrapFunptr_typedef4 :: BG.FunPtr Funptr_typedef4_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Funptr_typedef4_Aux
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef4" Funptr_typedef4 ty where

  hasField =
    \x0 ->
      ( \y1 -> Funptr_typedef4 {unwrapFunptr_typedef4 = y1}
      , BG.getField @"unwrapFunptr_typedef4" x0
      )

instance ( ty ~ BG.FunPtr Funptr_typedef4_Aux
         ) => BG.HasField "unwrapFunptr_typedef4" (BG.Ptr Funptr_typedef4) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef4")

instance HasCField.HasCField Funptr_typedef4 "unwrapFunptr_typedef4" where

  type CFieldType Funptr_typedef4 "unwrapFunptr_typedef4" =
    BG.FunPtr Funptr_typedef4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef5'

    __C declaration:__ @funptr_typedef5@

    __defined at:__ @macros\/reparse.h 136:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5_Aux = Funptr_typedef5_Aux
  { unwrapFunptr_typedef5_Aux :: BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFunptr_typedef5_Aux@
foreign import ccall safe "wrapper" hs_bindgen_1f45632f07742a46_base ::
     (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toFunptr_typedef5_Aux@
hs_bindgen_1f45632f07742a46 ::
     Funptr_typedef5_Aux
  -> IO (BG.FunPtr Funptr_typedef5_Aux)
hs_bindgen_1f45632f07742a46 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_1f45632f07742a46_base (BG.toFFIType fun0))

-- __unique:__ @fromFunptr_typedef5_Aux@
foreign import ccall safe "dynamic" hs_bindgen_0bd1877eaaba0d3e_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> BG.Int32 -> Double -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromFunptr_typedef5_Aux@
hs_bindgen_0bd1877eaaba0d3e ::
     BG.FunPtr Funptr_typedef5_Aux
  -> Funptr_typedef5_Aux
hs_bindgen_0bd1877eaaba0d3e =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_0bd1877eaaba0d3e_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Funptr_typedef5_Aux where

  toFunPtr = hs_bindgen_1f45632f07742a46

instance BG.FromFunPtr Funptr_typedef5_Aux where

  fromFunPtr = hs_bindgen_0bd1877eaaba0d3e

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef5_Aux" Funptr_typedef5_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Funptr_typedef5_Aux {unwrapFunptr_typedef5_Aux = y1}
      , BG.getField @"unwrapFunptr_typedef5_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
         ) => BG.HasField "unwrapFunptr_typedef5_Aux" (BG.Ptr Funptr_typedef5_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef5_Aux")

instance HasCField.HasCField Funptr_typedef5_Aux "unwrapFunptr_typedef5_Aux" where

  type CFieldType Funptr_typedef5_Aux "unwrapFunptr_typedef5_Aux" =
    BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef5@

    __defined at:__ @macros\/reparse.h 136:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5 = Funptr_typedef5
  { unwrapFunptr_typedef5 :: BG.FunPtr Funptr_typedef5_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Funptr_typedef5_Aux
         ) => BG.CompatHasField.HasField "unwrapFunptr_typedef5" Funptr_typedef5 ty where

  hasField =
    \x0 ->
      ( \y1 -> Funptr_typedef5 {unwrapFunptr_typedef5 = y1}
      , BG.getField @"unwrapFunptr_typedef5" x0
      )

instance ( ty ~ BG.FunPtr Funptr_typedef5_Aux
         ) => BG.HasField "unwrapFunptr_typedef5" (BG.Ptr Funptr_typedef5) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunptr_typedef5")

instance HasCField.HasCField Funptr_typedef5 "unwrapFunptr_typedef5" where

  type CFieldType Funptr_typedef5 "unwrapFunptr_typedef5" =
    BG.FunPtr Funptr_typedef5_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @comments2@

    __defined at:__ @macros\/reparse.h 145:30@

    __exported by:__ @macros\/reparse.h@
-}
newtype Comments2 = Comments2
  { unwrapComments2 :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapComments2" Comments2 ty where

  hasField =
    \x0 ->
      (\y1 ->
         Comments2 {unwrapComments2 = y1}, BG.getField @"unwrapComments2" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapComments2" (BG.Ptr Comments2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapComments2")

instance HasCField.HasCField Comments2 "unwrapComments2" where

  type CFieldType Comments2 "unwrapComments2" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| Struct fields

    __C declaration:__ @struct example_struct@

    __defined at:__ @macros\/reparse.h 151:8@

    __exported by:__ @macros\/reparse.h@
-}
data Example_struct = Example_struct
  { example_struct_field1 :: BG.CInt
    {- ^ __C declaration:__ @field1@

         __defined at:__ @macros\/reparse.h 152:8@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_field2 :: BG.Ptr BG.CInt
    {- ^ __C declaration:__ @field2@

         __defined at:__ @macros\/reparse.h 153:8@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_field3 :: BG.Ptr (BG.Ptr BG.CInt)
    {- ^ __C declaration:__ @field3@

         __defined at:__ @macros\/reparse.h 154:8@

         __exported by:__ @macros\/reparse.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Example_struct where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Example_struct where

  readRaw =
    \ptr0 ->
          pure Example_struct
      <*> HasCField.readRaw (BG.Proxy @"example_struct_field1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_field2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_field3") ptr0

instance Marshal.WriteRaw Example_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct
            example_struct_field12
            example_struct_field23
            example_struct_field34 ->
                 HasCField.writeRaw (BG.Proxy @"example_struct_field1") ptr0 example_struct_field12
              >> HasCField.writeRaw (BG.Proxy @"example_struct_field2") ptr0 example_struct_field23
              >> HasCField.writeRaw (BG.Proxy @"example_struct_field3") ptr0 example_struct_field34

deriving via Marshal.EquivStorable Example_struct instance BG.Storable Example_struct

{-| __C declaration:__ @field1@

    __defined at:__ @macros\/reparse.h 152:8@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_field1" Example_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct { example_struct_field1 = y1
                         , example_struct_field2 = BG.getField @"example_struct_field2" x0
                         , example_struct_field3 = BG.getField @"example_struct_field3" x0
                         }
      , BG.getField @"example_struct_field1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "example_struct_field1" (BG.Ptr Example_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_field1")

instance HasCField.HasCField Example_struct "example_struct_field1" where

  type CFieldType Example_struct "example_struct_field1" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @field2@

    __defined at:__ @macros\/reparse.h 153:8@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_field2" Example_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct { example_struct_field2 = y1
                         , example_struct_field1 = BG.getField @"example_struct_field1" x0
                         , example_struct_field3 = BG.getField @"example_struct_field3" x0
                         }
      , BG.getField @"example_struct_field2" x0
      )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "example_struct_field2" (BG.Ptr Example_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_field2")

instance HasCField.HasCField Example_struct "example_struct_field2" where

  type CFieldType Example_struct "example_struct_field2" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @field3@

    __defined at:__ @macros\/reparse.h 154:8@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ BG.Ptr (BG.Ptr BG.CInt)
         ) => BG.CompatHasField.HasField "example_struct_field3" Example_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct { example_struct_field3 = y1
                         , example_struct_field1 = BG.getField @"example_struct_field1" x0
                         , example_struct_field2 = BG.getField @"example_struct_field2" x0
                         }
      , BG.getField @"example_struct_field3" x0
      )

instance ( ty ~ BG.Ptr (BG.Ptr BG.CInt)
         ) => BG.HasField "example_struct_field3" (BG.Ptr Example_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_field3")

instance HasCField.HasCField Example_struct "example_struct_field3" where

  type CFieldType Example_struct "example_struct_field3" =
    BG.Ptr (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @const_typedef1@

    __defined at:__ @macros\/reparse.h 218:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef1 = Const_typedef1
  { unwrapConst_typedef1 :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConst_typedef1" Const_typedef1 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_typedef1 {unwrapConst_typedef1 = y1}
      , BG.getField @"unwrapConst_typedef1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapConst_typedef1" (BG.Ptr Const_typedef1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_typedef1")

instance HasCField.HasCField Const_typedef1 "unwrapConst_typedef1" where

  type CFieldType Const_typedef1 "unwrapConst_typedef1" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef2@

    __defined at:__ @macros\/reparse.h 219:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef2 = Const_typedef2
  { unwrapConst_typedef2 :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConst_typedef2" Const_typedef2 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_typedef2 {unwrapConst_typedef2 = y1}
      , BG.getField @"unwrapConst_typedef2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapConst_typedef2" (BG.Ptr Const_typedef2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_typedef2")

instance HasCField.HasCField Const_typedef2 "unwrapConst_typedef2" where

  type CFieldType Const_typedef2 "unwrapConst_typedef2" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef3@

    __defined at:__ @macros\/reparse.h 220:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef3 = Const_typedef3
  { unwrapConst_typedef3 :: PtrConst.PtrConst BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConst_typedef3" Const_typedef3 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_typedef3 {unwrapConst_typedef3 = y1}
      , BG.getField @"unwrapConst_typedef3" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "unwrapConst_typedef3" (BG.Ptr Const_typedef3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_typedef3")

instance HasCField.HasCField Const_typedef3 "unwrapConst_typedef3" where

  type CFieldType Const_typedef3 "unwrapConst_typedef3" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef4@

    __defined at:__ @macros\/reparse.h 221:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef4 = Const_typedef4
  { unwrapConst_typedef4 :: PtrConst.PtrConst BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConst_typedef4" Const_typedef4 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_typedef4 {unwrapConst_typedef4 = y1}
      , BG.getField @"unwrapConst_typedef4" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "unwrapConst_typedef4" (BG.Ptr Const_typedef4) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_typedef4")

instance HasCField.HasCField Const_typedef4 "unwrapConst_typedef4" where

  type CFieldType Const_typedef4 "unwrapConst_typedef4" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef5@

    __defined at:__ @macros\/reparse.h 222:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef5 = Const_typedef5
  { unwrapConst_typedef5 :: BG.Ptr BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConst_typedef5" Const_typedef5 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_typedef5 {unwrapConst_typedef5 = y1}
      , BG.getField @"unwrapConst_typedef5" x0
      )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "unwrapConst_typedef5" (BG.Ptr Const_typedef5) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_typedef5")

instance HasCField.HasCField Const_typedef5 "unwrapConst_typedef5" where

  type CFieldType Const_typedef5 "unwrapConst_typedef5" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef6@

    __defined at:__ @macros\/reparse.h 223:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef6 = Const_typedef6
  { unwrapConst_typedef6 :: PtrConst.PtrConst BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConst_typedef6" Const_typedef6 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_typedef6 {unwrapConst_typedef6 = y1}
      , BG.getField @"unwrapConst_typedef6" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "unwrapConst_typedef6" (BG.Ptr Const_typedef6) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_typedef6")

instance HasCField.HasCField Const_typedef6 "unwrapConst_typedef6" where

  type CFieldType Const_typedef6 "unwrapConst_typedef6" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef7@

    __defined at:__ @macros\/reparse.h 224:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef7 = Const_typedef7
  { unwrapConst_typedef7 :: PtrConst.PtrConst BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "unwrapConst_typedef7" Const_typedef7 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_typedef7 {unwrapConst_typedef7 = y1}
      , BG.getField @"unwrapConst_typedef7" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "unwrapConst_typedef7" (BG.Ptr Const_typedef7) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_typedef7")

instance HasCField.HasCField Const_typedef7 "unwrapConst_typedef7" where

  type CFieldType Const_typedef7 "unwrapConst_typedef7" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct example_struct_with_const@

    __defined at:__ @macros\/reparse.h 226:8@

    __exported by:__ @macros\/reparse.h@
-}
data Example_struct_with_const = Example_struct_with_const
  { example_struct_with_const_const_field1 :: BG.CInt
    {- ^ __C declaration:__ @const_field1@

         __defined at:__ @macros\/reparse.h 227:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field2 :: BG.CInt
    {- ^ __C declaration:__ @const_field2@

         __defined at:__ @macros\/reparse.h 228:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field3 :: PtrConst.PtrConst BG.CInt
    {- ^ __C declaration:__ @const_field3@

         __defined at:__ @macros\/reparse.h 229:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field4 :: PtrConst.PtrConst BG.CInt
    {- ^ __C declaration:__ @const_field4@

         __defined at:__ @macros\/reparse.h 230:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field5 :: BG.Ptr BG.CInt
    {- ^ __C declaration:__ @const_field5@

         __defined at:__ @macros\/reparse.h 231:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field6 :: PtrConst.PtrConst BG.CInt
    {- ^ __C declaration:__ @const_field6@

         __defined at:__ @macros\/reparse.h 232:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field7 :: PtrConst.PtrConst BG.CInt
    {- ^ __C declaration:__ @const_field7@

         __defined at:__ @macros\/reparse.h 233:19@

         __exported by:__ @macros\/reparse.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Example_struct_with_const where

  staticSizeOf = \_ -> (48 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Example_struct_with_const where

  readRaw =
    \ptr0 ->
          pure Example_struct_with_const
      <*> HasCField.readRaw (BG.Proxy @"example_struct_with_const_const_field1") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_with_const_const_field2") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_with_const_const_field3") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_with_const_const_field4") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_with_const_const_field5") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_with_const_const_field6") ptr0
      <*> HasCField.readRaw (BG.Proxy @"example_struct_with_const_const_field7") ptr0

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
                 HasCField.writeRaw (BG.Proxy @"example_struct_with_const_const_field1") ptr0 example_struct_with_const_const_field12
              >> HasCField.writeRaw (BG.Proxy @"example_struct_with_const_const_field2") ptr0 example_struct_with_const_const_field23
              >> HasCField.writeRaw (BG.Proxy @"example_struct_with_const_const_field3") ptr0 example_struct_with_const_const_field34
              >> HasCField.writeRaw (BG.Proxy @"example_struct_with_const_const_field4") ptr0 example_struct_with_const_const_field45
              >> HasCField.writeRaw (BG.Proxy @"example_struct_with_const_const_field5") ptr0 example_struct_with_const_const_field56
              >> HasCField.writeRaw (BG.Proxy @"example_struct_with_const_const_field6") ptr0 example_struct_with_const_const_field67
              >> HasCField.writeRaw (BG.Proxy @"example_struct_with_const_const_field7") ptr0 example_struct_with_const_const_field78

deriving via Marshal.EquivStorable Example_struct_with_const instance BG.Storable Example_struct_with_const

{-| __C declaration:__ @const_field1@

    __defined at:__ @macros\/reparse.h 227:19@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_with_const_const_field1" Example_struct_with_const ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct_with_const { example_struct_with_const_const_field1 = y1
                                    , example_struct_with_const_const_field2 = BG.getField @"example_struct_with_const_const_field2" x0
                                    , example_struct_with_const_const_field3 = BG.getField @"example_struct_with_const_const_field3" x0
                                    , example_struct_with_const_const_field4 = BG.getField @"example_struct_with_const_const_field4" x0
                                    , example_struct_with_const_const_field5 = BG.getField @"example_struct_with_const_const_field5" x0
                                    , example_struct_with_const_const_field6 = BG.getField @"example_struct_with_const_const_field6" x0
                                    , example_struct_with_const_const_field7 = BG.getField @"example_struct_with_const_const_field7" x0
                                    }
      , BG.getField @"example_struct_with_const_const_field1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "example_struct_with_const_const_field1" (BG.Ptr Example_struct_with_const) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_with_const_const_field1")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field1" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field1" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_field2@

    __defined at:__ @macros\/reparse.h 228:19@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_with_const_const_field2" Example_struct_with_const ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct_with_const { example_struct_with_const_const_field2 = y1
                                    , example_struct_with_const_const_field1 = BG.getField @"example_struct_with_const_const_field1" x0
                                    , example_struct_with_const_const_field3 = BG.getField @"example_struct_with_const_const_field3" x0
                                    , example_struct_with_const_const_field4 = BG.getField @"example_struct_with_const_const_field4" x0
                                    , example_struct_with_const_const_field5 = BG.getField @"example_struct_with_const_const_field5" x0
                                    , example_struct_with_const_const_field6 = BG.getField @"example_struct_with_const_const_field6" x0
                                    , example_struct_with_const_const_field7 = BG.getField @"example_struct_with_const_const_field7" x0
                                    }
      , BG.getField @"example_struct_with_const_const_field2" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "example_struct_with_const_const_field2" (BG.Ptr Example_struct_with_const) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_with_const_const_field2")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field2" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field2" =
    BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @const_field3@

    __defined at:__ @macros\/reparse.h 229:19@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_with_const_const_field3" Example_struct_with_const ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct_with_const { example_struct_with_const_const_field3 = y1
                                    , example_struct_with_const_const_field1 = BG.getField @"example_struct_with_const_const_field1" x0
                                    , example_struct_with_const_const_field2 = BG.getField @"example_struct_with_const_const_field2" x0
                                    , example_struct_with_const_const_field4 = BG.getField @"example_struct_with_const_const_field4" x0
                                    , example_struct_with_const_const_field5 = BG.getField @"example_struct_with_const_const_field5" x0
                                    , example_struct_with_const_const_field6 = BG.getField @"example_struct_with_const_const_field6" x0
                                    , example_struct_with_const_const_field7 = BG.getField @"example_struct_with_const_const_field7" x0
                                    }
      , BG.getField @"example_struct_with_const_const_field3" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "example_struct_with_const_const_field3" (BG.Ptr Example_struct_with_const) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_with_const_const_field3")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field3" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field3" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @const_field4@

    __defined at:__ @macros\/reparse.h 230:19@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_with_const_const_field4" Example_struct_with_const ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct_with_const { example_struct_with_const_const_field4 = y1
                                    , example_struct_with_const_const_field1 = BG.getField @"example_struct_with_const_const_field1" x0
                                    , example_struct_with_const_const_field2 = BG.getField @"example_struct_with_const_const_field2" x0
                                    , example_struct_with_const_const_field3 = BG.getField @"example_struct_with_const_const_field3" x0
                                    , example_struct_with_const_const_field5 = BG.getField @"example_struct_with_const_const_field5" x0
                                    , example_struct_with_const_const_field6 = BG.getField @"example_struct_with_const_const_field6" x0
                                    , example_struct_with_const_const_field7 = BG.getField @"example_struct_with_const_const_field7" x0
                                    }
      , BG.getField @"example_struct_with_const_const_field4" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "example_struct_with_const_const_field4" (BG.Ptr Example_struct_with_const) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_with_const_const_field4")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field4" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field4" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @const_field5@

    __defined at:__ @macros\/reparse.h 231:19@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_with_const_const_field5" Example_struct_with_const ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct_with_const { example_struct_with_const_const_field5 = y1
                                    , example_struct_with_const_const_field1 = BG.getField @"example_struct_with_const_const_field1" x0
                                    , example_struct_with_const_const_field2 = BG.getField @"example_struct_with_const_const_field2" x0
                                    , example_struct_with_const_const_field3 = BG.getField @"example_struct_with_const_const_field3" x0
                                    , example_struct_with_const_const_field4 = BG.getField @"example_struct_with_const_const_field4" x0
                                    , example_struct_with_const_const_field6 = BG.getField @"example_struct_with_const_const_field6" x0
                                    , example_struct_with_const_const_field7 = BG.getField @"example_struct_with_const_const_field7" x0
                                    }
      , BG.getField @"example_struct_with_const_const_field5" x0
      )

instance ( ty ~ BG.Ptr BG.CInt
         ) => BG.HasField "example_struct_with_const_const_field5" (BG.Ptr Example_struct_with_const) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_with_const_const_field5")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field5" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field5" =
    BG.Ptr BG.CInt

  offset# = \_ -> \_ -> 24

{-| __C declaration:__ @const_field6@

    __defined at:__ @macros\/reparse.h 232:19@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_with_const_const_field6" Example_struct_with_const ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct_with_const { example_struct_with_const_const_field6 = y1
                                    , example_struct_with_const_const_field1 = BG.getField @"example_struct_with_const_const_field1" x0
                                    , example_struct_with_const_const_field2 = BG.getField @"example_struct_with_const_const_field2" x0
                                    , example_struct_with_const_const_field3 = BG.getField @"example_struct_with_const_const_field3" x0
                                    , example_struct_with_const_const_field4 = BG.getField @"example_struct_with_const_const_field4" x0
                                    , example_struct_with_const_const_field5 = BG.getField @"example_struct_with_const_const_field5" x0
                                    , example_struct_with_const_const_field7 = BG.getField @"example_struct_with_const_const_field7" x0
                                    }
      , BG.getField @"example_struct_with_const_const_field6" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "example_struct_with_const_const_field6" (BG.Ptr Example_struct_with_const) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_with_const_const_field6")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field6" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field6" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 32

{-| __C declaration:__ @const_field7@

    __defined at:__ @macros\/reparse.h 233:19@

    __exported by:__ @macros\/reparse.h@
-}
instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.CompatHasField.HasField "example_struct_with_const_const_field7" Example_struct_with_const ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Example_struct_with_const { example_struct_with_const_const_field7 = y1
                                    , example_struct_with_const_const_field1 = BG.getField @"example_struct_with_const_const_field1" x0
                                    , example_struct_with_const_const_field2 = BG.getField @"example_struct_with_const_const_field2" x0
                                    , example_struct_with_const_const_field3 = BG.getField @"example_struct_with_const_const_field3" x0
                                    , example_struct_with_const_const_field4 = BG.getField @"example_struct_with_const_const_field4" x0
                                    , example_struct_with_const_const_field5 = BG.getField @"example_struct_with_const_const_field5" x0
                                    , example_struct_with_const_const_field6 = BG.getField @"example_struct_with_const_const_field6" x0
                                    }
      , BG.getField @"example_struct_with_const_const_field7" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CInt
         ) => BG.HasField "example_struct_with_const_const_field7" (BG.Ptr Example_struct_with_const) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"example_struct_with_const_const_field7")

instance HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field7" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field7" =
    PtrConst.PtrConst BG.CInt

  offset# = \_ -> \_ -> 40

{-| Auxiliary type used by 'Const_funptr1'

    __C declaration:__ @const_funptr1@

    __defined at:__ @macros\/reparse.h 236:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1_Aux = Const_funptr1_Aux
  { unwrapConst_funptr1_Aux :: BG.CInt -> BG.CDouble -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toConst_funptr1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_7f125e20a9d4075b_base ::
     (BG.Int32 -> Double -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO BG.Int32))

-- __unique:__ @toConst_funptr1_Aux@
hs_bindgen_7f125e20a9d4075b ::
     Const_funptr1_Aux
  -> IO (BG.FunPtr Const_funptr1_Aux)
hs_bindgen_7f125e20a9d4075b =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_7f125e20a9d4075b_base (BG.toFFIType fun0))

-- __unique:__ @fromConst_funptr1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ac4bd8d789bba94b_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO BG.Int32)
  -> BG.Int32 -> Double -> IO BG.Int32

-- __unique:__ @fromConst_funptr1_Aux@
hs_bindgen_ac4bd8d789bba94b ::
     BG.FunPtr Const_funptr1_Aux
  -> Const_funptr1_Aux
hs_bindgen_ac4bd8d789bba94b =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_ac4bd8d789bba94b_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Const_funptr1_Aux where

  toFunPtr = hs_bindgen_7f125e20a9d4075b

instance BG.FromFunPtr Const_funptr1_Aux where

  fromFunPtr = hs_bindgen_ac4bd8d789bba94b

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapConst_funptr1_Aux" Const_funptr1_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Const_funptr1_Aux {unwrapConst_funptr1_Aux = y1}
      , BG.getField @"unwrapConst_funptr1_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO BG.CInt)
         ) => BG.HasField "unwrapConst_funptr1_Aux" (BG.Ptr Const_funptr1_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr1_Aux")

instance HasCField.HasCField Const_funptr1_Aux "unwrapConst_funptr1_Aux" where

  type CFieldType Const_funptr1_Aux "unwrapConst_funptr1_Aux" =
    BG.CInt -> BG.CDouble -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr1@

    __defined at:__ @macros\/reparse.h 236:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1 = Const_funptr1
  { unwrapConst_funptr1 :: BG.FunPtr Const_funptr1_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Const_funptr1_Aux
         ) => BG.CompatHasField.HasField "unwrapConst_funptr1" Const_funptr1 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_funptr1 {unwrapConst_funptr1 = y1}
      , BG.getField @"unwrapConst_funptr1" x0
      )

instance ( ty ~ BG.FunPtr Const_funptr1_Aux
         ) => BG.HasField "unwrapConst_funptr1" (BG.Ptr Const_funptr1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr1")

instance HasCField.HasCField Const_funptr1 "unwrapConst_funptr1" where

  type CFieldType Const_funptr1 "unwrapConst_funptr1" =
    BG.FunPtr Const_funptr1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr2'

    __C declaration:__ @const_funptr2@

    __defined at:__ @macros\/reparse.h 237:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2_Aux = Const_funptr2_Aux
  { unwrapConst_funptr2_Aux :: BG.CInt -> BG.CDouble -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toConst_funptr2_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c7b1e36d845634fb_base ::
     (BG.Int32 -> Double -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO BG.Int32))

-- __unique:__ @toConst_funptr2_Aux@
hs_bindgen_c7b1e36d845634fb ::
     Const_funptr2_Aux
  -> IO (BG.FunPtr Const_funptr2_Aux)
hs_bindgen_c7b1e36d845634fb =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_c7b1e36d845634fb_base (BG.toFFIType fun0))

-- __unique:__ @fromConst_funptr2_Aux@
foreign import ccall safe "dynamic" hs_bindgen_352cebf463125ca9_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO BG.Int32)
  -> BG.Int32 -> Double -> IO BG.Int32

-- __unique:__ @fromConst_funptr2_Aux@
hs_bindgen_352cebf463125ca9 ::
     BG.FunPtr Const_funptr2_Aux
  -> Const_funptr2_Aux
hs_bindgen_352cebf463125ca9 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_352cebf463125ca9_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Const_funptr2_Aux where

  toFunPtr = hs_bindgen_c7b1e36d845634fb

instance BG.FromFunPtr Const_funptr2_Aux where

  fromFunPtr = hs_bindgen_352cebf463125ca9

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapConst_funptr2_Aux" Const_funptr2_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Const_funptr2_Aux {unwrapConst_funptr2_Aux = y1}
      , BG.getField @"unwrapConst_funptr2_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO BG.CInt)
         ) => BG.HasField "unwrapConst_funptr2_Aux" (BG.Ptr Const_funptr2_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr2_Aux")

instance HasCField.HasCField Const_funptr2_Aux "unwrapConst_funptr2_Aux" where

  type CFieldType Const_funptr2_Aux "unwrapConst_funptr2_Aux" =
    BG.CInt -> BG.CDouble -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr2@

    __defined at:__ @macros\/reparse.h 237:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2 = Const_funptr2
  { unwrapConst_funptr2 :: BG.FunPtr Const_funptr2_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Const_funptr2_Aux
         ) => BG.CompatHasField.HasField "unwrapConst_funptr2" Const_funptr2 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_funptr2 {unwrapConst_funptr2 = y1}
      , BG.getField @"unwrapConst_funptr2" x0
      )

instance ( ty ~ BG.FunPtr Const_funptr2_Aux
         ) => BG.HasField "unwrapConst_funptr2" (BG.Ptr Const_funptr2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr2")

instance HasCField.HasCField Const_funptr2 "unwrapConst_funptr2" where

  type CFieldType Const_funptr2 "unwrapConst_funptr2" =
    BG.FunPtr Const_funptr2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr3'

    __C declaration:__ @const_funptr3@

    __defined at:__ @macros\/reparse.h 238:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3_Aux = Const_funptr3_Aux
  { unwrapConst_funptr3_Aux :: BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toConst_funptr3_Aux@
foreign import ccall safe "wrapper" hs_bindgen_2dcbfe1c2502178c_base ::
     (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr3_Aux@
hs_bindgen_2dcbfe1c2502178c ::
     Const_funptr3_Aux
  -> IO (BG.FunPtr Const_funptr3_Aux)
hs_bindgen_2dcbfe1c2502178c =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_2dcbfe1c2502178c_base (BG.toFFIType fun0))

-- __unique:__ @fromConst_funptr3_Aux@
foreign import ccall safe "dynamic" hs_bindgen_86738dcfd7c9d33c_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> BG.Int32 -> Double -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr3_Aux@
hs_bindgen_86738dcfd7c9d33c ::
     BG.FunPtr Const_funptr3_Aux
  -> Const_funptr3_Aux
hs_bindgen_86738dcfd7c9d33c =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_86738dcfd7c9d33c_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Const_funptr3_Aux where

  toFunPtr = hs_bindgen_2dcbfe1c2502178c

instance BG.FromFunPtr Const_funptr3_Aux where

  fromFunPtr = hs_bindgen_86738dcfd7c9d33c

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.CompatHasField.HasField "unwrapConst_funptr3_Aux" Const_funptr3_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Const_funptr3_Aux {unwrapConst_funptr3_Aux = y1}
      , BG.getField @"unwrapConst_funptr3_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.HasField "unwrapConst_funptr3_Aux" (BG.Ptr Const_funptr3_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr3_Aux")

instance HasCField.HasCField Const_funptr3_Aux "unwrapConst_funptr3_Aux" where

  type CFieldType Const_funptr3_Aux "unwrapConst_funptr3_Aux" =
    BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr3@

    __defined at:__ @macros\/reparse.h 238:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3 = Const_funptr3
  { unwrapConst_funptr3 :: BG.FunPtr Const_funptr3_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Const_funptr3_Aux
         ) => BG.CompatHasField.HasField "unwrapConst_funptr3" Const_funptr3 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_funptr3 {unwrapConst_funptr3 = y1}
      , BG.getField @"unwrapConst_funptr3" x0
      )

instance ( ty ~ BG.FunPtr Const_funptr3_Aux
         ) => BG.HasField "unwrapConst_funptr3" (BG.Ptr Const_funptr3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr3")

instance HasCField.HasCField Const_funptr3 "unwrapConst_funptr3" where

  type CFieldType Const_funptr3 "unwrapConst_funptr3" =
    BG.FunPtr Const_funptr3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr4'

    __C declaration:__ @const_funptr4@

    __defined at:__ @macros\/reparse.h 239:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4_Aux = Const_funptr4_Aux
  { unwrapConst_funptr4_Aux :: BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toConst_funptr4_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5461deeda491de0b_base ::
     (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr4_Aux@
hs_bindgen_5461deeda491de0b ::
     Const_funptr4_Aux
  -> IO (BG.FunPtr Const_funptr4_Aux)
hs_bindgen_5461deeda491de0b =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_5461deeda491de0b_base (BG.toFFIType fun0))

-- __unique:__ @fromConst_funptr4_Aux@
foreign import ccall safe "dynamic" hs_bindgen_de7846fca3bfd1b6_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> BG.Int32 -> Double -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr4_Aux@
hs_bindgen_de7846fca3bfd1b6 ::
     BG.FunPtr Const_funptr4_Aux
  -> Const_funptr4_Aux
hs_bindgen_de7846fca3bfd1b6 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_de7846fca3bfd1b6_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Const_funptr4_Aux where

  toFunPtr = hs_bindgen_5461deeda491de0b

instance BG.FromFunPtr Const_funptr4_Aux where

  fromFunPtr = hs_bindgen_de7846fca3bfd1b6

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.CompatHasField.HasField "unwrapConst_funptr4_Aux" Const_funptr4_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Const_funptr4_Aux {unwrapConst_funptr4_Aux = y1}
      , BG.getField @"unwrapConst_funptr4_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.HasField "unwrapConst_funptr4_Aux" (BG.Ptr Const_funptr4_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr4_Aux")

instance HasCField.HasCField Const_funptr4_Aux "unwrapConst_funptr4_Aux" where

  type CFieldType Const_funptr4_Aux "unwrapConst_funptr4_Aux" =
    BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr4@

    __defined at:__ @macros\/reparse.h 239:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4 = Const_funptr4
  { unwrapConst_funptr4 :: BG.FunPtr Const_funptr4_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Const_funptr4_Aux
         ) => BG.CompatHasField.HasField "unwrapConst_funptr4" Const_funptr4 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_funptr4 {unwrapConst_funptr4 = y1}
      , BG.getField @"unwrapConst_funptr4" x0
      )

instance ( ty ~ BG.FunPtr Const_funptr4_Aux
         ) => BG.HasField "unwrapConst_funptr4" (BG.Ptr Const_funptr4) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr4")

instance HasCField.HasCField Const_funptr4 "unwrapConst_funptr4" where

  type CFieldType Const_funptr4 "unwrapConst_funptr4" =
    BG.FunPtr Const_funptr4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr5'

    __C declaration:__ @const_funptr5@

    __defined at:__ @macros\/reparse.h 240:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5_Aux = Const_funptr5_Aux
  { unwrapConst_funptr5_Aux :: BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toConst_funptr5_Aux@
foreign import ccall safe "wrapper" hs_bindgen_7b0174fc978a1ce1_base ::
     (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr5_Aux@
hs_bindgen_7b0174fc978a1ce1 ::
     Const_funptr5_Aux
  -> IO (BG.FunPtr Const_funptr5_Aux)
hs_bindgen_7b0174fc978a1ce1 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_7b0174fc978a1ce1_base (BG.toFFIType fun0))

-- __unique:__ @fromConst_funptr5_Aux@
foreign import ccall safe "dynamic" hs_bindgen_38a21d84bb7115b5_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> BG.Int32 -> Double -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr5_Aux@
hs_bindgen_38a21d84bb7115b5 ::
     BG.FunPtr Const_funptr5_Aux
  -> Const_funptr5_Aux
hs_bindgen_38a21d84bb7115b5 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_38a21d84bb7115b5_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Const_funptr5_Aux where

  toFunPtr = hs_bindgen_7b0174fc978a1ce1

instance BG.FromFunPtr Const_funptr5_Aux where

  fromFunPtr = hs_bindgen_38a21d84bb7115b5

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
         ) => BG.CompatHasField.HasField "unwrapConst_funptr5_Aux" Const_funptr5_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Const_funptr5_Aux {unwrapConst_funptr5_Aux = y1}
      , BG.getField @"unwrapConst_funptr5_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt))
         ) => BG.HasField "unwrapConst_funptr5_Aux" (BG.Ptr Const_funptr5_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr5_Aux")

instance HasCField.HasCField Const_funptr5_Aux "unwrapConst_funptr5_Aux" where

  type CFieldType Const_funptr5_Aux "unwrapConst_funptr5_Aux" =
    BG.CInt -> BG.CDouble -> IO (BG.Ptr BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr5@

    __defined at:__ @macros\/reparse.h 240:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5 = Const_funptr5
  { unwrapConst_funptr5 :: BG.FunPtr Const_funptr5_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Const_funptr5_Aux
         ) => BG.CompatHasField.HasField "unwrapConst_funptr5" Const_funptr5 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_funptr5 {unwrapConst_funptr5 = y1}
      , BG.getField @"unwrapConst_funptr5" x0
      )

instance ( ty ~ BG.FunPtr Const_funptr5_Aux
         ) => BG.HasField "unwrapConst_funptr5" (BG.Ptr Const_funptr5) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr5")

instance HasCField.HasCField Const_funptr5 "unwrapConst_funptr5" where

  type CFieldType Const_funptr5 "unwrapConst_funptr5" =
    BG.FunPtr Const_funptr5_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr6'

    __C declaration:__ @const_funptr6@

    __defined at:__ @macros\/reparse.h 241:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6_Aux = Const_funptr6_Aux
  { unwrapConst_funptr6_Aux :: BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toConst_funptr6_Aux@
foreign import ccall safe "wrapper" hs_bindgen_4e32721222f4df9f_base ::
     (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr6_Aux@
hs_bindgen_4e32721222f4df9f ::
     Const_funptr6_Aux
  -> IO (BG.FunPtr Const_funptr6_Aux)
hs_bindgen_4e32721222f4df9f =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_4e32721222f4df9f_base (BG.toFFIType fun0))

-- __unique:__ @fromConst_funptr6_Aux@
foreign import ccall safe "dynamic" hs_bindgen_45251216b04aa8b5_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> BG.Int32 -> Double -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr6_Aux@
hs_bindgen_45251216b04aa8b5 ::
     BG.FunPtr Const_funptr6_Aux
  -> Const_funptr6_Aux
hs_bindgen_45251216b04aa8b5 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_45251216b04aa8b5_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Const_funptr6_Aux where

  toFunPtr = hs_bindgen_4e32721222f4df9f

instance BG.FromFunPtr Const_funptr6_Aux where

  fromFunPtr = hs_bindgen_45251216b04aa8b5

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.CompatHasField.HasField "unwrapConst_funptr6_Aux" Const_funptr6_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Const_funptr6_Aux {unwrapConst_funptr6_Aux = y1}
      , BG.getField @"unwrapConst_funptr6_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.HasField "unwrapConst_funptr6_Aux" (BG.Ptr Const_funptr6_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr6_Aux")

instance HasCField.HasCField Const_funptr6_Aux "unwrapConst_funptr6_Aux" where

  type CFieldType Const_funptr6_Aux "unwrapConst_funptr6_Aux" =
    BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr6@

    __defined at:__ @macros\/reparse.h 241:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6 = Const_funptr6
  { unwrapConst_funptr6 :: BG.FunPtr Const_funptr6_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Const_funptr6_Aux
         ) => BG.CompatHasField.HasField "unwrapConst_funptr6" Const_funptr6 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_funptr6 {unwrapConst_funptr6 = y1}
      , BG.getField @"unwrapConst_funptr6" x0
      )

instance ( ty ~ BG.FunPtr Const_funptr6_Aux
         ) => BG.HasField "unwrapConst_funptr6" (BG.Ptr Const_funptr6) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr6")

instance HasCField.HasCField Const_funptr6 "unwrapConst_funptr6" where

  type CFieldType Const_funptr6 "unwrapConst_funptr6" =
    BG.FunPtr Const_funptr6_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr7'

    __C declaration:__ @const_funptr7@

    __defined at:__ @macros\/reparse.h 242:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7_Aux = Const_funptr7_Aux
  { unwrapConst_funptr7_Aux :: BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toConst_funptr7_Aux@
foreign import ccall safe "wrapper" hs_bindgen_0d04fc96ffb9de06_base ::
     (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> IO (BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void)))

-- __unique:__ @toConst_funptr7_Aux@
hs_bindgen_0d04fc96ffb9de06 ::
     Const_funptr7_Aux
  -> IO (BG.FunPtr Const_funptr7_Aux)
hs_bindgen_0d04fc96ffb9de06 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_0d04fc96ffb9de06_base (BG.toFFIType fun0))

-- __unique:__ @fromConst_funptr7_Aux@
foreign import ccall safe "dynamic" hs_bindgen_42fbcebf75a973ba_base ::
     BG.FunPtr (BG.Int32 -> Double -> IO (BG.Ptr BG.Void))
  -> BG.Int32 -> Double -> IO (BG.Ptr BG.Void)

-- __unique:__ @fromConst_funptr7_Aux@
hs_bindgen_42fbcebf75a973ba ::
     BG.FunPtr Const_funptr7_Aux
  -> Const_funptr7_Aux
hs_bindgen_42fbcebf75a973ba =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_42fbcebf75a973ba_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Const_funptr7_Aux where

  toFunPtr = hs_bindgen_0d04fc96ffb9de06

instance BG.FromFunPtr Const_funptr7_Aux where

  fromFunPtr = hs_bindgen_42fbcebf75a973ba

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.CompatHasField.HasField "unwrapConst_funptr7_Aux" Const_funptr7_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Const_funptr7_Aux {unwrapConst_funptr7_Aux = y1}
      , BG.getField @"unwrapConst_funptr7_Aux" x0
      )

instance ( ty ~ (BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt))
         ) => BG.HasField "unwrapConst_funptr7_Aux" (BG.Ptr Const_funptr7_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr7_Aux")

instance HasCField.HasCField Const_funptr7_Aux "unwrapConst_funptr7_Aux" where

  type CFieldType Const_funptr7_Aux "unwrapConst_funptr7_Aux" =
    BG.CInt -> BG.CDouble -> IO (PtrConst.PtrConst BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr7@

    __defined at:__ @macros\/reparse.h 242:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7 = Const_funptr7
  { unwrapConst_funptr7 :: BG.FunPtr Const_funptr7_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Const_funptr7_Aux
         ) => BG.CompatHasField.HasField "unwrapConst_funptr7" Const_funptr7 ty where

  hasField =
    \x0 ->
      ( \y1 -> Const_funptr7 {unwrapConst_funptr7 = y1}
      , BG.getField @"unwrapConst_funptr7" x0
      )

instance ( ty ~ BG.FunPtr Const_funptr7_Aux
         ) => BG.HasField "unwrapConst_funptr7" (BG.Ptr Const_funptr7) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConst_funptr7")

instance HasCField.HasCField Const_funptr7 "unwrapConst_funptr7" where

  type CFieldType Const_funptr7 "unwrapConst_funptr7" =
    BG.FunPtr Const_funptr7_Aux

  offset# = \_ -> \_ -> 0

{-| Macro-defined types

    __C declaration:__ @macro BOOL@

    __defined at:__ @macros\/reparse.h 278:9@

    __exported by:__ @macros\/reparse.h@
-}
bOOL :: [String]
bOOL = ["_Bool"]

{-| __C declaration:__ @macro INT@

    __defined at:__ @macros\/reparse.h 279:9@

    __exported by:__ @macros\/reparse.h@
-}
iNT :: [String]
iNT = ["int"]

{-| __C declaration:__ @macro INTP@

    __defined at:__ @macros\/reparse.h 280:9@

    __exported by:__ @macros\/reparse.h@
-}
iNTP :: [String]
iNTP = ["int", "*"]

{-| __C declaration:__ @macro INTCP@

    __defined at:__ @macros\/reparse.h 281:9@

    __exported by:__ @macros\/reparse.h@
-}
iNTCP :: [String]
iNTCP = ["const", "int", "*", "const"]
