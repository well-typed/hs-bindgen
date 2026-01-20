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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Prelude as P
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return, showsPrec)

{-| __C declaration:__ @A@

    __defined at:__ @macros\/reparse.h 3:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype A = A
  { un_A :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @struct some_struct@

    __defined at:__ @macros\/reparse.h 7:8@

    __exported by:__ @macros\/reparse.h@
-}
data Some_struct = Some_struct
  {}
  deriving stock (Eq, Show)

instance F.Storable Some_struct where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Some_struct

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct -> return ()

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
  { un_Some_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance F.Storable Some_union

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance Data.Primitive.Types.Prim Some_union

{-| __C declaration:__ @enum some_enum@

    __defined at:__ @macros\/reparse.h 9:6@

    __exported by:__ @macros\/reparse.h@
-}
newtype Some_enum = Some_enum
  { un_Some_enum :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance F.Storable Some_enum where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Some_enum
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_enum un_Some_enum2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Some_enum2

deriving via FC.CUInt instance Data.Primitive.Types.Prim Some_enum

instance HsBindgen.Runtime.CEnum.CEnum Some_enum where

  type CEnumZ Some_enum = FC.CUInt

  toCEnum = Some_enum

  fromCEnum = un_Some_enum

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

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Some_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

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
  { un_Arr_typedef1 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray A
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef1) "un_Arr_typedef1")
         ) => GHC.Records.HasField "un_Arr_typedef1" (Ptr.Ptr Arr_typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Arr_typedef1")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef1 "un_Arr_typedef1" where

  type CFieldType Arr_typedef1 "un_Arr_typedef1" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef2@

    __defined at:__ @macros\/reparse.h 110:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef2 = Arr_typedef2
  { un_Arr_typedef2 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef2) "un_Arr_typedef2")
         ) => GHC.Records.HasField "un_Arr_typedef2" (Ptr.Ptr Arr_typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Arr_typedef2")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef2 "un_Arr_typedef2" where

  type CFieldType Arr_typedef2 "un_Arr_typedef2" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef3@

    __defined at:__ @macros\/reparse.h 111:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef3 = Arr_typedef3
  { un_Arr_typedef3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) A
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef3) "un_Arr_typedef3")
         ) => GHC.Records.HasField "un_Arr_typedef3" (Ptr.Ptr Arr_typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Arr_typedef3")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef3 "un_Arr_typedef3" where

  type CFieldType Arr_typedef3 "un_Arr_typedef3" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 5) A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @arr_typedef4@

    __defined at:__ @macros\/reparse.h 112:13@

    __exported by:__ @macros\/reparse.h@
-}
newtype Arr_typedef4 = Arr_typedef4
  { un_Arr_typedef4 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Arr_typedef4) "un_Arr_typedef4")
         ) => GHC.Records.HasField "un_Arr_typedef4" (Ptr.Ptr Arr_typedef4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Arr_typedef4")

instance HsBindgen.Runtime.HasCField.HasCField Arr_typedef4 "un_Arr_typedef4" where

  type CFieldType Arr_typedef4 "un_Arr_typedef4" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| Typedefs

__C declaration:__ @typedef1@

__defined at:__ @macros\/reparse.h 118:14@

__exported by:__ @macros\/reparse.h@
-}
newtype Typedef1 = Typedef1
  { un_Typedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Typedef1) "un_Typedef1")
         ) => GHC.Records.HasField "un_Typedef1" (Ptr.Ptr Typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Typedef1")

instance HsBindgen.Runtime.HasCField.HasCField Typedef1 "un_Typedef1" where

  type CFieldType Typedef1 "un_Typedef1" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef2@

    __defined at:__ @macros\/reparse.h 119:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef2 = Typedef2
  { un_Typedef2 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Typedef2) "un_Typedef2")
         ) => GHC.Records.HasField "un_Typedef2" (Ptr.Ptr Typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Typedef2")

instance HsBindgen.Runtime.HasCField.HasCField Typedef2 "un_Typedef2" where

  type CFieldType Typedef2 "un_Typedef2" = Ptr.Ptr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef3@

    __defined at:__ @macros\/reparse.h 120:14@

    __exported by:__ @macros\/reparse.h@
-}
newtype Typedef3 = Typedef3
  { un_Typedef3 :: Ptr.Ptr (Ptr.Ptr A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Typedef3) "un_Typedef3")
         ) => GHC.Records.HasField "un_Typedef3" (Ptr.Ptr Typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Typedef3")

instance HsBindgen.Runtime.HasCField.HasCField Typedef3 "un_Typedef3" where

  type CFieldType Typedef3 "un_Typedef3" =
    Ptr.Ptr (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef1'

__C declaration:__ @funptr_typedef1@

__defined at:__ @macros\/reparse.h 132:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1_Aux = Funptr_typedef1_Aux
  { un_Funptr_typedef1_Aux :: IO A
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_c584d0f839fd43de_base ::
     IO FC.CInt
  -> IO (Ptr.FunPtr (IO FC.CInt))

-- __unique:__ @toFunptr_typedef1_Aux@
hs_bindgen_c584d0f839fd43de ::
     Funptr_typedef1_Aux
  -> IO (Ptr.FunPtr Funptr_typedef1_Aux)
hs_bindgen_c584d0f839fd43de =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_c584d0f839fd43de_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_806a46dc418a062c_base ::
     Ptr.FunPtr (IO FC.CInt)
  -> IO FC.CInt

-- __unique:__ @fromFunptr_typedef1_Aux@
hs_bindgen_806a46dc418a062c ::
     Ptr.FunPtr Funptr_typedef1_Aux
  -> Funptr_typedef1_Aux
hs_bindgen_806a46dc418a062c =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_806a46dc418a062c_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef1_Aux where

  toFunPtr = hs_bindgen_c584d0f839fd43de

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef1_Aux where

  fromFunPtr = hs_bindgen_806a46dc418a062c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef1_Aux) "un_Funptr_typedef1_Aux")
         ) => GHC.Records.HasField "un_Funptr_typedef1_Aux" (Ptr.Ptr Funptr_typedef1_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef1_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef1_Aux "un_Funptr_typedef1_Aux" where

  type CFieldType Funptr_typedef1_Aux "un_Funptr_typedef1_Aux" =
    IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef1@

    __defined at:__ @macros\/reparse.h 132:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef1 = Funptr_typedef1
  { un_Funptr_typedef1 :: Ptr.FunPtr Funptr_typedef1_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef1) "un_Funptr_typedef1")
         ) => GHC.Records.HasField "un_Funptr_typedef1" (Ptr.Ptr Funptr_typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef1")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef1 "un_Funptr_typedef1" where

  type CFieldType Funptr_typedef1 "un_Funptr_typedef1" =
    Ptr.FunPtr Funptr_typedef1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef2'

__C declaration:__ @funptr_typedef2@

__defined at:__ @macros\/reparse.h 133:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2_Aux = Funptr_typedef2_Aux
  { un_Funptr_typedef2_Aux :: IO (Ptr.Ptr A)
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_f174457a161ac5a0_base ::
     IO (Ptr.Ptr Void)
  -> IO (Ptr.FunPtr (IO (Ptr.Ptr Void)))

-- __unique:__ @toFunptr_typedef2_Aux@
hs_bindgen_f174457a161ac5a0 ::
     Funptr_typedef2_Aux
  -> IO (Ptr.FunPtr Funptr_typedef2_Aux)
hs_bindgen_f174457a161ac5a0 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_f174457a161ac5a0_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_323d07dff85b802c_base ::
     Ptr.FunPtr (IO (Ptr.Ptr Void))
  -> IO (Ptr.Ptr Void)

-- __unique:__ @fromFunptr_typedef2_Aux@
hs_bindgen_323d07dff85b802c ::
     Ptr.FunPtr Funptr_typedef2_Aux
  -> Funptr_typedef2_Aux
hs_bindgen_323d07dff85b802c =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_323d07dff85b802c_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef2_Aux where

  toFunPtr = hs_bindgen_f174457a161ac5a0

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef2_Aux where

  fromFunPtr = hs_bindgen_323d07dff85b802c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef2_Aux) "un_Funptr_typedef2_Aux")
         ) => GHC.Records.HasField "un_Funptr_typedef2_Aux" (Ptr.Ptr Funptr_typedef2_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef2_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef2_Aux "un_Funptr_typedef2_Aux" where

  type CFieldType Funptr_typedef2_Aux "un_Funptr_typedef2_Aux" =
    IO (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef2@

    __defined at:__ @macros\/reparse.h 133:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef2 = Funptr_typedef2
  { un_Funptr_typedef2 :: Ptr.FunPtr Funptr_typedef2_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef2) "un_Funptr_typedef2")
         ) => GHC.Records.HasField "un_Funptr_typedef2" (Ptr.Ptr Funptr_typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef2")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef2 "un_Funptr_typedef2" where

  type CFieldType Funptr_typedef2 "un_Funptr_typedef2" =
    Ptr.FunPtr Funptr_typedef2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef3'

__C declaration:__ @funptr_typedef3@

__defined at:__ @macros\/reparse.h 134:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3_Aux = Funptr_typedef3_Aux
  { un_Funptr_typedef3_Aux :: IO (Ptr.Ptr (Ptr.Ptr A))
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_031d1a7decd790d8_base ::
     IO (Ptr.Ptr Void)
  -> IO (Ptr.FunPtr (IO (Ptr.Ptr Void)))

-- __unique:__ @toFunptr_typedef3_Aux@
hs_bindgen_031d1a7decd790d8 ::
     Funptr_typedef3_Aux
  -> IO (Ptr.FunPtr Funptr_typedef3_Aux)
hs_bindgen_031d1a7decd790d8 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_031d1a7decd790d8_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_82dc7b932974117e_base ::
     Ptr.FunPtr (IO (Ptr.Ptr Void))
  -> IO (Ptr.Ptr Void)

-- __unique:__ @fromFunptr_typedef3_Aux@
hs_bindgen_82dc7b932974117e ::
     Ptr.FunPtr Funptr_typedef3_Aux
  -> Funptr_typedef3_Aux
hs_bindgen_82dc7b932974117e =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_82dc7b932974117e_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef3_Aux where

  toFunPtr = hs_bindgen_031d1a7decd790d8

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef3_Aux where

  fromFunPtr = hs_bindgen_82dc7b932974117e

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef3_Aux) "un_Funptr_typedef3_Aux")
         ) => GHC.Records.HasField "un_Funptr_typedef3_Aux" (Ptr.Ptr Funptr_typedef3_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef3_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef3_Aux "un_Funptr_typedef3_Aux" where

  type CFieldType Funptr_typedef3_Aux "un_Funptr_typedef3_Aux" =
    IO (Ptr.Ptr (Ptr.Ptr A))

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef3@

    __defined at:__ @macros\/reparse.h 134:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef3 = Funptr_typedef3
  { un_Funptr_typedef3 :: Ptr.FunPtr Funptr_typedef3_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef3) "un_Funptr_typedef3")
         ) => GHC.Records.HasField "un_Funptr_typedef3" (Ptr.Ptr Funptr_typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef3")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef3 "un_Funptr_typedef3" where

  type CFieldType Funptr_typedef3 "un_Funptr_typedef3" =
    Ptr.FunPtr Funptr_typedef3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef4'

__C declaration:__ @funptr_typedef4@

__defined at:__ @macros\/reparse.h 135:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4_Aux = Funptr_typedef4_Aux
  { un_Funptr_typedef4_Aux :: FC.CInt -> FC.CDouble -> IO A
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_da2336d254667386_base ::
     (FC.CInt -> FC.CDouble -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CInt))

-- __unique:__ @toFunptr_typedef4_Aux@
hs_bindgen_da2336d254667386 ::
     Funptr_typedef4_Aux
  -> IO (Ptr.FunPtr Funptr_typedef4_Aux)
hs_bindgen_da2336d254667386 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_da2336d254667386_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_d4a97954476da161_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CInt)
  -> FC.CInt -> FC.CDouble -> IO FC.CInt

-- __unique:__ @fromFunptr_typedef4_Aux@
hs_bindgen_d4a97954476da161 ::
     Ptr.FunPtr Funptr_typedef4_Aux
  -> Funptr_typedef4_Aux
hs_bindgen_d4a97954476da161 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_d4a97954476da161_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef4_Aux where

  toFunPtr = hs_bindgen_da2336d254667386

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef4_Aux where

  fromFunPtr = hs_bindgen_d4a97954476da161

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef4_Aux) "un_Funptr_typedef4_Aux")
         ) => GHC.Records.HasField "un_Funptr_typedef4_Aux" (Ptr.Ptr Funptr_typedef4_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef4_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef4_Aux "un_Funptr_typedef4_Aux" where

  type CFieldType Funptr_typedef4_Aux "un_Funptr_typedef4_Aux" =
    FC.CInt -> FC.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef4@

    __defined at:__ @macros\/reparse.h 135:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef4 = Funptr_typedef4
  { un_Funptr_typedef4 :: Ptr.FunPtr Funptr_typedef4_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef4) "un_Funptr_typedef4")
         ) => GHC.Records.HasField "un_Funptr_typedef4" (Ptr.Ptr Funptr_typedef4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef4")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef4 "un_Funptr_typedef4" where

  type CFieldType Funptr_typedef4 "un_Funptr_typedef4" =
    Ptr.FunPtr Funptr_typedef4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Funptr_typedef5'

__C declaration:__ @funptr_typedef5@

__defined at:__ @macros\/reparse.h 136:16@

__exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5_Aux = Funptr_typedef5_Aux
  { un_Funptr_typedef5_Aux :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_1f45632f07742a46_base ::
     (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void)))

-- __unique:__ @toFunptr_typedef5_Aux@
hs_bindgen_1f45632f07742a46 ::
     Funptr_typedef5_Aux
  -> IO (Ptr.FunPtr Funptr_typedef5_Aux)
hs_bindgen_1f45632f07742a46 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_1f45632f07742a46_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0bd1877eaaba0d3e_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void))
  -> FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void)

-- __unique:__ @fromFunptr_typedef5_Aux@
hs_bindgen_0bd1877eaaba0d3e ::
     Ptr.FunPtr Funptr_typedef5_Aux
  -> Funptr_typedef5_Aux
hs_bindgen_0bd1877eaaba0d3e =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_0bd1877eaaba0d3e_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef5_Aux where

  toFunPtr = hs_bindgen_1f45632f07742a46

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef5_Aux where

  fromFunPtr = hs_bindgen_0bd1877eaaba0d3e

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef5_Aux) "un_Funptr_typedef5_Aux")
         ) => GHC.Records.HasField "un_Funptr_typedef5_Aux" (Ptr.Ptr Funptr_typedef5_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef5_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef5_Aux "un_Funptr_typedef5_Aux" where

  type CFieldType Funptr_typedef5_Aux "un_Funptr_typedef5_Aux" =
    FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @funptr_typedef5@

    __defined at:__ @macros\/reparse.h 136:16@

    __exported by:__ @macros\/reparse.h@
-}
newtype Funptr_typedef5 = Funptr_typedef5
  { un_Funptr_typedef5 :: Ptr.FunPtr Funptr_typedef5_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Funptr_typedef5) "un_Funptr_typedef5")
         ) => GHC.Records.HasField "un_Funptr_typedef5" (Ptr.Ptr Funptr_typedef5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Funptr_typedef5")

instance HsBindgen.Runtime.HasCField.HasCField Funptr_typedef5 "un_Funptr_typedef5" where

  type CFieldType Funptr_typedef5 "un_Funptr_typedef5" =
    Ptr.FunPtr Funptr_typedef5_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @comments2@

    __defined at:__ @macros\/reparse.h 145:30@

    __exported by:__ @macros\/reparse.h@
-}
newtype Comments2 = Comments2
  { un_Comments2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Comments2) "un_Comments2")
         ) => GHC.Records.HasField "un_Comments2" (Ptr.Ptr Comments2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Comments2")

instance HsBindgen.Runtime.HasCField.HasCField Comments2 "un_Comments2" where

  type CFieldType Comments2 "un_Comments2" = A

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

instance F.Storable Example_struct where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Example_struct
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_field1") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_field2") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_field3") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct
            example_struct_field12
            example_struct_field23
            example_struct_field34 ->
                 HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_field1") ptr0 example_struct_field12
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_field2") ptr0 example_struct_field23
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_field3") ptr0 example_struct_field34

instance HsBindgen.Runtime.HasCField.HasCField Example_struct "example_struct_field1" where

  type CFieldType Example_struct "example_struct_field1" =
    A

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct) "example_struct_field1")
         ) => GHC.Records.HasField "example_struct_field1" (Ptr.Ptr Example_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_field1")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct "example_struct_field2" where

  type CFieldType Example_struct "example_struct_field2" =
    Ptr.Ptr A

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct) "example_struct_field2")
         ) => GHC.Records.HasField "example_struct_field2" (Ptr.Ptr Example_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_field2")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct "example_struct_field3" where

  type CFieldType Example_struct "example_struct_field3" =
    Ptr.Ptr (Ptr.Ptr A)

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct) "example_struct_field3")
         ) => GHC.Records.HasField "example_struct_field3" (Ptr.Ptr Example_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_field3")

{-| __C declaration:__ @const_typedef1@

    __defined at:__ @macros\/reparse.h 220:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef1 = Const_typedef1
  { un_Const_typedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef1) "un_Const_typedef1")
         ) => GHC.Records.HasField "un_Const_typedef1" (Ptr.Ptr Const_typedef1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_typedef1")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef1 "un_Const_typedef1" where

  type CFieldType Const_typedef1 "un_Const_typedef1" =
    A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef2@

    __defined at:__ @macros\/reparse.h 221:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef2 = Const_typedef2
  { un_Const_typedef2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef2) "un_Const_typedef2")
         ) => GHC.Records.HasField "un_Const_typedef2" (Ptr.Ptr Const_typedef2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_typedef2")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef2 "un_Const_typedef2" where

  type CFieldType Const_typedef2 "un_Const_typedef2" =
    A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef3@

    __defined at:__ @macros\/reparse.h 222:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef3 = Const_typedef3
  { un_Const_typedef3 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef3) "un_Const_typedef3")
         ) => GHC.Records.HasField "un_Const_typedef3" (Ptr.Ptr Const_typedef3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_typedef3")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef3 "un_Const_typedef3" where

  type CFieldType Const_typedef3 "un_Const_typedef3" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef4@

    __defined at:__ @macros\/reparse.h 223:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef4 = Const_typedef4
  { un_Const_typedef4 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef4) "un_Const_typedef4")
         ) => GHC.Records.HasField "un_Const_typedef4" (Ptr.Ptr Const_typedef4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_typedef4")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef4 "un_Const_typedef4" where

  type CFieldType Const_typedef4 "un_Const_typedef4" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef5@

    __defined at:__ @macros\/reparse.h 224:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef5 = Const_typedef5
  { un_Const_typedef5 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef5) "un_Const_typedef5")
         ) => GHC.Records.HasField "un_Const_typedef5" (Ptr.Ptr Const_typedef5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_typedef5")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef5 "un_Const_typedef5" where

  type CFieldType Const_typedef5 "un_Const_typedef5" =
    Ptr.Ptr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef6@

    __defined at:__ @macros\/reparse.h 225:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef6 = Const_typedef6
  { un_Const_typedef6 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef6) "un_Const_typedef6")
         ) => GHC.Records.HasField "un_Const_typedef6" (Ptr.Ptr Const_typedef6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_typedef6")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef6 "un_Const_typedef6" where

  type CFieldType Const_typedef6 "un_Const_typedef6" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_typedef7@

    __defined at:__ @macros\/reparse.h 226:25@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_typedef7 = Const_typedef7
  { un_Const_typedef7 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_typedef7) "un_Const_typedef7")
         ) => GHC.Records.HasField "un_Const_typedef7" (Ptr.Ptr Const_typedef7) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_typedef7")

instance HsBindgen.Runtime.HasCField.HasCField Const_typedef7 "un_Const_typedef7" where

  type CFieldType Const_typedef7 "un_Const_typedef7" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

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
  , example_struct_with_const_const_field3 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
    {- ^ __C declaration:__ @const_field3@

         __defined at:__ @macros\/reparse.h 231:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field4 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
    {- ^ __C declaration:__ @const_field4@

         __defined at:__ @macros\/reparse.h 232:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field5 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field5@

         __defined at:__ @macros\/reparse.h 233:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field6 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
    {- ^ __C declaration:__ @const_field6@

         __defined at:__ @macros\/reparse.h 234:19@

         __exported by:__ @macros\/reparse.h@
    -}
  , example_struct_with_const_const_field7 :: HsBindgen.Runtime.ConstPtr.ConstPtr A
    {- ^ __C declaration:__ @const_field7@

         __defined at:__ @macros\/reparse.h 235:19@

         __exported by:__ @macros\/reparse.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Example_struct_with_const where

  sizeOf = \_ -> (48 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Example_struct_with_const
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_with_const_const_field1") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_with_const_const_field2") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_with_const_const_field3") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_with_const_const_field4") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_with_const_const_field5") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_with_const_const_field6") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"example_struct_with_const_const_field7") ptr0

  poke =
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
                 HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_with_const_const_field1") ptr0 example_struct_with_const_const_field12
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_with_const_const_field2") ptr0 example_struct_with_const_const_field23
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_with_const_const_field3") ptr0 example_struct_with_const_const_field34
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_with_const_const_field4") ptr0 example_struct_with_const_const_field45
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_with_const_const_field5") ptr0 example_struct_with_const_const_field56
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_with_const_const_field6") ptr0 example_struct_with_const_const_field67
              >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"example_struct_with_const_const_field7") ptr0 example_struct_with_const_const_field78

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field1" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field1" =
    A

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field1")
         ) => GHC.Records.HasField "example_struct_with_const_const_field1" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_with_const_const_field1")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field2" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field2" =
    A

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field2")
         ) => GHC.Records.HasField "example_struct_with_const_const_field2" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_with_const_const_field2")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field3" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field3" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field3")
         ) => GHC.Records.HasField "example_struct_with_const_const_field3" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_with_const_const_field3")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field4" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field4" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field4")
         ) => GHC.Records.HasField "example_struct_with_const_const_field4" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_with_const_const_field4")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field5" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field5" =
    Ptr.Ptr A

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field5")
         ) => GHC.Records.HasField "example_struct_with_const_const_field5" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_with_const_const_field5")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field6" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field6" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

  offset# = \_ -> \_ -> 32

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field6")
         ) => GHC.Records.HasField "example_struct_with_const_const_field6" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_with_const_const_field6")

instance HsBindgen.Runtime.HasCField.HasCField Example_struct_with_const "example_struct_with_const_const_field7" where

  type CFieldType Example_struct_with_const "example_struct_with_const_const_field7" =
    HsBindgen.Runtime.ConstPtr.ConstPtr A

  offset# = \_ -> \_ -> 40

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Example_struct_with_const) "example_struct_with_const_const_field7")
         ) => GHC.Records.HasField "example_struct_with_const_const_field7" (Ptr.Ptr Example_struct_with_const) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"example_struct_with_const_const_field7")

{-| Auxiliary type used by 'Const_funptr1'

__C declaration:__ @const_funptr1@

__defined at:__ @macros\/reparse.h 238:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1_Aux = Const_funptr1_Aux
  { un_Const_funptr1_Aux :: FC.CInt -> FC.CDouble -> IO A
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_7f125e20a9d4075b_base ::
     (FC.CInt -> FC.CDouble -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CInt))

-- __unique:__ @toConst_funptr1_Aux@
hs_bindgen_7f125e20a9d4075b ::
     Const_funptr1_Aux
  -> IO (Ptr.FunPtr Const_funptr1_Aux)
hs_bindgen_7f125e20a9d4075b =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_7f125e20a9d4075b_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ac4bd8d789bba94b_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CInt)
  -> FC.CInt -> FC.CDouble -> IO FC.CInt

-- __unique:__ @fromConst_funptr1_Aux@
hs_bindgen_ac4bd8d789bba94b ::
     Ptr.FunPtr Const_funptr1_Aux
  -> Const_funptr1_Aux
hs_bindgen_ac4bd8d789bba94b =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_ac4bd8d789bba94b_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr1_Aux where

  toFunPtr = hs_bindgen_7f125e20a9d4075b

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr1_Aux where

  fromFunPtr = hs_bindgen_ac4bd8d789bba94b

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr1_Aux) "un_Const_funptr1_Aux")
         ) => GHC.Records.HasField "un_Const_funptr1_Aux" (Ptr.Ptr Const_funptr1_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr1_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr1_Aux "un_Const_funptr1_Aux" where

  type CFieldType Const_funptr1_Aux "un_Const_funptr1_Aux" =
    FC.CInt -> FC.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr1@

    __defined at:__ @macros\/reparse.h 238:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr1 = Const_funptr1
  { un_Const_funptr1 :: Ptr.FunPtr Const_funptr1_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr1) "un_Const_funptr1")
         ) => GHC.Records.HasField "un_Const_funptr1" (Ptr.Ptr Const_funptr1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr1")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr1 "un_Const_funptr1" where

  type CFieldType Const_funptr1 "un_Const_funptr1" =
    Ptr.FunPtr Const_funptr1_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr2'

__C declaration:__ @const_funptr2@

__defined at:__ @macros\/reparse.h 239:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2_Aux = Const_funptr2_Aux
  { un_Const_funptr2_Aux :: FC.CInt -> FC.CDouble -> IO A
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_c7b1e36d845634fb_base ::
     (FC.CInt -> FC.CDouble -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CInt))

-- __unique:__ @toConst_funptr2_Aux@
hs_bindgen_c7b1e36d845634fb ::
     Const_funptr2_Aux
  -> IO (Ptr.FunPtr Const_funptr2_Aux)
hs_bindgen_c7b1e36d845634fb =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_c7b1e36d845634fb_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_352cebf463125ca9_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO FC.CInt)
  -> FC.CInt -> FC.CDouble -> IO FC.CInt

-- __unique:__ @fromConst_funptr2_Aux@
hs_bindgen_352cebf463125ca9 ::
     Ptr.FunPtr Const_funptr2_Aux
  -> Const_funptr2_Aux
hs_bindgen_352cebf463125ca9 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_352cebf463125ca9_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr2_Aux where

  toFunPtr = hs_bindgen_c7b1e36d845634fb

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr2_Aux where

  fromFunPtr = hs_bindgen_352cebf463125ca9

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr2_Aux) "un_Const_funptr2_Aux")
         ) => GHC.Records.HasField "un_Const_funptr2_Aux" (Ptr.Ptr Const_funptr2_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr2_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr2_Aux "un_Const_funptr2_Aux" where

  type CFieldType Const_funptr2_Aux "un_Const_funptr2_Aux" =
    FC.CInt -> FC.CDouble -> IO A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr2@

    __defined at:__ @macros\/reparse.h 239:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr2 = Const_funptr2
  { un_Const_funptr2 :: Ptr.FunPtr Const_funptr2_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr2) "un_Const_funptr2")
         ) => GHC.Records.HasField "un_Const_funptr2" (Ptr.Ptr Const_funptr2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr2")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr2 "un_Const_funptr2" where

  type CFieldType Const_funptr2 "un_Const_funptr2" =
    Ptr.FunPtr Const_funptr2_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr3'

__C declaration:__ @const_funptr3@

__defined at:__ @macros\/reparse.h 240:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3_Aux = Const_funptr3_Aux
  { un_Const_funptr3_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_2dcbfe1c2502178c_base ::
     (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)))

-- __unique:__ @toConst_funptr3_Aux@
hs_bindgen_2dcbfe1c2502178c ::
     Const_funptr3_Aux
  -> IO (Ptr.FunPtr Const_funptr3_Aux)
hs_bindgen_2dcbfe1c2502178c =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_2dcbfe1c2502178c_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_86738dcfd7c9d33c_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- __unique:__ @fromConst_funptr3_Aux@
hs_bindgen_86738dcfd7c9d33c ::
     Ptr.FunPtr Const_funptr3_Aux
  -> Const_funptr3_Aux
hs_bindgen_86738dcfd7c9d33c =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_86738dcfd7c9d33c_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr3_Aux where

  toFunPtr = hs_bindgen_2dcbfe1c2502178c

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr3_Aux where

  fromFunPtr = hs_bindgen_86738dcfd7c9d33c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr3_Aux) "un_Const_funptr3_Aux")
         ) => GHC.Records.HasField "un_Const_funptr3_Aux" (Ptr.Ptr Const_funptr3_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr3_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr3_Aux "un_Const_funptr3_Aux" where

  type CFieldType Const_funptr3_Aux "un_Const_funptr3_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr3@

    __defined at:__ @macros\/reparse.h 240:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr3 = Const_funptr3
  { un_Const_funptr3 :: Ptr.FunPtr Const_funptr3_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr3) "un_Const_funptr3")
         ) => GHC.Records.HasField "un_Const_funptr3" (Ptr.Ptr Const_funptr3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr3")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr3 "un_Const_funptr3" where

  type CFieldType Const_funptr3 "un_Const_funptr3" =
    Ptr.FunPtr Const_funptr3_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr4'

__C declaration:__ @const_funptr4@

__defined at:__ @macros\/reparse.h 241:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4_Aux = Const_funptr4_Aux
  { un_Const_funptr4_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_5461deeda491de0b_base ::
     (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)))

-- __unique:__ @toConst_funptr4_Aux@
hs_bindgen_5461deeda491de0b ::
     Const_funptr4_Aux
  -> IO (Ptr.FunPtr Const_funptr4_Aux)
hs_bindgen_5461deeda491de0b =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_5461deeda491de0b_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_de7846fca3bfd1b6_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- __unique:__ @fromConst_funptr4_Aux@
hs_bindgen_de7846fca3bfd1b6 ::
     Ptr.FunPtr Const_funptr4_Aux
  -> Const_funptr4_Aux
hs_bindgen_de7846fca3bfd1b6 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_de7846fca3bfd1b6_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr4_Aux where

  toFunPtr = hs_bindgen_5461deeda491de0b

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr4_Aux where

  fromFunPtr = hs_bindgen_de7846fca3bfd1b6

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr4_Aux) "un_Const_funptr4_Aux")
         ) => GHC.Records.HasField "un_Const_funptr4_Aux" (Ptr.Ptr Const_funptr4_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr4_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr4_Aux "un_Const_funptr4_Aux" where

  type CFieldType Const_funptr4_Aux "un_Const_funptr4_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr4@

    __defined at:__ @macros\/reparse.h 241:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr4 = Const_funptr4
  { un_Const_funptr4 :: Ptr.FunPtr Const_funptr4_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr4) "un_Const_funptr4")
         ) => GHC.Records.HasField "un_Const_funptr4" (Ptr.Ptr Const_funptr4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr4")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr4 "un_Const_funptr4" where

  type CFieldType Const_funptr4 "un_Const_funptr4" =
    Ptr.FunPtr Const_funptr4_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr5'

__C declaration:__ @const_funptr5@

__defined at:__ @macros\/reparse.h 242:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5_Aux = Const_funptr5_Aux
  { un_Const_funptr5_Aux :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_7b0174fc978a1ce1_base ::
     (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void))
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void)))

-- __unique:__ @toConst_funptr5_Aux@
hs_bindgen_7b0174fc978a1ce1 ::
     Const_funptr5_Aux
  -> IO (Ptr.FunPtr Const_funptr5_Aux)
hs_bindgen_7b0174fc978a1ce1 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_7b0174fc978a1ce1_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_38a21d84bb7115b5_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void))
  -> FC.CInt -> FC.CDouble -> IO (Ptr.Ptr Void)

-- __unique:__ @fromConst_funptr5_Aux@
hs_bindgen_38a21d84bb7115b5 ::
     Ptr.FunPtr Const_funptr5_Aux
  -> Const_funptr5_Aux
hs_bindgen_38a21d84bb7115b5 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_38a21d84bb7115b5_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr5_Aux where

  toFunPtr = hs_bindgen_7b0174fc978a1ce1

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr5_Aux where

  fromFunPtr = hs_bindgen_38a21d84bb7115b5

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr5_Aux) "un_Const_funptr5_Aux")
         ) => GHC.Records.HasField "un_Const_funptr5_Aux" (Ptr.Ptr Const_funptr5_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr5_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr5_Aux "un_Const_funptr5_Aux" where

  type CFieldType Const_funptr5_Aux "un_Const_funptr5_Aux" =
    FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr5@

    __defined at:__ @macros\/reparse.h 242:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr5 = Const_funptr5
  { un_Const_funptr5 :: Ptr.FunPtr Const_funptr5_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr5) "un_Const_funptr5")
         ) => GHC.Records.HasField "un_Const_funptr5" (Ptr.Ptr Const_funptr5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr5")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr5 "un_Const_funptr5" where

  type CFieldType Const_funptr5 "un_Const_funptr5" =
    Ptr.FunPtr Const_funptr5_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr6'

__C declaration:__ @const_funptr6@

__defined at:__ @macros\/reparse.h 243:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6_Aux = Const_funptr6_Aux
  { un_Const_funptr6_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_4e32721222f4df9f_base ::
     (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)))

-- __unique:__ @toConst_funptr6_Aux@
hs_bindgen_4e32721222f4df9f ::
     Const_funptr6_Aux
  -> IO (Ptr.FunPtr Const_funptr6_Aux)
hs_bindgen_4e32721222f4df9f =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_4e32721222f4df9f_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_45251216b04aa8b5_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- __unique:__ @fromConst_funptr6_Aux@
hs_bindgen_45251216b04aa8b5 ::
     Ptr.FunPtr Const_funptr6_Aux
  -> Const_funptr6_Aux
hs_bindgen_45251216b04aa8b5 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_45251216b04aa8b5_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr6_Aux where

  toFunPtr = hs_bindgen_4e32721222f4df9f

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr6_Aux where

  fromFunPtr = hs_bindgen_45251216b04aa8b5

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr6_Aux) "un_Const_funptr6_Aux")
         ) => GHC.Records.HasField "un_Const_funptr6_Aux" (Ptr.Ptr Const_funptr6_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr6_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr6_Aux "un_Const_funptr6_Aux" where

  type CFieldType Const_funptr6_Aux "un_Const_funptr6_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr6@

    __defined at:__ @macros\/reparse.h 243:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr6 = Const_funptr6
  { un_Const_funptr6 :: Ptr.FunPtr Const_funptr6_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr6) "un_Const_funptr6")
         ) => GHC.Records.HasField "un_Const_funptr6" (Ptr.Ptr Const_funptr6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr6")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr6 "un_Const_funptr6" where

  type CFieldType Const_funptr6 "un_Const_funptr6" =
    Ptr.FunPtr Const_funptr6_Aux

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Const_funptr7'

__C declaration:__ @const_funptr7@

__defined at:__ @macros\/reparse.h 244:27@

__exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7_Aux = Const_funptr7_Aux
  { un_Const_funptr7_Aux :: FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_0d04fc96ffb9de06_base ::
     (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> IO (Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)))

-- __unique:__ @toConst_funptr7_Aux@
hs_bindgen_0d04fc96ffb9de06 ::
     Const_funptr7_Aux
  -> IO (Ptr.FunPtr Const_funptr7_Aux)
hs_bindgen_0d04fc96ffb9de06 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_0d04fc96ffb9de06_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_42fbcebf75a973ba_base ::
     Ptr.FunPtr (FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void))
  -> FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- __unique:__ @fromConst_funptr7_Aux@
hs_bindgen_42fbcebf75a973ba ::
     Ptr.FunPtr Const_funptr7_Aux
  -> Const_funptr7_Aux
hs_bindgen_42fbcebf75a973ba =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_42fbcebf75a973ba_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr7_Aux where

  toFunPtr = hs_bindgen_0d04fc96ffb9de06

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr7_Aux where

  fromFunPtr = hs_bindgen_42fbcebf75a973ba

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr7_Aux) "un_Const_funptr7_Aux")
         ) => GHC.Records.HasField "un_Const_funptr7_Aux" (Ptr.Ptr Const_funptr7_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr7_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr7_Aux "un_Const_funptr7_Aux" where

  type CFieldType Const_funptr7_Aux "un_Const_funptr7_Aux" =
    FC.CInt -> FC.CDouble -> IO (HsBindgen.Runtime.ConstPtr.ConstPtr A)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @const_funptr7@

    __defined at:__ @macros\/reparse.h 244:27@

    __exported by:__ @macros\/reparse.h@
-}
newtype Const_funptr7 = Const_funptr7
  { un_Const_funptr7 :: Ptr.FunPtr Const_funptr7_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Const_funptr7) "un_Const_funptr7")
         ) => GHC.Records.HasField "un_Const_funptr7" (Ptr.Ptr Const_funptr7) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Const_funptr7")

instance HsBindgen.Runtime.HasCField.HasCField Const_funptr7 "un_Const_funptr7" where

  type CFieldType Const_funptr7 "un_Const_funptr7" =
    Ptr.FunPtr Const_funptr7_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BOOL@

    __defined at:__ @macros\/reparse.h 280:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype BOOL = BOOL
  { un_BOOL :: FC.CBool
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @INT@

    __defined at:__ @macros\/reparse.h 281:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INT = INT
  { un_INT :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @INTP@

    __defined at:__ @macros\/reparse.h 282:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INTP = INTP
  { un_INTP :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| __C declaration:__ @INTCP@

    __defined at:__ @macros\/reparse.h 283:9@

    __exported by:__ @macros\/reparse.h@
-}
newtype INTCP = INTCP
  { un_INTCP :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)
