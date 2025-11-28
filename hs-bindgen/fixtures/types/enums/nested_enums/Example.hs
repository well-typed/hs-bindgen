{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified Text.Read
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enumA@

    __defined at:__ @types\/enums\/nested_enums.h:2:14@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
newtype EnumA = EnumA
  { un_EnumA :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable EnumA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure EnumA
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumA un_EnumA2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_EnumA2

instance HsBindgen.Runtime.CEnum.CEnum EnumA where

  type CEnumZ EnumA = FC.CUInt

  toCEnum = EnumA

  fromCEnum = un_EnumA

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "VALA_1")
                                                     , (1, Data.List.NonEmpty.singleton "VALA_2")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumA"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumA"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumA where

  minDeclaredValue = VALA_1

  maxDeclaredValue = VALA_2

instance Show EnumA where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read EnumA where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @VALA_1@

    __defined at:__ @types\/enums\/nested_enums.h:3:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALA_1 :: EnumA
pattern VALA_1 = EnumA 0

{-| __C declaration:__ @VALA_2@

    __defined at:__ @types\/enums\/nested_enums.h:4:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALA_2 :: EnumA
pattern VALA_2 = EnumA 1

{-| __C declaration:__ @exA@

    __defined at:__ @types\/enums\/nested_enums.h:1:8@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
data ExA = ExA
  { exA_fieldA1 :: EnumA
    {- ^ __C declaration:__ @fieldA1@

         __defined at:__ @types\/enums\/nested_enums.h:5:11@

         __exported by:__ @types\/enums\/nested_enums.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable ExA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExA
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exA_fieldA1") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exA_fieldA1") ptr0 exA_fieldA12

instance HsBindgen.Runtime.HasCField.HasCField ExA "exA_fieldA1" where

  type CFieldType ExA "exA_fieldA1" = EnumA

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExA) "exA_fieldA1")
         ) => GHC.Records.HasField "exA_fieldA1" (Ptr.Ptr ExA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exA_fieldA1")

{-| __defined at:__ @types\/enums\/nested_enums.h:9:9@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
newtype ExB_fieldB1 = ExB_fieldB1
  { un_ExB_fieldB1 :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable ExB_fieldB1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExB_fieldB1
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB_fieldB1 un_ExB_fieldB12 ->
            F.pokeByteOff ptr0 (0 :: Int) un_ExB_fieldB12

instance HsBindgen.Runtime.CEnum.CEnum ExB_fieldB1 where

  type CEnumZ ExB_fieldB1 = FC.CUInt

  toCEnum = ExB_fieldB1

  fromCEnum = un_ExB_fieldB1

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "VALB_1")
                                                     , (1, Data.List.NonEmpty.singleton "VALB_2")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "ExB_fieldB1"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "ExB_fieldB1"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum ExB_fieldB1 where

  minDeclaredValue = VALB_1

  maxDeclaredValue = VALB_2

instance Show ExB_fieldB1 where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read ExB_fieldB1 where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @VALB_1@

    __defined at:__ @types\/enums\/nested_enums.h:10:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALB_1 :: ExB_fieldB1
pattern VALB_1 = ExB_fieldB1 0

{-| __C declaration:__ @VALB_2@

    __defined at:__ @types\/enums\/nested_enums.h:11:17@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
pattern VALB_2 :: ExB_fieldB1
pattern VALB_2 = ExB_fieldB1 1

{-| __C declaration:__ @exB@

    __defined at:__ @types\/enums\/nested_enums.h:8:8@

    __exported by:__ @types\/enums\/nested_enums.h@
-}
data ExB = ExB
  { exB_fieldB1 :: ExB_fieldB1
    {- ^ __C declaration:__ @fieldB1@

         __defined at:__ @types\/enums\/nested_enums.h:12:11@

         __exported by:__ @types\/enums\/nested_enums.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable ExB where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExB
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"exB_fieldB1") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"exB_fieldB1") ptr0 exB_fieldB12

instance HsBindgen.Runtime.HasCField.HasCField ExB "exB_fieldB1" where

  type CFieldType ExB "exB_fieldB1" = ExB_fieldB1

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType ExB) "exB_fieldB1")
         ) => GHC.Records.HasField "exB_fieldB1" (Ptr.Ptr ExB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"exB_fieldB1")
