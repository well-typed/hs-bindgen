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

import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import qualified Text.Read
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum foo_enum@

    __defined at:__ @types\/enums\/enum_cpp_syntax.h 4:9@

    __exported by:__ @types\/enums\/enum_cpp_syntax.h@
-}
newtype Foo_enum = Foo_enum
  { unwrapFoo_enum :: HsBindgen.Runtime.LibC.Word32
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Foo_enum where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Foo_enum where

  readRaw =
    \ptr0 ->
          pure Foo_enum
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Foo_enum where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_enum unwrapFoo_enum2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapFoo_enum2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Foo_enum instance F.Storable Foo_enum

deriving via HsBindgen.Runtime.LibC.Word32 instance Data.Primitive.Types.Prim Foo_enum

instance HsBindgen.Runtime.CEnum.CEnum Foo_enum where

  type CEnumZ Foo_enum = HsBindgen.Runtime.LibC.Word32

  toCEnum = Foo_enum

  fromCEnum = unwrapFoo_enum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "A")
                                                     , (1, Data.List.NonEmpty.singleton "B")
                                                     , (2, Data.List.NonEmpty.singleton "C")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Foo_enum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Foo_enum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Foo_enum where

  minDeclaredValue = A

  maxDeclaredValue = C

instance Show Foo_enum where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Foo_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo_enum) "unwrapFoo_enum")
         ) => GHC.Records.HasField "unwrapFoo_enum" (Ptr.Ptr Foo_enum) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFoo_enum")

instance HsBindgen.Runtime.HasCField.HasCField Foo_enum "unwrapFoo_enum" where

  type CFieldType Foo_enum "unwrapFoo_enum" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @types\/enums\/enum_cpp_syntax.h 4:27@

    __exported by:__ @types\/enums\/enum_cpp_syntax.h@
-}
pattern A :: Foo_enum
pattern A = Foo_enum 0

{-| __C declaration:__ @B@

    __defined at:__ @types\/enums\/enum_cpp_syntax.h 4:30@

    __exported by:__ @types\/enums\/enum_cpp_syntax.h@
-}
pattern B :: Foo_enum
pattern B = Foo_enum 1

{-| __C declaration:__ @C@

    __defined at:__ @types\/enums\/enum_cpp_syntax.h 4:33@

    __exported by:__ @types\/enums\/enum_cpp_syntax.h@
-}
pattern C :: Foo_enum
pattern C = Foo_enum 2
