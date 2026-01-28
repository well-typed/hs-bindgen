{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Foreign as F
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.LibC
import qualified Text.Read
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum foo_enum@

    __defined at:__ @types\/enums\/enum_cpp_syntax.h 4:9@

    __exported by:__ @types\/enums\/enum_cpp_syntax.h@
-}
newtype Foo_enum = Foo_enum
  { un_Foo_enum :: HsBindgen.Runtime.LibC.Word32
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance F.Storable Foo_enum where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo_enum
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_enum un_Foo_enum2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Foo_enum2

deriving via HsBindgen.Runtime.LibC.Word32 instance Data.Primitive.Types.Prim Foo_enum

instance HsBindgen.Runtime.CEnum.CEnum Foo_enum where

  type CEnumZ Foo_enum = HsBindgen.Runtime.LibC.Word32

  toCEnum = Foo_enum

  fromCEnum = un_Foo_enum

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

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Foo_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

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
