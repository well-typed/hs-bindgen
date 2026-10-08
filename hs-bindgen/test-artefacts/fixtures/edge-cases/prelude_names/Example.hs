{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Example
    ( Example.String'(..)
    , Example.Show'(..)
    , Example.CChar'(..)
    , Example.C_types(..)
    , pattern Example.CInt'
    , Example.Constants(..)
    , pattern Example.Eq
    , pattern Example.Int
    , pattern Example.True
    , Example.Ordering(..)
    , pattern Example.LT
    , pattern Example.EQ
    , pattern Example.GT
    , Example.Maybe(..)
    , Example.Word(..)
    , Example.maximum
    , Example.twice_maximum
    , Example.readList
    , Example.showsPrec
    , Example.Void(..)
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @struct string@

    __defined at:__ @edge-cases\/prelude_names.h 3:8@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
data String' = String'
  { string'_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/prelude_names.h 3:21@

         __exported by:__ @edge-cases\/prelude_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize String' where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw String' where

  readRaw =
    \ptr0 ->
          pure String'
      <*> HasCField.readRaw (BG.Proxy @"string'_x") ptr0

instance Marshal.WriteRaw String' where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          String' string'_x2 ->
            HasCField.writeRaw (BG.Proxy @"string'_x") ptr0 string'_x2

deriving via Marshal.EquivStorable String' instance BG.Storable String'

deriving via Struct.IsStructViaReadRaw String' instance Struct.IsStruct String'

{-| __C declaration:__ @x@

    __defined at:__ @edge-cases\/prelude_names.h 3:21@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "string'_x" String' ty where

  hasField =
    \x0 ->
      (\y1 ->
         String' {string'_x = y1}, BG.getField @"string'_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "string'_x" (BG.Ptr String') (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"string'_x")

instance HasCField.HasCField String' "string'_x" where

  type CFieldType String' "string'_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Show@

    __defined at:__ @edge-cases\/prelude_names.h 4:13@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
newtype Show' = Show'
  { unwrapShow' :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapShow'" Show' ty where

  hasField =
    \x0 ->
      (\y1 ->
         Show' {unwrapShow' = y1}, BG.getField @"unwrapShow'" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapShow'" (BG.Ptr Show') (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapShow'")

instance HasCField.HasCField Show' "unwrapShow'" where

  type CFieldType Show' "unwrapShow'" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct CChar@

    __defined at:__ @edge-cases\/prelude_names.h 9:8@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
data CChar' = CChar'
  { cChar'_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/prelude_names.h 9:20@

         __exported by:__ @edge-cases\/prelude_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize CChar' where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw CChar' where

  readRaw =
    \ptr0 ->
          pure CChar'
      <*> HasCField.readRaw (BG.Proxy @"cChar'_x") ptr0

instance Marshal.WriteRaw CChar' where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          CChar' cChar'_x2 ->
            HasCField.writeRaw (BG.Proxy @"cChar'_x") ptr0 cChar'_x2

deriving via Marshal.EquivStorable CChar' instance BG.Storable CChar'

deriving via Struct.IsStructViaReadRaw CChar' instance Struct.IsStruct CChar'

{-| __C declaration:__ @x@

    __defined at:__ @edge-cases\/prelude_names.h 9:20@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "cChar'_x" CChar' ty where

  hasField =
    \x0 ->
      (\y1 ->
         CChar' {cChar'_x = y1}, BG.getField @"cChar'_x" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "cChar'_x" (BG.Ptr CChar') (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"cChar'_x")

instance HasCField.HasCField CChar' "cChar'_x" where

  type CFieldType CChar' "cChar'_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @enum c_types@

    __defined at:__ @edge-cases\/prelude_names.h 10:6@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
newtype C_types = C_types
  { unwrapC_types :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize C_types where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw C_types where

  readRaw =
    \ptr0 ->
          pure C_types
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw C_types where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          C_types unwrapC_types2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapC_types2

deriving via Marshal.EquivStorable C_types instance BG.Storable C_types

deriving via BG.CUInt instance BG.Prim C_types

instance CEnum.CEnum C_types where

  type CEnumZ C_types = BG.CUInt

  toCEnum = C_types

  fromCEnum = BG.getField @"unwrapC_types"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "CInt'")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "C_types"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "C_types"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum C_types where

  minDeclaredValue = CInt'

  maxDeclaredValue = CInt'

instance Show C_types where

  showsPrec = CEnum.shows

instance Read C_types where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapC_types" C_types ty where

  hasField =
    \x0 ->
      (\y1 ->
         C_types {unwrapC_types = y1}, BG.getField @"unwrapC_types" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapC_types" (BG.Ptr C_types) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapC_types")

instance HasCField.HasCField C_types "unwrapC_types" where

  type CFieldType C_types "unwrapC_types" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @CInt@

    __defined at:__ @edge-cases\/prelude_names.h 10:16@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
pattern CInt' :: C_types
pattern CInt' = C_types 0

{-| __C declaration:__ @enum constants@

    __defined at:__ @edge-cases\/prelude_names.h 14:6@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
newtype Constants = Constants
  { unwrapConstants :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Constants where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Constants where

  readRaw =
    \ptr0 ->
          pure Constants
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Constants where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Constants unwrapConstants2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapConstants2

deriving via Marshal.EquivStorable Constants instance BG.Storable Constants

deriving via BG.CUInt instance BG.Prim Constants

instance CEnum.CEnum Constants where

  type CEnumZ Constants = BG.CUInt

  toCEnum = Constants

  fromCEnum = BG.getField @"unwrapConstants"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "Eq"), (1, BG.singleton "Int"), (2, BG.singleton "True")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Constants"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Constants"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Constants where

  minDeclaredValue = Eq

  maxDeclaredValue = True

instance Show Constants where

  showsPrec = CEnum.shows

instance Read Constants where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapConstants" Constants ty where

  hasField =
    \x0 ->
      (\y1 ->
         Constants {unwrapConstants = y1}, BG.getField @"unwrapConstants" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapConstants" (BG.Ptr Constants) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapConstants")

instance HasCField.HasCField Constants "unwrapConstants" where

  type CFieldType Constants "unwrapConstants" =
    BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Eq@

    __defined at:__ @edge-cases\/prelude_names.h 14:18@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
pattern Eq :: Constants
pattern Eq = Constants 0

{-| __C declaration:__ @Int@

    __defined at:__ @edge-cases\/prelude_names.h 14:22@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
pattern Int :: Constants
pattern Int = Constants 1

{-| __C declaration:__ @True@

    __defined at:__ @edge-cases\/prelude_names.h 14:27@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
pattern True :: Constants
pattern True = Constants 2

{-| __C declaration:__ @enum ordering@

    __defined at:__ @edge-cases\/prelude_names.h 15:6@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
newtype Ordering = Ordering
  { unwrapOrdering :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Ordering where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Ordering where

  readRaw =
    \ptr0 ->
          pure Ordering
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Ordering where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Ordering unwrapOrdering2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapOrdering2

deriving via Marshal.EquivStorable Ordering instance BG.Storable Ordering

deriving via BG.CUInt instance BG.Prim Ordering

instance CEnum.CEnum Ordering where

  type CEnumZ Ordering = BG.CUInt

  toCEnum = Ordering

  fromCEnum = BG.getField @"unwrapOrdering"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "LT"), (1, BG.singleton "EQ"), (2, BG.singleton "GT")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Ordering"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Ordering"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Ordering where

  minDeclaredValue = LT

  maxDeclaredValue = GT

instance Show Ordering where

  showsPrec = CEnum.shows

instance Read Ordering where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapOrdering" Ordering ty where

  hasField =
    \x0 ->
      (\y1 ->
         Ordering {unwrapOrdering = y1}, BG.getField @"unwrapOrdering" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapOrdering" (BG.Ptr Ordering) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOrdering")

instance HasCField.HasCField Ordering "unwrapOrdering" where

  type CFieldType Ordering "unwrapOrdering" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @LT@

    __defined at:__ @edge-cases\/prelude_names.h 15:17@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
pattern LT :: Ordering
pattern LT = Ordering 0

{-| __C declaration:__ @EQ@

    __defined at:__ @edge-cases\/prelude_names.h 15:21@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
pattern EQ :: Ordering
pattern EQ = Ordering 1

{-| __C declaration:__ @GT@

    __defined at:__ @edge-cases\/prelude_names.h 15:25@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
pattern GT :: Ordering
pattern GT = Ordering 2

{-| __C declaration:__ @Maybe@

    __defined at:__ @edge-cases\/prelude_names.h 16:13@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
newtype Maybe = Maybe
  { unwrapMaybe :: BG.CInt
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
         ) => BG.CompatHasField.HasField "unwrapMaybe" Maybe ty where

  hasField =
    \x0 ->
      (\y1 ->
         Maybe {unwrapMaybe = y1}, BG.getField @"unwrapMaybe" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapMaybe" (BG.Ptr Maybe) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMaybe")

instance HasCField.HasCField Maybe "unwrapMaybe" where

  type CFieldType Maybe "unwrapMaybe" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Word@

    __defined at:__ @edge-cases\/prelude_names.h 17:8@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
data Word = Word
  { word_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/prelude_names.h 17:19@

         __exported by:__ @edge-cases\/prelude_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Word where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Word where

  readRaw =
    \ptr0 ->
          pure Word
      <*> HasCField.readRaw (BG.Proxy @"word_x") ptr0

instance Marshal.WriteRaw Word where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Word word_x2 ->
            HasCField.writeRaw (BG.Proxy @"word_x") ptr0 word_x2

deriving via Marshal.EquivStorable Word instance BG.Storable Word

deriving via Struct.IsStructViaReadRaw Word instance Struct.IsStruct Word

{-| __C declaration:__ @x@

    __defined at:__ @edge-cases\/prelude_names.h 17:19@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "word_x" Word ty where

  hasField =
    \x0 ->
      (\y1 -> Word {word_x = y1}, BG.getField @"word_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "word_x" (BG.Ptr Word) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"word_x")

instance HasCField.HasCField Word "word_x" where

  type CFieldType Word "word_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro maximum@

    __defined at:__ @edge-cases\/prelude_names.h 21:9@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
maximum :: BG.CInt
maximum = (3 :: BG.CInt)

{-| __C declaration:__ @macro twice_maximum@

    __defined at:__ @edge-cases\/prelude_names.h 22:9@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
twice_maximum :: BG.CInt
twice_maximum =
  (C.Expr.HostPlatform.*) (2 :: BG.CInt) maximum

{-| __C declaration:__ @macro readList@

    __defined at:__ @edge-cases\/prelude_names.h 26:9@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
readList :: BG.CInt
readList = (1 :: BG.CInt)

{-| __C declaration:__ @macro showsPrec@

    __defined at:__ @edge-cases\/prelude_names.h 27:9@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
showsPrec :: BG.CInt
showsPrec =
  (C.Expr.HostPlatform.*) (2 :: BG.CInt) readList

{-| __C declaration:__ @struct Void@

    __defined at:__ @edge-cases\/prelude_names.h 30:8@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
data Void = Void
  { void_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/prelude_names.h 30:19@

         __exported by:__ @edge-cases\/prelude_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Void where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Void where

  readRaw =
    \ptr0 ->
          pure Void
      <*> HasCField.readRaw (BG.Proxy @"void_x") ptr0

instance Marshal.WriteRaw Void where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Void void_x2 ->
            HasCField.writeRaw (BG.Proxy @"void_x") ptr0 void_x2

deriving via Marshal.EquivStorable Void instance BG.Storable Void

deriving via Struct.IsStructViaReadRaw Void instance Struct.IsStruct Void

{-| __C declaration:__ @x@

    __defined at:__ @edge-cases\/prelude_names.h 30:19@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "void_x" Void ty where

  hasField =
    \x0 ->
      (\y1 -> Void {void_x = y1}, BG.getField @"void_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "void_x" (BG.Ptr Void) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"void_x")

instance HasCField.HasCField Void "void_x" where

  type CFieldType Void "void_x" = BG.CInt

  offset# = \_ -> \_ -> 0
