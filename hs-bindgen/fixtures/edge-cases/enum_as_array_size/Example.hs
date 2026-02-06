{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import qualified Text.Read
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum test@

    __defined at:__ @edge-cases\/enum_as_array_size.h 3:6@

    __exported by:__ @edge-cases\/enum_as_array_size.h@
-}
newtype Test = Test
  { unwrapTest :: FC.CUInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize Test where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Test where

  readRaw =
    \ptr0 ->
          pure Test
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw Test where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Test unwrapTest2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapTest2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Test instance F.Storable Test

deriving via FC.CUInt instance Data.Primitive.Types.Prim Test

instance HsBindgen.Runtime.CEnum.CEnum Test where

  type CEnumZ Test = FC.CUInt

  toCEnum = Test

  fromCEnum = unwrapTest

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "Test_a")
                                                     , (1, Data.List.NonEmpty.singleton "Test_count")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Test"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Test"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Test where

  minDeclaredValue = Test_a

  maxDeclaredValue = Test_count

instance Show Test where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read Test where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance GHC.Records.HasField "unwrapTest" (Ptr.Ptr Test) (Ptr.Ptr FC.CUInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTest")

instance HsBindgen.Runtime.HasCField.HasCField Test "unwrapTest" where

  type CFieldType Test "unwrapTest" = FC.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @test_a@

    __defined at:__ @edge-cases\/enum_as_array_size.h 4:3@

    __exported by:__ @edge-cases\/enum_as_array_size.h@
-}
pattern Test_a :: Test
pattern Test_a = Test 0

{-| __C declaration:__ @test_count@

    __defined at:__ @edge-cases\/enum_as_array_size.h 5:3@

    __exported by:__ @edge-cases\/enum_as_array_size.h@
-}
pattern Test_count :: Test
pattern Test_count = Test 1
