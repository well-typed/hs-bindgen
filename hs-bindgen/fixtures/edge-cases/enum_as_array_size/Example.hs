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
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @enum test@

    __defined at:__ @edge-cases\/enum_as_array_size.h 3:6@

    __exported by:__ @edge-cases\/enum_as_array_size.h@
-}
newtype Test = Test
  { unwrapTest :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Test where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Test where

  readRaw =
    \ptr0 ->
          pure Test
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Test where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Test unwrapTest2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapTest2

deriving via Marshal.EquivStorable Test instance RIP.Storable Test

deriving via RIP.CUInt instance RIP.Prim Test

instance CEnum.CEnum Test where

  type CEnumZ Test = RIP.CUInt

  toCEnum = Test

  fromCEnum = unwrapTest

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "Test_a"), (1, RIP.singleton "Test_count")]

  showsUndeclared = CEnum.showsWrappedUndeclared "Test"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Test"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Test where

  minDeclaredValue = Test_a

  maxDeclaredValue = Test_count

instance Show Test where

  showsPrec = CEnum.shows

instance Read Test where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrapTest" (RIP.Ptr Test) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTest")

instance HasCField.HasCField Test "unwrapTest" where

  type CFieldType Test "unwrapTest" = RIP.CUInt

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
