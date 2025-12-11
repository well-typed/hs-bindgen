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
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified Text.Read
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)

{-| __C declaration:__ @enum test@

    __defined at:__ @edge-cases\/enum_as_array_size.h:3:6@

    __exported by:__ @edge-cases\/enum_as_array_size.h@
-}
newtype Test = Test
  { un_Test :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance F.Storable Test where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Test
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Test un_Test2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Test2

deriving via FC.CUInt instance Data.Primitive.Types.Prim Test

instance HsBindgen.Runtime.CEnum.CEnum Test where

  type CEnumZ Test = FC.CUInt

  toCEnum = Test

  fromCEnum = un_Test

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

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Test where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @test_a@

    __defined at:__ @edge-cases\/enum_as_array_size.h:4:3@

    __exported by:__ @edge-cases\/enum_as_array_size.h@
-}
pattern Test_a :: Test
pattern Test_a = Test 0

{-| __C declaration:__ @test_count@

    __defined at:__ @edge-cases\/enum_as_array_size.h:5:3@

    __exported by:__ @edge-cases\/enum_as_array_size.h@
-}
pattern Test_count :: Test
pattern Test_count = Test 1
