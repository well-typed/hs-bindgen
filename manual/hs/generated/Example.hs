{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import C.Expr.BuildPlatform ((+))
import qualified C.Expr.BuildPlatform as C
import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Floating, Fractional, IO, Int, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show, pure)

fIELD_OFFSET :: FC.CInt
fIELD_OFFSET = (4 :: FC.CInt)

ePSILON :: FC.CDouble
ePSILON = (0.1 :: FC.CDouble)

pTR_TO_FIELD :: forall a0. (C.Add a0) FC.CInt => a0 -> (C.AddRes a0) FC.CInt
pTR_TO_FIELD = \ptr0 -> (+) ptr0 (4 :: FC.CInt)

newtype YEAR = YEAR
  { unYEAR :: FC.CInt
  }

deriving newtype instance F.Storable YEAR

deriving stock instance Eq YEAR

deriving stock instance Ord YEAR

deriving stock instance Read YEAR

deriving stock instance Show YEAR

deriving newtype instance Enum YEAR

deriving newtype instance Ix.Ix YEAR

deriving newtype instance Bounded YEAR

deriving newtype instance Bits.Bits YEAR

deriving newtype instance FiniteBits YEAR

deriving newtype instance Integral YEAR

deriving newtype instance Num YEAR

deriving newtype instance Real YEAR

newtype MONTH = MONTH
  { unMONTH :: FC.CInt
  }

deriving newtype instance F.Storable MONTH

deriving stock instance Eq MONTH

deriving stock instance Ord MONTH

deriving stock instance Read MONTH

deriving stock instance Show MONTH

deriving newtype instance Enum MONTH

deriving newtype instance Ix.Ix MONTH

deriving newtype instance Bounded MONTH

deriving newtype instance Bits.Bits MONTH

deriving newtype instance FiniteBits MONTH

deriving newtype instance Integral MONTH

deriving newtype instance Num MONTH

deriving newtype instance Real MONTH

newtype DAY = DAY
  { unDAY :: FC.CInt
  }

deriving newtype instance F.Storable DAY

deriving stock instance Eq DAY

deriving stock instance Ord DAY

deriving stock instance Read DAY

deriving stock instance Show DAY

deriving newtype instance Enum DAY

deriving newtype instance Ix.Ix DAY

deriving newtype instance Bounded DAY

deriving newtype instance Bits.Bits DAY

deriving newtype instance FiniteBits DAY

deriving newtype instance Integral DAY

deriving newtype instance Num DAY

deriving newtype instance Real DAY

foreign import capi safe "no_external_headers.h mk_triple" mk_triple :: FC.CInt -> FC.CInt -> FC.CInt -> (F.Ptr Triple) -> IO ()

foreign import capi safe "no_external_headers.h index_triple" index_triple :: (F.Ptr Triple) -> Index -> IO FC.CInt

foreign import capi safe "no_external_headers.h sum_triple" sum_triple :: (F.Ptr Triple) -> IO Sum

foreign import capi safe "no_external_headers.h average_triple" average_triple :: (F.Ptr Triple) -> IO Average

foreign import capi safe "no_external_headers.h getYear" getYear :: (F.Ptr Date) -> IO FC.CInt

data Triple = Triple
  { triple_a :: FC.CInt
  , triple_b :: FC.CInt
  , triple_c :: FC.CInt
  }

instance F.Storable Triple where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Triple
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Triple triple_a2 triple_b3 triple_c4 ->
               F.pokeByteOff ptr0 (0 :: Int) triple_a2
            >> F.pokeByteOff ptr0 (4 :: Int) triple_b3
            >> F.pokeByteOff ptr0 (8 :: Int) triple_c4

deriving stock instance Show Triple

deriving stock instance Eq Triple

newtype Index = Index
  { unIndex :: FC.CUInt
  }

instance F.Storable Index where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Index
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Index unIndex2 ->
            F.pokeByteOff ptr0 (0 :: Int) unIndex2

deriving stock instance Show Index

deriving stock instance Read Index

deriving stock instance Eq Index

deriving stock instance Ord Index

deriving newtype instance Enum Index

pattern A :: Index
pattern A = Index 0

pattern B :: Index
pattern B = Index 1

pattern C :: Index
pattern C = Index 2

newtype Sum = Sum
  { unSum :: FC.CInt
  }

deriving newtype instance F.Storable Sum

deriving stock instance Eq Sum

deriving stock instance Ord Sum

deriving stock instance Read Sum

deriving stock instance Show Sum

deriving newtype instance Enum Sum

deriving newtype instance Ix.Ix Sum

deriving newtype instance Bounded Sum

deriving newtype instance Bits.Bits Sum

deriving newtype instance FiniteBits Sum

deriving newtype instance Integral Sum

deriving newtype instance Num Sum

deriving newtype instance Real Sum

newtype Average = Average
  { unAverage :: FC.CDouble
  }

deriving newtype instance F.Storable Average

deriving stock instance Eq Average

deriving stock instance Ord Average

deriving stock instance Read Average

deriving stock instance Show Average

deriving newtype instance Enum Average

deriving newtype instance Floating Average

deriving newtype instance Fractional Average

deriving newtype instance Num Average

deriving newtype instance Real Average

deriving newtype instance RealFloat Average

deriving newtype instance RealFrac Average

data Date = Date
  { date_year :: YEAR
  , date_month :: MONTH
  , date_day :: DAY
  }

instance F.Storable Date where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Date
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Date date_year2 date_month3 date_day4 ->
               F.pokeByteOff ptr0 (0 :: Int) date_year2
            >> F.pokeByteOff ptr0 (4 :: Int) date_month3
            >> F.pokeByteOff ptr0 (8 :: Int) date_day4

deriving stock instance Show Date

deriving stock instance Eq Date
