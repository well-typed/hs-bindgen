{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import C.Expr.HostPlatform ((+))
import qualified C.Expr.HostPlatform as C
import qualified Data.Array.Byte
import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Data.Map.Strict
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Floating, Fractional, IO, Int, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show, pure, show)

fIELD_OFFSET :: FC.CInt
fIELD_OFFSET = (4 :: FC.CInt)

ePSILON :: FC.CDouble
ePSILON = (0.1 :: FC.CDouble)

pTR_TO_FIELD :: forall a0. (C.Add a0) FC.CInt => a0 -> (C.AddRes a0) FC.CInt
pTR_TO_FIELD = \ptr0 -> (+) ptr0 (4 :: FC.CInt)

newtype YEAR = YEAR
  { un_YEAR :: FC.CInt
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
  { un_MONTH :: FC.CInt
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
  { un_DAY :: FC.CInt
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

foreign import capi safe "manual_examples.h mk_triple" mk_triple :: FC.CInt -> FC.CInt -> FC.CInt -> (F.Ptr Triple) -> IO ()

foreign import capi safe "manual_examples.h index_triple" index_triple :: (F.Ptr Triple) -> Index -> IO FC.CInt

foreign import capi safe "manual_examples.h sum_triple" sum_triple :: (F.Ptr Triple) -> IO Sum

foreign import capi safe "manual_examples.h average_triple" average_triple :: (F.Ptr Triple) -> IO Average

foreign import capi safe "manual_examples.h getYear" getYear :: (F.Ptr Date) -> IO YEAR

foreign import capi safe "manual_examples.h print_occupation" print_occupation :: FC.CInt -> (F.Ptr Occupation) -> IO ()

foreign import capi safe "manual_examples.h 拜拜" 拜拜 :: IO ()

foreign import capi safe "manual_examples.h ϒ" cϒ :: IO ()

foreign import capi safe "manual_examples.h import" import' :: IO ()

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
  { un_Index :: FC.CUInt
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
          Index un_Index2 -> F.pokeByteOff ptr0 (0 :: Int) un_Index2

deriving stock instance Eq Index

deriving stock instance Ord Index

deriving stock instance Read Index

instance HsBindgen.Runtime.CEnum.CEnum Index where

  type CEnumZ Index = FC.CUInt

  fromCEnumZ = Index

  toCEnumZ = un_Index

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [(0, Data.List.NonEmpty.singleton "A"), (1, Data.List.NonEmpty.singleton "B"), (2, Data.List.NonEmpty.singleton "C")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum Index where

  minValue = A

  maxValue = C

instance Show Index where

  show = HsBindgen.Runtime.CEnum.showCEnum "Index"

pattern A :: Index
pattern A = Index 0

pattern B :: Index
pattern B = Index 1

pattern C :: Index
pattern C = Index 2

newtype Sum = Sum
  { un_Sum :: FC.CInt
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
  { un_Average :: FC.CDouble
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

data Student = Student
  { student_university :: F.Ptr FC.CChar
  , student_year :: FC.CInt
  }

instance F.Storable Student where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Student
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Student student_university2 student_year3 ->
               F.pokeByteOff ptr0 (0 :: Int) student_university2
            >> F.pokeByteOff ptr0 (8 :: Int) student_year3

deriving stock instance Show Student

deriving stock instance Eq Student

data Person

data Employee = Employee
  { employee_company :: F.Ptr FC.CChar
  , employee_supervisor :: F.Ptr Person
  , employee_salary :: FC.CInt
  }

instance F.Storable Employee where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Employee
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Employee employee_company2 employee_supervisor3 employee_salary4 ->
               F.pokeByteOff ptr0 (0 :: Int) employee_company2
            >> F.pokeByteOff ptr0 (8 :: Int) employee_supervisor3
            >> F.pokeByteOff ptr0 (16 :: Int) employee_salary4

deriving stock instance Show Employee

deriving stock instance Eq Employee

newtype Occupation = Occupation
  { un_Occupation :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 24) 8 instance F.Storable Occupation

get_occupation_student :: Occupation -> Student
get_occupation_student = HsBindgen.Runtime.ByteArray.getUnionPayload

set_occupation_student :: Student -> Occupation
set_occupation_student = HsBindgen.Runtime.ByteArray.setUnionPayload

get_occupation_employee :: Occupation -> Employee
get_occupation_employee = HsBindgen.Runtime.ByteArray.getUnionPayload

set_occupation_employee :: Employee -> Occupation
set_occupation_employee = HsBindgen.Runtime.ByteArray.setUnionPayload

data Rect_lower_left = Rect_lower_left
  { rect_lower_left_x :: FC.CInt
  , rect_lower_left_y :: FC.CInt
  }

instance F.Storable Rect_lower_left where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Rect_lower_left
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect_lower_left rect_lower_left_x2 rect_lower_left_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) rect_lower_left_x2
            >> F.pokeByteOff ptr0 (4 :: Int) rect_lower_left_y3

deriving stock instance Show Rect_lower_left

deriving stock instance Eq Rect_lower_left

data Rect_upper_right = Rect_upper_right
  { rect_upper_right_x :: FC.CInt
  , rect_upper_right_y :: FC.CInt
  }

instance F.Storable Rect_upper_right where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Rect_upper_right
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect_upper_right rect_upper_right_x2 rect_upper_right_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) rect_upper_right_x2
            >> F.pokeByteOff ptr0 (4 :: Int) rect_upper_right_y3

deriving stock instance Show Rect_upper_right

deriving stock instance Eq Rect_upper_right

data Rect = Rect
  { rect_lower_left :: Rect_lower_left
  , rect_upper_right :: Rect_upper_right
  }

instance F.Storable Rect where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Rect
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect rect_lower_left2 rect_upper_right3 ->
               F.pokeByteOff ptr0 (0 :: Int) rect_lower_left2
            >> F.pokeByteOff ptr0 (8 :: Int) rect_upper_right3

deriving stock instance Show Rect

deriving stock instance Eq Rect

data Config_Deref = Config_Deref
  { config_Deref_width :: FC.CInt
  , config_Deref_height :: FC.CInt
  }

instance F.Storable Config_Deref where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Config_Deref
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config_Deref config_Deref_width2 config_Deref_height3 ->
               F.pokeByteOff ptr0 (0 :: Int) config_Deref_width2
            >> F.pokeByteOff ptr0 (4 :: Int) config_Deref_height3

deriving stock instance Show Config_Deref

deriving stock instance Eq Config_Deref

newtype Config = Config
  { un_Config :: F.Ptr Config_Deref
  }

deriving newtype instance F.Storable Config

newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }

deriving newtype instance F.Storable Adio'0301s

deriving stock instance Eq Adio'0301s

deriving stock instance Ord Adio'0301s

deriving stock instance Read Adio'0301s

deriving stock instance Show Adio'0301s

deriving newtype instance Enum Adio'0301s

deriving newtype instance Ix.Ix Adio'0301s

deriving newtype instance Bounded Adio'0301s

deriving newtype instance Bits.Bits Adio'0301s

deriving newtype instance FiniteBits Adio'0301s

deriving newtype instance Integral Adio'0301s

deriving newtype instance Num Adio'0301s

deriving newtype instance Real Adio'0301s

newtype C数字 = C数字
  { un_C数字 :: FC.CInt
  }

deriving newtype instance F.Storable C数字

deriving stock instance Eq C数字

deriving stock instance Ord C数字

deriving stock instance Read C数字

deriving stock instance Show C数字

deriving newtype instance Enum C数字

deriving newtype instance Ix.Ix C数字

deriving newtype instance Bounded C数字

deriving newtype instance Bits.Bits C数字

deriving newtype instance FiniteBits C数字

deriving newtype instance Integral C数字

deriving newtype instance Num C数字

deriving newtype instance Real C数字

newtype Data = Data
  { un_Data :: FC.CInt
  }

deriving newtype instance F.Storable Data

deriving stock instance Eq Data

deriving stock instance Ord Data

deriving stock instance Read Data

deriving stock instance Show Data

deriving newtype instance Enum Data

deriving newtype instance Ix.Ix Data

deriving newtype instance Bounded Data

deriving newtype instance Bits.Bits Data

deriving newtype instance FiniteBits Data

deriving newtype instance Integral Data

deriving newtype instance Num Data

deriving newtype instance Real Data
