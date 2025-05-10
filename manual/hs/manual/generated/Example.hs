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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import C.Expr.HostPlatform ((+))
import qualified C.Expr.HostPlatform as C
import qualified Data.Array.Byte
import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Floating, Fractional, IO, Int, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show, pure, showsPrec)
import qualified Text.Read

-- #include "manual_examples.h"

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

-- void mk_triple (signed int arg1, signed int arg2, signed int arg3, struct triple *arg4)

foreign import capi safe "manual_examples.h mk_triple" mk_triple :: FC.CInt -> FC.CInt -> FC.CInt -> (F.Ptr Triple) -> IO ()

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
          Index un_Index2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Index2

deriving stock instance Eq Index

deriving stock instance Ord Index

instance HsBindgen.Runtime.CEnum.CEnum Index where

  type CEnumZ Index = FC.CUInt

  toCEnum = Index

  fromCEnum = un_Index

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "A")
                                                     , (1, Data.List.NonEmpty.singleton "B")
                                                     , (2, Data.List.NonEmpty.singleton "C")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Index"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Index"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Index where

  minDeclaredValue = A

  maxDeclaredValue = C

instance Show Index where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Index where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern A :: Index
pattern A = Index 0

pattern B :: Index
pattern B = Index 1

pattern C :: Index
pattern C = Index 2

-- signed int index_triple (struct triple *arg1, enum index arg2)

foreign import capi safe "manual_examples.h index_triple" index_triple :: (F.Ptr Triple) -> Index -> IO FC.CInt

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

-- sum sum_triple (struct triple *arg1)

foreign import capi safe "manual_examples.h sum_triple" sum_triple :: (F.Ptr Triple) -> IO Sum

-- average average_triple (struct triple *arg1)

foreign import capi safe "manual_examples.h average_triple" average_triple :: (F.Ptr Triple) -> IO Average

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

-- YEAR getYear (date *arg1)

foreign import capi safe "manual_examples.h getYear" getYear :: (F.Ptr Date) -> IO YEAR

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
get_occupation_student =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_occupation_student :: Student -> Occupation
set_occupation_student =
  HsBindgen.Runtime.ByteArray.setUnionPayload

get_occupation_employee :: Occupation -> Employee
get_occupation_employee =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_occupation_employee :: Employee -> Occupation
set_occupation_employee =
  HsBindgen.Runtime.ByteArray.setUnionPayload

data Person

-- void print_occupation (signed int arg1, union occupation *arg2)

foreign import capi safe "manual_examples.h print_occupation" print_occupation :: FC.CInt -> (F.Ptr Occupation) -> IO ()

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

-- void 拜拜 (void)

foreign import capi safe "manual_examples.h 拜拜" 拜拜 :: IO ()

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

-- void ϒ (void)

foreign import capi safe "manual_examples.h ϒ" cϒ :: IO ()

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

-- void import (void)

foreign import capi safe "manual_examples.h import" import' :: IO ()

newtype Signal = Signal
  { un_Signal :: FC.CUInt
  }

instance F.Storable Signal where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Signal
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Signal un_Signal2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Signal2

deriving stock instance Eq Signal

deriving stock instance Ord Signal

instance HsBindgen.Runtime.CEnum.CEnum Signal where

  type CEnumZ Signal = FC.CUInt

  toCEnum = Signal

  fromCEnum = un_Signal

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (1, Data.List.NonEmpty.singleton "Start")
                                                     , (2, Data.List.NonEmpty.singleton "Pause")
                                                     , (3, Data.List.NonEmpty.singleton "Resume")
                                                     , (4, Data.List.NonEmpty.singleton "Stop")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Signal"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Signal"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Signal where

  minDeclaredValue = Start

  maxDeclaredValue = Stop

instance Show Signal where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Signal where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern Start :: Signal
pattern Start = Signal 1

pattern Pause :: Signal
pattern Pause = Signal 2

pattern Resume :: Signal
pattern Resume = Signal 3

pattern Stop :: Signal
pattern Stop = Signal 4

newtype HTTP_status = HTTP_status
  { un_HTTP_status :: FC.CUInt
  }

instance F.Storable HTTP_status where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure HTTP_status
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          HTTP_status un_HTTP_status2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_HTTP_status2

deriving stock instance Eq HTTP_status

deriving stock instance Ord HTTP_status

instance HsBindgen.Runtime.CEnum.CEnum HTTP_status where

  type CEnumZ HTTP_status = FC.CUInt

  toCEnum = HTTP_status

  fromCEnum = un_HTTP_status

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (200, Data.List.NonEmpty.singleton "Ok")
                                                     , (301, Data.List.NonEmpty.singleton "Moved")
                                                     , (400, Data.List.NonEmpty.singleton "Bad_request")
                                                     , (401, Data.List.NonEmpty.singleton "Unauthorized")
                                                     , (404, Data.List.NonEmpty.singleton "Not_found")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "HTTP_status"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "HTTP_status"

instance Show HTTP_status where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read HTTP_status where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern Ok :: HTTP_status
pattern Ok = HTTP_status 200

pattern Moved :: HTTP_status
pattern Moved = HTTP_status 301

pattern Bad_request :: HTTP_status
pattern Bad_request = HTTP_status 400

pattern Unauthorized :: HTTP_status
pattern Unauthorized = HTTP_status 401

pattern Not_found :: HTTP_status
pattern Not_found = HTTP_status 404

newtype Descending = Descending
  { un_Descending :: FC.CUInt
  }

instance F.Storable Descending where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Descending
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Descending un_Descending2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Descending2

deriving stock instance Eq Descending

deriving stock instance Ord Descending

instance HsBindgen.Runtime.CEnum.CEnum Descending where

  type CEnumZ Descending = FC.CUInt

  toCEnum = Descending

  fromCEnum = un_Descending

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (98, Data.List.NonEmpty.singleton "Z")
                                                     , (99, ("Y" Data.List.NonEmpty.:| ["Y_alias"]))
                                                     , (100, Data.List.NonEmpty.singleton "X")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Descending"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Descending"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Descending where

  minDeclaredValue = Z

  maxDeclaredValue = X

instance Show Descending where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Descending where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern X :: Descending
pattern X = Descending 100

pattern Y :: Descending
pattern Y = Descending 99

pattern Y_alias :: Descending
pattern Y_alias = Descending 99

pattern Z :: Descending
pattern Z = Descending 98

newtype Result = Result
  { un_Result :: FC.CInt
  }

instance F.Storable Result where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Result
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Result un_Result2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Result2

deriving stock instance Eq Result

deriving stock instance Ord Result

instance HsBindgen.Runtime.CEnum.CEnum Result where

  type CEnumZ Result = FC.CInt

  toCEnum = Result

  fromCEnum = un_Result

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (-1, Data.List.NonEmpty.singleton "Failed")
                                                     , (0, Data.List.NonEmpty.singleton "Success")
                                                     , (1, Data.List.NonEmpty.singleton "Postponed")
                                                     , (2, Data.List.NonEmpty.singleton "Already_done")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Result"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Result"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Result where

  minDeclaredValue = Failed

  maxDeclaredValue = Already_done

instance Show Result where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Result where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern Failed :: Result
pattern Failed = Result (-1)

pattern Success :: Result
pattern Success = Result 0

pattern Postponed :: Result
pattern Postponed = Result 1

pattern Already_done :: Result
pattern Already_done = Result 2

newtype Vote = Vote
  { un_Vote :: FC.CUChar
  }

instance F.Storable Vote where

  sizeOf = \_ -> (1 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Vote
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vote un_Vote2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Vote2

deriving stock instance Eq Vote

deriving stock instance Ord Vote

instance HsBindgen.Runtime.CEnum.CEnum Vote where

  type CEnumZ Vote = FC.CUChar

  toCEnum = Vote

  fromCEnum = un_Vote

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "Infavour")
                                                     , (1, Data.List.NonEmpty.singleton "Against")
                                                     , (2, Data.List.NonEmpty.singleton "Abstain")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Vote"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Vote"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Vote where

  minDeclaredValue = Infavour

  maxDeclaredValue = Abstain

instance Show Vote where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Vote where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern Infavour :: Vote
pattern Infavour = Vote 0

pattern Against :: Vote
pattern Against = Vote 1

pattern Abstain :: Vote
pattern Abstain = Vote 2

newtype CXCursorKind = CXCursorKind
  { un_CXCursorKind :: FC.CUInt
  }

instance F.Storable CXCursorKind where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure CXCursorKind
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          CXCursorKind un_CXCursorKind2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_CXCursorKind2

deriving stock instance Eq CXCursorKind

deriving stock instance Ord CXCursorKind

instance HsBindgen.Runtime.CEnum.CEnum CXCursorKind where

  type CEnumZ CXCursorKind = FC.CUInt

  toCEnum = CXCursorKind

  fromCEnum = un_CXCursorKind

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (100, ("CXCursor_FirstExpr" Data.List.NonEmpty.:| ["CXCursor_UnexposedExpr"]))
                                                     , (101, Data.List.NonEmpty.singleton "CXCursor_DeclRefExpr")
                                                     , (102, Data.List.NonEmpty.singleton "CXCursor_MemberRefExpr")
                                                     , (156, ("CXCursor_PackIndexingExpr" Data.List.NonEmpty.:| ["CXCursor_LastExpr"]))
                                                     , (200, ("CXCursor_FirstStmt" Data.List.NonEmpty.:| ["CXCursor_UnexposedStmt"]))
                                                     , (201, Data.List.NonEmpty.singleton "CXCursor_LabelStmt")
                                                     , (202, Data.List.NonEmpty.singleton "CXCursor_CompoundStmt")
                                                     , ( 331
                                                       , ("CXCursor_OpenACCUpdateConstruct" Data.List.NonEmpty.:| ["CXCursor_LastStmt"])
                                                       )
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "CXCursorKind"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "CXCursorKind"

instance Show CXCursorKind where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read CXCursorKind where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern CXCursor_FirstExpr :: CXCursorKind
pattern CXCursor_FirstExpr = CXCursorKind 100

pattern CXCursor_UnexposedExpr :: CXCursorKind
pattern CXCursor_UnexposedExpr = CXCursorKind 100

pattern CXCursor_DeclRefExpr :: CXCursorKind
pattern CXCursor_DeclRefExpr = CXCursorKind 101

pattern CXCursor_MemberRefExpr :: CXCursorKind
pattern CXCursor_MemberRefExpr = CXCursorKind 102

pattern CXCursor_PackIndexingExpr :: CXCursorKind
pattern CXCursor_PackIndexingExpr = CXCursorKind 156

pattern CXCursor_LastExpr :: CXCursorKind
pattern CXCursor_LastExpr = CXCursorKind 156

pattern CXCursor_FirstStmt :: CXCursorKind
pattern CXCursor_FirstStmt = CXCursorKind 200

pattern CXCursor_UnexposedStmt :: CXCursorKind
pattern CXCursor_UnexposedStmt = CXCursorKind 200

pattern CXCursor_LabelStmt :: CXCursorKind
pattern CXCursor_LabelStmt = CXCursorKind 201

pattern CXCursor_CompoundStmt :: CXCursorKind
pattern CXCursor_CompoundStmt = CXCursorKind 202

pattern CXCursor_OpenACCUpdateConstruct :: CXCursorKind
pattern CXCursor_OpenACCUpdateConstruct = CXCursorKind 331

pattern CXCursor_LastStmt :: CXCursorKind
pattern CXCursor_LastStmt = CXCursorKind 331
