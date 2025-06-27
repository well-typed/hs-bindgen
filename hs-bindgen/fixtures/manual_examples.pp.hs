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
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Floating, Fractional, IO, Int, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show, pure, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include \"manual_examples.h\"\nvoid testmodule_mk_triple (signed int arg1, signed int arg2, signed int arg3, triple *arg4) { mk_triple(arg1, arg2, arg3, arg4); }\nsigned int testmodule_index_triple (triple *arg1, index arg2) { return index_triple(arg1, arg2); }\nsum testmodule_sum_triple (triple *arg1) { return sum_triple(arg1); }\naverage testmodule_average_triple (triple *arg1) { return average_triple(arg1); }\nYEAR testmodule_getYear (date *arg1) { return getYear(arg1); }\nvoid testmodule_print_occupation (signed int arg1, occupation *arg2) { print_occupation(arg1, arg2); }\nvoid testmodule_\25308\25308 (void) { \25308\25308(); }\nvoid testmodule_\978 (void) { \978(); }\nvoid testmodule_import (void) { import(); }\nsigned int testmodule_mod_10 (signed int arg1) { return mod_10(arg1); }\n")

data Triple = Triple
  { triple_a :: FC.CInt
  , triple_b :: FC.CInt
  , triple_c :: FC.CInt
  }
  deriving stock (Eq, Show)

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

foreign import ccall safe "testmodule_mk_triple" mk_triple :: FC.CInt -> FC.CInt -> FC.CInt -> (F.Ptr Triple) -> IO ()

newtype Index = Index
  { un_Index :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum Index where

  type CEnumZ Index = FC.CUInt

  toCEnum = Index

  fromCEnum = un_Index

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "A"), (1, Data.List.NonEmpty.singleton "B"), (2, Data.List.NonEmpty.singleton "C")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Index"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Index"

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

foreign import ccall safe "testmodule_index_triple" index_triple :: (F.Ptr Triple) -> Index -> IO FC.CInt

newtype Sum = Sum
  { un_Sum :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Average = Average
  { un_Average :: FC.CDouble
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

foreign import ccall safe "testmodule_sum_triple" sum_triple :: (F.Ptr Triple) -> IO Sum

foreign import ccall safe "testmodule_average_triple" average_triple :: (F.Ptr Triple) -> IO Average

fIELD_OFFSET :: FC.CInt
fIELD_OFFSET = (4 :: FC.CInt)

ePSILON :: FC.CDouble
ePSILON = (0.1 :: FC.CDouble)

pTR_TO_FIELD :: forall a0. (C.Add a0) FC.CInt => a0 -> (C.AddRes a0) FC.CInt
pTR_TO_FIELD = \ptr0 -> (+) ptr0 (4 :: FC.CInt)

newtype YEAR = YEAR
  { un_YEAR :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype MONTH = MONTH
  { un_MONTH :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype DAY = DAY
  { un_DAY :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data Date = Date
  { date_year :: YEAR
  , date_month :: MONTH
  , date_day :: DAY
  }
  deriving stock (Eq, Show)

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

foreign import ccall safe "testmodule_getYear" getYear :: (F.Ptr Date) -> IO YEAR

data Student = Student
  { student_university :: F.Ptr FC.CChar
  , student_year :: FC.CInt
  }
  deriving stock (Eq, Show)

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

data Person

data Employee = Employee
  { employee_company :: F.Ptr FC.CChar
  , employee_supervisor :: F.Ptr Person
  , employee_salary :: FC.CInt
  }
  deriving stock (Eq, Show)

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

foreign import ccall safe "testmodule_print_occupation" print_occupation :: FC.CInt -> (F.Ptr Occupation) -> IO ()

data Rect_lower_left = Rect_lower_left
  { rect_lower_left_x :: FC.CInt
  , rect_lower_left_y :: FC.CInt
  }
  deriving stock (Eq, Show)

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

data Rect_upper_right = Rect_upper_right
  { rect_upper_right_x :: FC.CInt
  , rect_upper_right_y :: FC.CInt
  }
  deriving stock (Eq, Show)

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

data Rect = Rect
  { rect_lower_left :: Rect_lower_left
  , rect_upper_right :: Rect_upper_right
  }
  deriving stock (Eq, Show)

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

data Config_Deref = Config_Deref
  { config_Deref_width :: FC.CInt
  , config_Deref_height :: FC.CInt
  }
  deriving stock (Eq, Show)

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

newtype Config = Config
  { un_Config :: F.Ptr Config_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "testmodule_拜拜" 拜拜 :: IO ()

newtype C数字 = C数字
  { un_C数字 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "testmodule_ϒ" cϒ :: IO ()

newtype Data = Data
  { un_Data :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "testmodule_import" import' :: IO ()

newtype Signal = Signal
  { un_Signal :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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
          Signal un_Signal2 -> F.pokeByteOff ptr0 (0 :: Int) un_Signal2

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

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Signal"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Signal"

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
  deriving stock (Eq, Ord)

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
          HTTP_status un_HTTP_status2 -> F.pokeByteOff ptr0 (0 :: Int) un_HTTP_status2

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

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "HTTP_status"

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
  deriving stock (Eq, Ord)

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
          Descending un_Descending2 -> F.pokeByteOff ptr0 (0 :: Int) un_Descending2

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

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Descending"

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
  deriving stock (Eq, Ord)

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
          Result un_Result2 -> F.pokeByteOff ptr0 (0 :: Int) un_Result2

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

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Result"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Result"

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
  deriving stock (Eq, Ord)

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
          Vote un_Vote2 -> F.pokeByteOff ptr0 (0 :: Int) un_Vote2

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

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Vote"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Vote"

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
  deriving stock (Eq, Ord)

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
          CXCursorKind un_CXCursorKind2 -> F.pokeByteOff ptr0 (0 :: Int) un_CXCursorKind2

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
                                                     , (331, ("CXCursor_OpenACCUpdateConstruct" Data.List.NonEmpty.:| ["CXCursor_LastStmt"]))
                                                     ]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "CXCursorKind"

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

foreign import ccall safe "testmodule_mod_10" mod_10 :: FC.CInt -> IO FC.CInt
