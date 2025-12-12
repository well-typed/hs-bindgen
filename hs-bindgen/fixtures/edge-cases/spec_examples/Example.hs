{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

{-| Examples from the initial specification

__C declaration:__ @int16_T@

__defined at:__ @edge-cases\/spec_examples.h:10:15@

__exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int16_T = Int16_T
  { un_Int16_T :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int16_T) "un_Int16_T")
         ) => GHC.Records.HasField "un_Int16_T" (Ptr.Ptr Int16_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Int16_T")

instance HsBindgen.Runtime.HasCField.HasCField Int16_T "un_Int16_T" where

  type CFieldType Int16_T "un_Int16_T" = FC.CShort

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int32_T@

    __defined at:__ @edge-cases\/spec_examples.h:11:13@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int32_T = Int32_T
  { un_Int32_T :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int32_T) "un_Int32_T")
         ) => GHC.Records.HasField "un_Int32_T" (Ptr.Ptr Int32_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Int32_T")

instance HsBindgen.Runtime.HasCField.HasCField Int32_T "un_Int32_T" where

  type CFieldType Int32_T "un_Int32_T" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int64_T@

    __defined at:__ @edge-cases\/spec_examples.h:12:19@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
newtype Int64_T = Int64_T
  { un_Int64_T :: FC.CLLong
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int64_T) "un_Int64_T")
         ) => GHC.Records.HasField "un_Int64_T" (Ptr.Ptr Int64_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Int64_T")

instance HsBindgen.Runtime.HasCField.HasCField Int64_T "un_Int64_T" where

  type CFieldType Int64_T "un_Int64_T" = FC.CLLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @cint16_T@

    __defined at:__ @edge-cases\/spec_examples.h:14:9@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data Cint16_T = Cint16_T
  { cint16_T_re :: Int16_T
    {- ^ __C declaration:__ @re@

         __defined at:__ @edge-cases\/spec_examples.h:15:11@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , cint16_T_im :: Int16_T
    {- ^ __C declaration:__ @im@

         __defined at:__ @edge-cases\/spec_examples.h:16:11@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Cint16_T where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Cint16_T
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"cint16_T_re") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"cint16_T_im") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Cint16_T cint16_T_re2 cint16_T_im3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"cint16_T_re") ptr0 cint16_T_re2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"cint16_T_im") ptr0 cint16_T_im3

instance HsBindgen.Runtime.HasCField.HasCField Cint16_T "cint16_T_re" where

  type CFieldType Cint16_T "cint16_T_re" = Int16_T

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Cint16_T) "cint16_T_re")
         ) => GHC.Records.HasField "cint16_T_re" (Ptr.Ptr Cint16_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"cint16_T_re")

instance HsBindgen.Runtime.HasCField.HasCField Cint16_T "cint16_T_im" where

  type CFieldType Cint16_T "cint16_T_im" = Int16_T

  offset# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Cint16_T) "cint16_T_im")
         ) => GHC.Records.HasField "cint16_T_im" (Ptr.Ptr Cint16_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"cint16_T_im")

{-| __C declaration:__ @B@

    __defined at:__ @edge-cases\/spec_examples.h:19:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data B = B
  {}
  deriving stock (Eq, Show)

instance F.Storable B where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure B

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          B -> return ()

{-| __C declaration:__ @A@

    __defined at:__ @edge-cases\/spec_examples.h:23:8@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data A = A
  { a_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/spec_examples.h:24:10@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_label :: Ptr.Ptr FC.CChar
    {- ^ __C declaration:__ @label@

         __defined at:__ @edge-cases\/spec_examples.h:25:9@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_samples :: (HsBindgen.Runtime.ConstantArray.ConstantArray 128) FC.CChar
    {- ^ __C declaration:__ @samples@

         __defined at:__ @edge-cases\/spec_examples.h:26:8@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_b :: B
    {- ^ __C declaration:__ @b@

         __defined at:__ @edge-cases\/spec_examples.h:27:12@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  , a_c :: Ptr.Ptr C
    {- ^ __C declaration:__ @c@

         __defined at:__ @edge-cases\/spec_examples.h:28:13@

         __exported by:__ @edge-cases\/spec_examples.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable A where

  sizeOf = \_ -> (152 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure A
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"a_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"a_label") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"a_samples") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"a_b") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"a_c") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_x2 a_label3 a_samples4 a_b5 a_c6 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"a_x") ptr0 a_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"a_label") ptr0 a_label3
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"a_samples") ptr0 a_samples4
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"a_b") ptr0 a_b5
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"a_c") ptr0 a_c6

instance HsBindgen.Runtime.HasCField.HasCField A "a_x" where

  type CFieldType A "a_x" = FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_x")
         ) => GHC.Records.HasField "a_x" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"a_x")

instance HsBindgen.Runtime.HasCField.HasCField A "a_label" where

  type CFieldType A "a_label" = Ptr.Ptr FC.CChar

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_label")
         ) => GHC.Records.HasField "a_label" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"a_label")

instance HsBindgen.Runtime.HasCField.HasCField A "a_samples" where

  type CFieldType A "a_samples" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 128) FC.CChar

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_samples")
         ) => GHC.Records.HasField "a_samples" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"a_samples")

instance HsBindgen.Runtime.HasCField.HasCField A "a_b" where

  type CFieldType A "a_b" = B

  offset# = \_ -> \_ -> 144

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_b")
         ) => GHC.Records.HasField "a_b" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"a_b")

instance HsBindgen.Runtime.HasCField.HasCField A "a_c" where

  type CFieldType A "a_c" = Ptr.Ptr C

  offset# = \_ -> \_ -> 144

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_c")
         ) => GHC.Records.HasField "a_c" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"a_c")

{-| __C declaration:__ @C@

    __defined at:__ @edge-cases\/spec_examples.h:28:10@

    __exported by:__ @edge-cases\/spec_examples.h@
-}
data C
