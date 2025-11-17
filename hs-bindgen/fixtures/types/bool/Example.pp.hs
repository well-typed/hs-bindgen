{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
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
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @bools1@

    __defined at:__ @types\/bool.h:1:8@

    __exported by:__ @types\/bool.h@
-}
data Bools1 = Bools1
  { bools1_x :: FC.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/bool.h:2:11@

         __exported by:__ @types\/bool.h@
    -}
  , bools1_y :: FC.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/bool.h:3:11@

         __exported by:__ @types\/bool.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bools1 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools1
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bools1_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bools1_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools1 bools1_x2 bools1_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bools1_x") ptr0 bools1_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bools1_y") ptr0 bools1_y3

instance HsBindgen.Runtime.HasCField.HasCField Bools1 "bools1_x" where

  type CFieldType Bools1 "bools1_x" = FC.CBool

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bools1) "bools1_x")
         ) => GHC.Records.HasField "bools1_x" (Ptr.Ptr Bools1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bools1_x")

instance HsBindgen.Runtime.HasCField.HasCField Bools1 "bools1_y" where

  type CFieldType Bools1 "bools1_y" = FC.CBool

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bools1) "bools1_y")
         ) => GHC.Records.HasField "bools1_y" (Ptr.Ptr Bools1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bools1_y")

{-| __C declaration:__ @bools2@

    __defined at:__ @types\/bool.h:8:8@

    __exported by:__ @types\/bool.h@
-}
data Bools2 = Bools2
  { bools2_x :: FC.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/bool.h:9:10@

         __exported by:__ @types\/bool.h@
    -}
  , bools2_y :: FC.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/bool.h:10:10@

         __exported by:__ @types\/bool.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bools2 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools2
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bools2_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bools2_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools2 bools2_x2 bools2_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bools2_x") ptr0 bools2_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bools2_y") ptr0 bools2_y3

instance HsBindgen.Runtime.HasCField.HasCField Bools2 "bools2_x" where

  type CFieldType Bools2 "bools2_x" = FC.CBool

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bools2) "bools2_x")
         ) => GHC.Records.HasField "bools2_x" (Ptr.Ptr Bools2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bools2_x")

instance HsBindgen.Runtime.HasCField.HasCField Bools2 "bools2_y" where

  type CFieldType Bools2 "bools2_y" = FC.CBool

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bools2) "bools2_y")
         ) => GHC.Records.HasField "bools2_y" (Ptr.Ptr Bools2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bools2_y")

{-| __C declaration:__ @BOOL@

    __defined at:__ @types\/bool.h:13:9@

    __exported by:__ @types\/bool.h@
-}
newtype BOOL = BOOL
  { un_BOOL :: FC.CBool
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @bools3@

    __defined at:__ @types\/bool.h:15:8@

    __exported by:__ @types\/bool.h@
-}
data Bools3 = Bools3
  { bools3_x :: BOOL
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/bool.h:16:10@

         __exported by:__ @types\/bool.h@
    -}
  , bools3_y :: BOOL
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/bool.h:17:10@

         __exported by:__ @types\/bool.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bools3 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools3
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bools3_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"bools3_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools3 bools3_x2 bools3_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bools3_x") ptr0 bools3_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"bools3_y") ptr0 bools3_y3

instance HsBindgen.Runtime.HasCField.HasCField Bools3 "bools3_x" where

  type CFieldType Bools3 "bools3_x" = BOOL

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bools3) "bools3_x")
         ) => GHC.Records.HasField "bools3_x" (Ptr.Ptr Bools3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bools3_x")

instance HsBindgen.Runtime.HasCField.HasCField Bools3 "bools3_y" where

  type CFieldType Bools3 "bools3_y" = BOOL

  offset# = \_ -> \_ -> 1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bools3) "bools3_y")
         ) => GHC.Records.HasField "bools3_y" (Ptr.Ptr Bools3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"bools3_y")
