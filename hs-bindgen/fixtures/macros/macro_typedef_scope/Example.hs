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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @T1@

    __defined at:__ @macros\/macro_typedef_scope.h 4:9@

    __exported by:__ @macros\/macro_typedef_scope.h@
-}
newtype T1 = T1
  { un_T1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @T2@

    __defined at:__ @macros\/macro_typedef_scope.h 5:12@

    __exported by:__ @macros\/macro_typedef_scope.h@
-}
newtype T2 = T2
  { un_T2 :: T1
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T2) "un_T2")
         ) => GHC.Records.HasField "un_T2" (Ptr.Ptr T2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_T2")

instance HsBindgen.Runtime.HasCField.HasCField T2 "un_T2" where

  type CFieldType T2 "un_T2" = T1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T3@

    __defined at:__ @macros\/macro_typedef_scope.h 6:9@

    __exported by:__ @macros\/macro_typedef_scope.h@
-}
newtype T3 = T3
  { un_T3 :: T2
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @T4@

    __defined at:__ @macros\/macro_typedef_scope.h 7:12@

    __exported by:__ @macros\/macro_typedef_scope.h@
-}
newtype T4 = T4
  { un_T4 :: T3
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T4) "un_T4")
         ) => GHC.Records.HasField "un_T4" (Ptr.Ptr T4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_T4")

instance HsBindgen.Runtime.HasCField.HasCField T4 "un_T4" where

  type CFieldType T4 "un_T4" = T3

  offset# = \_ -> \_ -> 0
