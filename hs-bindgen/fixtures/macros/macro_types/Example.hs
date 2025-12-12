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
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, Floating, Fractional, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

{-| __C declaration:__ @PtrInt@

    __defined at:__ @macros\/macro_types.h:2:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrInt = PtrInt
  { un_PtrInt :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| __C declaration:__ @PtrPtrChar@

    __defined at:__ @macros\/macro_types.h:5:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype PtrPtrChar = PtrPtrChar
  { un_PtrPtrChar :: Ptr.Ptr (Ptr.Ptr FC.CChar)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| __C declaration:__ @MTy@

    __defined at:__ @macros\/macro_types.h:8:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype MTy = MTy
  { un_MTy :: FC.CFloat
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

{-| __C declaration:__ @tty@

    __defined at:__ @macros\/macro_types.h:9:13@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Tty = Tty
  { un_Tty :: MTy
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tty) "un_Tty")
         ) => GHC.Records.HasField "un_Tty" (Ptr.Ptr Tty) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Tty")

instance HsBindgen.Runtime.HasCField.HasCField Tty "un_Tty" where

  type CFieldType Tty "un_Tty" = MTy

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @UINT8_T@

    __defined at:__ @macros\/macro_types.h:11:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype UINT8_T = UINT8_T
  { un_UINT8_T :: FC.CUChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @BOOLEAN_T@

    __defined at:__ @macros\/macro_types.h:12:9@

    __exported by:__ @macros\/macro_types.h@
-}
newtype BOOLEAN_T = BOOLEAN_T
  { un_BOOLEAN_T :: UINT8_T
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @boolean_T@

    __defined at:__ @macros\/macro_types.h:13:19@

    __exported by:__ @macros\/macro_types.h@
-}
newtype Boolean_T = Boolean_T
  { un_Boolean_T :: BOOLEAN_T
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Boolean_T) "un_Boolean_T")
         ) => GHC.Records.HasField "un_Boolean_T" (Ptr.Ptr Boolean_T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Boolean_T")

instance HsBindgen.Runtime.HasCField.HasCField Boolean_T "un_Boolean_T" where

  type CFieldType Boolean_T "un_Boolean_T" = BOOLEAN_T

  offset# = \_ -> \_ -> 0
