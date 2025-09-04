{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.ConstantArray
import Prelude (Bounded, Enum, Eq, Floating, Fractional, IO, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show)

{-| __C declaration:__ @PtrInt@

    __defined at:__ @macro_types.h:2:9@

    __exported by:__ @macro_types.h@
-}
newtype PtrInt = PtrInt
  { un_PtrInt :: F.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @PtrPtrChar@

    __defined at:__ @macro_types.h:5:9@

    __exported by:__ @macro_types.h@
-}
newtype PtrPtrChar = PtrPtrChar
  { un_PtrPtrChar :: F.Ptr (F.Ptr FC.CChar)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Arr1@

    __defined at:__ @macro_types.h:8:9@

    __exported by:__ @macro_types.h@
-}
newtype Arr1 = Arr1
  { un_Arr1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 2) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Arr2@

    __defined at:__ @macro_types.h:11:9@

    __exported by:__ @macro_types.h@
-}
newtype Arr2 = Arr2
  { un_Arr2 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) (F.Ptr FC.CFloat)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Arr3@

    __defined at:__ @macro_types.h:14:9@

    __exported by:__ @macro_types.h@
-}
newtype Arr3 = Arr3
  { un_Arr3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) (F.FunPtr (FC.CDouble -> IO FC.CFloat))
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Fun1@

    __defined at:__ @macro_types.h:17:9@

    __exported by:__ @macro_types.h@
-}
newtype Fun1 = Fun1
  { un_Fun1 :: FC.CInt -> IO (F.Ptr FC.CFloat)
  }

{-| __C declaration:__ @Fun2@

    __defined at:__ @macro_types.h:20:9@

    __exported by:__ @macro_types.h@
-}
newtype Fun2 = Fun2
  { un_Fun2 :: F.FunPtr (FC.CFloat -> (F.Ptr FC.CDouble) -> IO FC.CInt)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Fun3@

    __defined at:__ @macro_types.h:23:9@

    __exported by:__ @macro_types.h@
-}
newtype Fun3 = Fun3
  { un_Fun3 :: F.FunPtr ((F.Ptr FC.CFloat) -> IO (F.Ptr FC.CInt))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @Fun4@

    __defined at:__ @macro_types.h:26:9@

    __exported by:__ @macro_types.h@
-}
newtype Fun4 = Fun4
  { un_Fun4 :: FC.CInt -> (F.Ptr FC.CLong) -> IO (F.FunPtr (FC.CFloat -> (F.Ptr FC.CDouble) -> IO (F.Ptr FC.CLong)))
  }

{-| __C declaration:__ @Fun5@

    __defined at:__ @macro_types.h:29:9@

    __exported by:__ @macro_types.h@
-}
newtype Fun5 = Fun5
  { un_Fun5 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 8) FC.CChar) -> IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) (F.Ptr FC.CShort)))
  }

{-| __C declaration:__ @MTy@

    __defined at:__ @macro_types.h:33:9@

    __exported by:__ @macro_types.h@
-}
newtype MTy = MTy
  { un_MTy :: FC.CFloat
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

{-| __C declaration:__ @tty@

    __defined at:__ @macro_types.h:34:13@

    __exported by:__ @macro_types.h@
-}
newtype Tty = Tty
  { un_Tty :: MTy
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Enum, Floating, Fractional, Num, Real, RealFloat, RealFrac)

{-| __C declaration:__ @UINT8_T@

    __defined at:__ @macro_types.h:36:9@

    __exported by:__ @macro_types.h@
-}
newtype UINT8_T = UINT8_T
  { un_UINT8_T :: FC.CUChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @BOOLEAN_T@

    __defined at:__ @macro_types.h:37:9@

    __exported by:__ @macro_types.h@
-}
newtype BOOLEAN_T = BOOLEAN_T
  { un_BOOLEAN_T :: UINT8_T
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @boolean_T@

    __defined at:__ @macro_types.h:38:19@

    __exported by:__ @macro_types.h@
-}
newtype Boolean_T = Boolean_T
  { un_Boolean_T :: BOOLEAN_T
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
