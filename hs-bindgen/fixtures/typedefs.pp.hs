{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @myint@

    __defined at:__ @typedefs.h:1:13@

    __exported by:__ @typedefs.h@
-}
newtype Myint = Myint
  { un_Myint :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @intptr@

    __defined at:__ @typedefs.h:2:15@

    __exported by:__ @typedefs.h@
-}
newtype Intptr = Intptr
  { un_Intptr :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @int2int@

    __defined at:__ @typedefs.h:5:13@

    __exported by:__ @typedefs.h@
-}
newtype Int2int = Int2int
  { un_Int2int :: FC.CInt -> IO FC.CInt
  }

{-| Auxiliary type used by 'FunctionPointer_Function'

__defined at:__ @typedefs.h:8:16@

__exported by:__ @typedefs.h@
-}
newtype FunctionPointer_Function_Deref = FunctionPointer_Function_Deref
  { un_FunctionPointer_Function_Deref :: IO ()
  }

{-| __C declaration:__ @FunctionPointer_Function@

    __defined at:__ @typedefs.h:8:16@

    __exported by:__ @typedefs.h@
-}
newtype FunctionPointer_Function = FunctionPointer_Function
  { un_FunctionPointer_Function :: Ptr.FunPtr FunctionPointer_Function_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @NonFunctionPointer_Function@

    __defined at:__ @typedefs.h:9:14@

    __exported by:__ @typedefs.h@
-}
newtype NonFunctionPointer_Function = NonFunctionPointer_Function
  { un_NonFunctionPointer_Function :: FC.CInt -> IO FC.CInt
  }

{-| Auxiliary type used by 'F1'

__defined at:__ @typedefs.h:11:16@

__exported by:__ @typedefs.h@
-}
newtype F1_Deref = F1_Deref
  { un_F1_Deref :: IO ()
  }

{-| __C declaration:__ @f1@

    __defined at:__ @typedefs.h:11:16@

    __exported by:__ @typedefs.h@
-}
newtype F1 = F1
  { un_F1 :: Ptr.FunPtr F1_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @g1@

    __defined at:__ @typedefs.h:13:14@

    __exported by:__ @typedefs.h@
-}
newtype G1 = G1
  { un_G1 :: IO ()
  }

{-| __C declaration:__ @g2@

    __defined at:__ @typedefs.h:14:14@

    __exported by:__ @typedefs.h@
-}
newtype G2 = G2
  { un_G2 :: Ptr.FunPtr G1
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @h1@

    __defined at:__ @typedefs.h:16:14@

    __exported by:__ @typedefs.h@
-}
newtype H1 = H1
  { un_H1 :: IO ()
  }

{-| __C declaration:__ @h2@

    __defined at:__ @typedefs.h:17:12@

    __exported by:__ @typedefs.h@
-}
newtype H2 = H2
  { un_H2 :: H1
  }

{-| __C declaration:__ @h3@

    __defined at:__ @typedefs.h:18:14@

    __exported by:__ @typedefs.h@
-}
newtype H3 = H3
  { un_H3 :: Ptr.FunPtr H2
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
