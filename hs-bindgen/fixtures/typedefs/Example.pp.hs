{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.FunPtr
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

foreign import ccall safe "wrapper" toInt2int ::
     Int2int
  -> IO (Ptr.FunPtr Int2int)

foreign import ccall safe "dynamic" fromInt2int ::
     Ptr.FunPtr Int2int
  -> Int2int

instance HsBindgen.Runtime.FunPtr.ToFunPtr Int2int where

  toFunPtr = toInt2int

instance HsBindgen.Runtime.FunPtr.FromFunPtr Int2int where

  fromFunPtr = fromInt2int

{-| Auxiliary type used by 'FunctionPointer_Function'

__defined at:__ @typedefs.h:8:16@

__exported by:__ @typedefs.h@
-}
newtype FunctionPointer_Function_Deref = FunctionPointer_Function_Deref
  { un_FunctionPointer_Function_Deref :: IO ()
  }

foreign import ccall safe "wrapper" toFunctionPointer_Function_Deref ::
     FunctionPointer_Function_Deref
  -> IO (Ptr.FunPtr FunctionPointer_Function_Deref)

foreign import ccall safe "dynamic" fromFunctionPointer_Function_Deref ::
     Ptr.FunPtr FunctionPointer_Function_Deref
  -> FunctionPointer_Function_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr FunctionPointer_Function_Deref where

  toFunPtr = toFunctionPointer_Function_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr FunctionPointer_Function_Deref where

  fromFunPtr = fromFunctionPointer_Function_Deref

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

foreign import ccall safe "wrapper" toNonFunctionPointer_Function ::
     NonFunctionPointer_Function
  -> IO (Ptr.FunPtr NonFunctionPointer_Function)

foreign import ccall safe "dynamic" fromNonFunctionPointer_Function ::
     Ptr.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function

instance HsBindgen.Runtime.FunPtr.ToFunPtr NonFunctionPointer_Function where

  toFunPtr = toNonFunctionPointer_Function

instance HsBindgen.Runtime.FunPtr.FromFunPtr NonFunctionPointer_Function where

  fromFunPtr = fromNonFunctionPointer_Function

{-| Auxiliary type used by 'F1'

__defined at:__ @typedefs.h:11:16@

__exported by:__ @typedefs.h@
-}
newtype F1_Deref = F1_Deref
  { un_F1_Deref :: IO ()
  }

foreign import ccall safe "wrapper" toF1_Deref ::
     F1_Deref
  -> IO (Ptr.FunPtr F1_Deref)

foreign import ccall safe "dynamic" fromF1_Deref ::
     Ptr.FunPtr F1_Deref
  -> F1_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr F1_Deref where

  toFunPtr = toF1_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr F1_Deref where

  fromFunPtr = fromF1_Deref

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

foreign import ccall safe "wrapper" toG1 ::
     G1
  -> IO (Ptr.FunPtr G1)

foreign import ccall safe "dynamic" fromG1 ::
     Ptr.FunPtr G1
  -> G1

instance HsBindgen.Runtime.FunPtr.ToFunPtr G1 where

  toFunPtr = toG1

instance HsBindgen.Runtime.FunPtr.FromFunPtr G1 where

  fromFunPtr = fromG1

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

foreign import ccall safe "wrapper" toH1 ::
     H1
  -> IO (Ptr.FunPtr H1)

foreign import ccall safe "dynamic" fromH1 ::
     Ptr.FunPtr H1
  -> H1

instance HsBindgen.Runtime.FunPtr.ToFunPtr H1 where

  toFunPtr = toH1

instance HsBindgen.Runtime.FunPtr.FromFunPtr H1 where

  fromFunPtr = fromH1

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
