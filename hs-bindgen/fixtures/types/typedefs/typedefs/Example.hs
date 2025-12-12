{-# LANGUAGE CApiFFI #-}
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
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified Prelude as P
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @myint@

    __defined at:__ @types\/typedefs\/typedefs.h:1:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Myint = Myint
  { un_Myint :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Myint) "un_Myint")
         ) => GHC.Records.HasField "un_Myint" (Ptr.Ptr Myint) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Myint")

instance HsBindgen.Runtime.HasCField.HasCField Myint "un_Myint" where

  type CFieldType Myint "un_Myint" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @intptr@

    __defined at:__ @types\/typedefs\/typedefs.h:2:15@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Intptr = Intptr
  { un_Intptr :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Intptr) "un_Intptr")
         ) => GHC.Records.HasField "un_Intptr" (Ptr.Ptr Intptr) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Intptr")

instance HsBindgen.Runtime.HasCField.HasCField Intptr "un_Intptr" where

  type CFieldType Intptr "un_Intptr" = Ptr.Ptr FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @int2int@

    __defined at:__ @types\/typedefs\/typedefs.h:5:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Int2int = Int2int
  { un_Int2int :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| This is an internal function.
-}
foreign import ccall safe "wrapper" toInt2int_base ::
     (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

toInt2int ::
     Int2int
  -> IO (Ptr.FunPtr Int2int)
toInt2int =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (toInt2int_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

{-| This is an internal function.
-}
foreign import ccall safe "dynamic" fromInt2int_base ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt -> IO FC.CInt

fromInt2int ::
     Ptr.FunPtr Int2int
  -> Int2int
fromInt2int =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (fromInt2int_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Int2int where

  toFunPtr = toInt2int

instance HsBindgen.Runtime.FunPtr.FromFunPtr Int2int where

  fromFunPtr = fromInt2int

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int2int) "un_Int2int")
         ) => GHC.Records.HasField "un_Int2int" (Ptr.Ptr Int2int) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Int2int")

instance HsBindgen.Runtime.HasCField.HasCField Int2int "un_Int2int" where

  type CFieldType Int2int "un_Int2int" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'FunctionPointer_Function'

__defined at:__ @types\/typedefs\/typedefs.h:8:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function_Deref = FunctionPointer_Function_Deref
  { un_FunctionPointer_Function_Deref :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| This is an internal function.
-}
foreign import ccall safe "wrapper" toFunctionPointer_Function_Deref_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

toFunctionPointer_Function_Deref ::
     FunctionPointer_Function_Deref
  -> IO (Ptr.FunPtr FunctionPointer_Function_Deref)
toFunctionPointer_Function_Deref =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (toFunctionPointer_Function_Deref_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

{-| This is an internal function.
-}
foreign import ccall safe "dynamic" fromFunctionPointer_Function_Deref_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

fromFunctionPointer_Function_Deref ::
     Ptr.FunPtr FunctionPointer_Function_Deref
  -> FunctionPointer_Function_Deref
fromFunctionPointer_Function_Deref =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (fromFunctionPointer_Function_Deref_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr FunctionPointer_Function_Deref where

  toFunPtr = toFunctionPointer_Function_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr FunctionPointer_Function_Deref where

  fromFunPtr = fromFunctionPointer_Function_Deref

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FunctionPointer_Function_Deref) "un_FunctionPointer_Function_Deref")
         ) => GHC.Records.HasField "un_FunctionPointer_Function_Deref" (Ptr.Ptr FunctionPointer_Function_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_FunctionPointer_Function_Deref")

instance HsBindgen.Runtime.HasCField.HasCField FunctionPointer_Function_Deref "un_FunctionPointer_Function_Deref" where

  type CFieldType FunctionPointer_Function_Deref "un_FunctionPointer_Function_Deref" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h:8:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function = FunctionPointer_Function
  { un_FunctionPointer_Function :: Ptr.FunPtr FunctionPointer_Function_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FunctionPointer_Function) "un_FunctionPointer_Function")
         ) => GHC.Records.HasField "un_FunctionPointer_Function" (Ptr.Ptr FunctionPointer_Function) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_FunctionPointer_Function")

instance HsBindgen.Runtime.HasCField.HasCField FunctionPointer_Function "un_FunctionPointer_Function" where

  type CFieldType FunctionPointer_Function "un_FunctionPointer_Function" =
    Ptr.FunPtr FunctionPointer_Function_Deref

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @NonFunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h:9:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype NonFunctionPointer_Function = NonFunctionPointer_Function
  { un_NonFunctionPointer_Function :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| This is an internal function.
-}
foreign import ccall safe "wrapper" toNonFunctionPointer_Function_base ::
     (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

toNonFunctionPointer_Function ::
     NonFunctionPointer_Function
  -> IO (Ptr.FunPtr NonFunctionPointer_Function)
toNonFunctionPointer_Function =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (toNonFunctionPointer_Function_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

{-| This is an internal function.
-}
foreign import ccall safe "dynamic" fromNonFunctionPointer_Function_base ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt -> IO FC.CInt

fromNonFunctionPointer_Function ::
     Ptr.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function
fromNonFunctionPointer_Function =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (fromNonFunctionPointer_Function_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr NonFunctionPointer_Function where

  toFunPtr = toNonFunctionPointer_Function

instance HsBindgen.Runtime.FunPtr.FromFunPtr NonFunctionPointer_Function where

  fromFunPtr = fromNonFunctionPointer_Function

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType NonFunctionPointer_Function) "un_NonFunctionPointer_Function")
         ) => GHC.Records.HasField "un_NonFunctionPointer_Function" (Ptr.Ptr NonFunctionPointer_Function) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_NonFunctionPointer_Function")

instance HsBindgen.Runtime.HasCField.HasCField NonFunctionPointer_Function "un_NonFunctionPointer_Function" where

  type CFieldType NonFunctionPointer_Function "un_NonFunctionPointer_Function" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F1'

__defined at:__ @types\/typedefs\/typedefs.h:11:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1_Deref = F1_Deref
  { un_F1_Deref :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| This is an internal function.
-}
foreign import ccall safe "wrapper" toF1_Deref_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

toF1_Deref ::
     F1_Deref
  -> IO (Ptr.FunPtr F1_Deref)
toF1_Deref =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (toF1_Deref_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

{-| This is an internal function.
-}
foreign import ccall safe "dynamic" fromF1_Deref_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

fromF1_Deref ::
     Ptr.FunPtr F1_Deref
  -> F1_Deref
fromF1_Deref =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (fromF1_Deref_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr F1_Deref where

  toFunPtr = toF1_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr F1_Deref where

  fromFunPtr = fromF1_Deref

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F1_Deref) "un_F1_Deref")
         ) => GHC.Records.HasField "un_F1_Deref" (Ptr.Ptr F1_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F1_Deref")

instance HsBindgen.Runtime.HasCField.HasCField F1_Deref "un_F1_Deref" where

  type CFieldType F1_Deref "un_F1_Deref" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/typedefs.h:11:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1 = F1
  { un_F1 :: Ptr.FunPtr F1_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F1) "un_F1")
         ) => GHC.Records.HasField "un_F1" (Ptr.Ptr F1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F1")

instance HsBindgen.Runtime.HasCField.HasCField F1 "un_F1" where

  type CFieldType F1 "un_F1" = Ptr.FunPtr F1_Deref

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g1@

    __defined at:__ @types\/typedefs\/typedefs.h:13:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G1 = G1
  { un_G1 :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| This is an internal function.
-}
foreign import ccall safe "wrapper" toG1_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

toG1 ::
     G1
  -> IO (Ptr.FunPtr G1)
toG1 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (toG1_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

{-| This is an internal function.
-}
foreign import ccall safe "dynamic" fromG1_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

fromG1 ::
     Ptr.FunPtr G1
  -> G1
fromG1 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (fromG1_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr G1 where

  toFunPtr = toG1

instance HsBindgen.Runtime.FunPtr.FromFunPtr G1 where

  fromFunPtr = fromG1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType G1) "un_G1")
         ) => GHC.Records.HasField "un_G1" (Ptr.Ptr G1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_G1")

instance HsBindgen.Runtime.HasCField.HasCField G1 "un_G1" where

  type CFieldType G1 "un_G1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g2@

    __defined at:__ @types\/typedefs\/typedefs.h:14:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G2 = G2
  { un_G2 :: Ptr.FunPtr G1
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType G2) "un_G2")
         ) => GHC.Records.HasField "un_G2" (Ptr.Ptr G2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_G2")

instance HsBindgen.Runtime.HasCField.HasCField G2 "un_G2" where

  type CFieldType G2 "un_G2" = Ptr.FunPtr G1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h1@

    __defined at:__ @types\/typedefs\/typedefs.h:16:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H1 = H1
  { un_H1 :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| This is an internal function.
-}
foreign import ccall safe "wrapper" toH1_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

toH1 ::
     H1
  -> IO (Ptr.FunPtr H1)
toH1 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (toH1_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

{-| This is an internal function.
-}
foreign import ccall safe "dynamic" fromH1_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

fromH1 ::
     Ptr.FunPtr H1
  -> H1
fromH1 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (fromH1_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr H1 where

  toFunPtr = toH1

instance HsBindgen.Runtime.FunPtr.FromFunPtr H1 where

  fromFunPtr = fromH1

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType H1) "un_H1")
         ) => GHC.Records.HasField "un_H1" (Ptr.Ptr H1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_H1")

instance HsBindgen.Runtime.HasCField.HasCField H1 "un_H1" where

  type CFieldType H1 "un_H1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h2@

    __defined at:__ @types\/typedefs\/typedefs.h:17:12@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H2 = H2
  { un_H2 :: H1
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType H2) "un_H2")
         ) => GHC.Records.HasField "un_H2" (Ptr.Ptr H2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_H2")

instance HsBindgen.Runtime.HasCField.HasCField H2 "un_H2" where

  type CFieldType H2 "un_H2" = H1

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h3@

    __defined at:__ @types\/typedefs\/typedefs.h:18:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H3 = H3
  { un_H3 :: Ptr.FunPtr H2
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType H3) "un_H3")
         ) => GHC.Records.HasField "un_H3" (Ptr.Ptr H3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_H3")

instance HsBindgen.Runtime.HasCField.HasCField H3 "un_H3" where

  type CFieldType H3 "un_H3" = Ptr.FunPtr H2

  offset# = \_ -> \_ -> 0
