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
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified Prelude as P
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @myint@

    __defined at:__ @types\/typedefs\/typedefs.h 1:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Myint = Myint
  { un_Myint :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Myint) "un_Myint")
         ) => GHC.Records.HasField "un_Myint" (Ptr.Ptr Myint) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Myint")

instance HsBindgen.Runtime.HasCField.HasCField Myint "un_Myint" where

  type CFieldType Myint "un_Myint" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @intptr@

    __defined at:__ @types\/typedefs\/typedefs.h 2:15@

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

    __defined at:__ @types\/typedefs\/typedefs.h 5:13@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype Int2int = Int2int
  { un_Int2int :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_a6c7dd49f5b9d470_base ::
     (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

-- __unique:__ @toInt2int@
hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (Ptr.FunPtr Int2int)
hs_bindgen_a6c7dd49f5b9d470 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_a6c7dd49f5b9d470_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_65378a8a3cf640ad_base ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt -> IO FC.CInt

-- __unique:__ @fromInt2int@
hs_bindgen_65378a8a3cf640ad ::
     Ptr.FunPtr Int2int
  -> Int2int
hs_bindgen_65378a8a3cf640ad =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_65378a8a3cf640ad_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Int2int where

  toFunPtr = hs_bindgen_a6c7dd49f5b9d470

instance HsBindgen.Runtime.FunPtr.FromFunPtr Int2int where

  fromFunPtr = hs_bindgen_65378a8a3cf640ad

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int2int) "un_Int2int")
         ) => GHC.Records.HasField "un_Int2int" (Ptr.Ptr Int2int) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Int2int")

instance HsBindgen.Runtime.HasCField.HasCField Int2int "un_Int2int" where

  type CFieldType Int2int "un_Int2int" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'FunctionPointer_Function'

__C declaration:__ @FunctionPointer_Function@

__defined at:__ @types\/typedefs\/typedefs.h 8:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function_Aux = FunctionPointer_Function_Aux
  { un_FunctionPointer_Function_Aux :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_b171c028cdc0781d_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toFunctionPointer_Function_Aux@
hs_bindgen_b171c028cdc0781d ::
     FunctionPointer_Function_Aux
  -> IO (Ptr.FunPtr FunctionPointer_Function_Aux)
hs_bindgen_b171c028cdc0781d =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_b171c028cdc0781d_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_4c3da8240a31e036_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFunctionPointer_Function_Aux@
hs_bindgen_4c3da8240a31e036 ::
     Ptr.FunPtr FunctionPointer_Function_Aux
  -> FunctionPointer_Function_Aux
hs_bindgen_4c3da8240a31e036 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_4c3da8240a31e036_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr FunctionPointer_Function_Aux where

  toFunPtr = hs_bindgen_b171c028cdc0781d

instance HsBindgen.Runtime.FunPtr.FromFunPtr FunctionPointer_Function_Aux where

  fromFunPtr = hs_bindgen_4c3da8240a31e036

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FunctionPointer_Function_Aux) "un_FunctionPointer_Function_Aux")
         ) => GHC.Records.HasField "un_FunctionPointer_Function_Aux" (Ptr.Ptr FunctionPointer_Function_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_FunctionPointer_Function_Aux")

instance HsBindgen.Runtime.HasCField.HasCField FunctionPointer_Function_Aux "un_FunctionPointer_Function_Aux" where

  type CFieldType FunctionPointer_Function_Aux "un_FunctionPointer_Function_Aux" =
    IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 8:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype FunctionPointer_Function = FunctionPointer_Function
  { un_FunctionPointer_Function :: Ptr.FunPtr FunctionPointer_Function_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType FunctionPointer_Function) "un_FunctionPointer_Function")
         ) => GHC.Records.HasField "un_FunctionPointer_Function" (Ptr.Ptr FunctionPointer_Function) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_FunctionPointer_Function")

instance HsBindgen.Runtime.HasCField.HasCField FunctionPointer_Function "un_FunctionPointer_Function" where

  type CFieldType FunctionPointer_Function "un_FunctionPointer_Function" =
    Ptr.FunPtr FunctionPointer_Function_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @NonFunctionPointer_Function@

    __defined at:__ @types\/typedefs\/typedefs.h 9:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype NonFunctionPointer_Function = NonFunctionPointer_Function
  { un_NonFunctionPointer_Function :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_766ae751d60365e9_base ::
     (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

-- __unique:__ @toNonFunctionPointer_Function@
hs_bindgen_766ae751d60365e9 ::
     NonFunctionPointer_Function
  -> IO (Ptr.FunPtr NonFunctionPointer_Function)
hs_bindgen_766ae751d60365e9 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_766ae751d60365e9_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_36c7108d046bcbc3_base ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt -> IO FC.CInt

-- __unique:__ @fromNonFunctionPointer_Function@
hs_bindgen_36c7108d046bcbc3 ::
     Ptr.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function
hs_bindgen_36c7108d046bcbc3 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_36c7108d046bcbc3_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr NonFunctionPointer_Function where

  toFunPtr = hs_bindgen_766ae751d60365e9

instance HsBindgen.Runtime.FunPtr.FromFunPtr NonFunctionPointer_Function where

  fromFunPtr = hs_bindgen_36c7108d046bcbc3

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType NonFunctionPointer_Function) "un_NonFunctionPointer_Function")
         ) => GHC.Records.HasField "un_NonFunctionPointer_Function" (Ptr.Ptr NonFunctionPointer_Function) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_NonFunctionPointer_Function")

instance HsBindgen.Runtime.HasCField.HasCField NonFunctionPointer_Function "un_NonFunctionPointer_Function" where

  type CFieldType NonFunctionPointer_Function "un_NonFunctionPointer_Function" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F1'

__C declaration:__ @f1@

__defined at:__ @types\/typedefs\/typedefs.h 11:16@

__exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1_Aux = F1_Aux
  { un_F1_Aux :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (Ptr.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_00d16e666202ed6c_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF1_Aux@
hs_bindgen_ddeb5206e8192425 ::
     Ptr.FunPtr F1_Aux
  -> F1_Aux
hs_bindgen_ddeb5206e8192425 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_ddeb5206e8192425_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr F1_Aux where

  toFunPtr = hs_bindgen_00d16e666202ed6c

instance HsBindgen.Runtime.FunPtr.FromFunPtr F1_Aux where

  fromFunPtr = hs_bindgen_ddeb5206e8192425

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F1_Aux) "un_F1_Aux")
         ) => GHC.Records.HasField "un_F1_Aux" (Ptr.Ptr F1_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F1_Aux")

instance HsBindgen.Runtime.HasCField.HasCField F1_Aux "un_F1_Aux" where

  type CFieldType F1_Aux "un_F1_Aux" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/typedefs.h 11:16@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype F1 = F1
  { un_F1 :: Ptr.FunPtr F1_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F1) "un_F1")
         ) => GHC.Records.HasField "un_F1" (Ptr.Ptr F1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F1")

instance HsBindgen.Runtime.HasCField.HasCField F1 "un_F1" where

  type CFieldType F1 "un_F1" = Ptr.FunPtr F1_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g1@

    __defined at:__ @types\/typedefs\/typedefs.h 13:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype G1 = G1
  { un_G1 :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_fa5806570b682579_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toG1@
hs_bindgen_fa5806570b682579 ::
     G1
  -> IO (Ptr.FunPtr G1)
hs_bindgen_fa5806570b682579 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_fa5806570b682579_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_8405c8e75aa78be5_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromG1@
hs_bindgen_8405c8e75aa78be5 ::
     Ptr.FunPtr G1
  -> G1
hs_bindgen_8405c8e75aa78be5 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_8405c8e75aa78be5_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr G1 where

  toFunPtr = hs_bindgen_fa5806570b682579

instance HsBindgen.Runtime.FunPtr.FromFunPtr G1 where

  fromFunPtr = hs_bindgen_8405c8e75aa78be5

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType G1) "un_G1")
         ) => GHC.Records.HasField "un_G1" (Ptr.Ptr G1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_G1")

instance HsBindgen.Runtime.HasCField.HasCField G1 "un_G1" where

  type CFieldType G1 "un_G1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @g2@

    __defined at:__ @types\/typedefs\/typedefs.h 14:14@

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

    __defined at:__ @types\/typedefs\/typedefs.h 16:14@

    __exported by:__ @types\/typedefs\/typedefs.h@
-}
newtype H1 = H1
  { un_H1 :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_ffae0d1234ed018f_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toH1@
hs_bindgen_ffae0d1234ed018f ::
     H1
  -> IO (Ptr.FunPtr H1)
hs_bindgen_ffae0d1234ed018f =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_ffae0d1234ed018f_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_1a33688324e1924f_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromH1@
hs_bindgen_1a33688324e1924f ::
     Ptr.FunPtr H1
  -> H1
hs_bindgen_1a33688324e1924f =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_1a33688324e1924f_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr H1 where

  toFunPtr = hs_bindgen_ffae0d1234ed018f

instance HsBindgen.Runtime.FunPtr.FromFunPtr H1 where

  fromFunPtr = hs_bindgen_1a33688324e1924f

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType H1) "un_H1")
         ) => GHC.Records.HasField "un_H1" (Ptr.Ptr H1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_H1")

instance HsBindgen.Runtime.HasCField.HasCField H1 "un_H1" where

  type CFieldType H1 "un_H1" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @h2@

    __defined at:__ @types\/typedefs\/typedefs.h 17:12@

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

    __defined at:__ @types\/typedefs\/typedefs.h 18:14@

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
