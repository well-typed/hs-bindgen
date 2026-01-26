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
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified Prelude as P
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

{-| Auxiliary type used by 'F1'

__C declaration:__ @f1@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 7:16@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F1_Aux = F1_Aux
  { un_F1_Aux :: FC.CInt -> FC.CInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (Ptr.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_00d16e666202ed6c_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ())
  -> GHC.Int.Int32 -> GHC.Int.Int32 -> IO ()

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

  type CFieldType F1_Aux "un_F1_Aux" =
    FC.CInt -> FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 7:16@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
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

{-| Auxiliary type used by 'F2'

__C declaration:__ @f2@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 10:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F2_Aux = F2_Aux
  { un_F2_Aux :: FC.CInt -> FC.CInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_c39d7524b75b54e8_base ::
     (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ()))

-- __unique:__ @toF2_Aux@
hs_bindgen_c39d7524b75b54e8 ::
     F2_Aux
  -> IO (Ptr.FunPtr F2_Aux)
hs_bindgen_c39d7524b75b54e8 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_c39d7524b75b54e8_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_e15bcd26f1ed1df7_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ())
  -> GHC.Int.Int32 -> GHC.Int.Int32 -> IO ()

-- __unique:__ @fromF2_Aux@
hs_bindgen_e15bcd26f1ed1df7 ::
     Ptr.FunPtr F2_Aux
  -> F2_Aux
hs_bindgen_e15bcd26f1ed1df7 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_e15bcd26f1ed1df7_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr F2_Aux where

  toFunPtr = hs_bindgen_c39d7524b75b54e8

instance HsBindgen.Runtime.FunPtr.FromFunPtr F2_Aux where

  fromFunPtr = hs_bindgen_e15bcd26f1ed1df7

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F2_Aux) "un_F2_Aux")
         ) => GHC.Records.HasField "un_F2_Aux" (Ptr.Ptr F2_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F2_Aux")

instance HsBindgen.Runtime.HasCField.HasCField F2_Aux "un_F2_Aux" where

  type CFieldType F2_Aux "un_F2_Aux" =
    FC.CInt -> FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f2@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 10:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F2 = F2
  { un_F2 :: Ptr.Ptr (Ptr.FunPtr F2_Aux)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F2) "un_F2")
         ) => GHC.Records.HasField "un_F2" (Ptr.Ptr F2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F2")

instance HsBindgen.Runtime.HasCField.HasCField F2 "un_F2" where

  type CFieldType F2 "un_F2" =
    Ptr.Ptr (Ptr.FunPtr F2_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F3'

__C declaration:__ @f3@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 13:18@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F3_Aux = F3_Aux
  { un_F3_Aux :: FC.CInt -> FC.CInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_4a960721e7d1dcef_base ::
     (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ()))

-- __unique:__ @toF3_Aux@
hs_bindgen_4a960721e7d1dcef ::
     F3_Aux
  -> IO (Ptr.FunPtr F3_Aux)
hs_bindgen_4a960721e7d1dcef =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_4a960721e7d1dcef_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_66460422a7197535_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> GHC.Int.Int32 -> IO ())
  -> GHC.Int.Int32 -> GHC.Int.Int32 -> IO ()

-- __unique:__ @fromF3_Aux@
hs_bindgen_66460422a7197535 ::
     Ptr.FunPtr F3_Aux
  -> F3_Aux
hs_bindgen_66460422a7197535 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_66460422a7197535_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr F3_Aux where

  toFunPtr = hs_bindgen_4a960721e7d1dcef

instance HsBindgen.Runtime.FunPtr.FromFunPtr F3_Aux where

  fromFunPtr = hs_bindgen_66460422a7197535

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F3_Aux) "un_F3_Aux")
         ) => GHC.Records.HasField "un_F3_Aux" (Ptr.Ptr F3_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F3_Aux")

instance HsBindgen.Runtime.HasCField.HasCField F3_Aux "un_F3_Aux" where

  type CFieldType F3_Aux "un_F3_Aux" =
    FC.CInt -> FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f3@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 13:18@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F3 = F3
  { un_F3 :: Ptr.Ptr (Ptr.Ptr (Ptr.FunPtr F3_Aux))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F3) "un_F3")
         ) => GHC.Records.HasField "un_F3" (Ptr.Ptr F3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F3")

instance HsBindgen.Runtime.HasCField.HasCField F3 "un_F3" where

  type CFieldType F3 "un_F3" =
    Ptr.Ptr (Ptr.Ptr (Ptr.FunPtr F3_Aux))

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F4'

__C declaration:__ @f4@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 16:16@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F4_Aux = F4_Aux
  { un_F4_Aux :: IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_83bcff023b3bc648_base ::
     IO GHC.Int.Int32
  -> IO (Ptr.FunPtr (IO GHC.Int.Int32))

-- __unique:__ @toF4_Aux@
hs_bindgen_83bcff023b3bc648 ::
     F4_Aux
  -> IO (Ptr.FunPtr F4_Aux)
hs_bindgen_83bcff023b3bc648 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_83bcff023b3bc648_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_40f9a8d432b9eb97_base ::
     Ptr.FunPtr (IO GHC.Int.Int32)
  -> IO GHC.Int.Int32

-- __unique:__ @fromF4_Aux@
hs_bindgen_40f9a8d432b9eb97 ::
     Ptr.FunPtr F4_Aux
  -> F4_Aux
hs_bindgen_40f9a8d432b9eb97 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_40f9a8d432b9eb97_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr F4_Aux where

  toFunPtr = hs_bindgen_83bcff023b3bc648

instance HsBindgen.Runtime.FunPtr.FromFunPtr F4_Aux where

  fromFunPtr = hs_bindgen_40f9a8d432b9eb97

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F4_Aux) "un_F4_Aux")
         ) => GHC.Records.HasField "un_F4_Aux" (Ptr.Ptr F4_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F4_Aux")

instance HsBindgen.Runtime.HasCField.HasCField F4_Aux "un_F4_Aux" where

  type CFieldType F4_Aux "un_F4_Aux" = IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f4@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 16:16@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F4 = F4
  { un_F4 :: Ptr.Ptr (Ptr.FunPtr F4_Aux)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F4) "un_F4")
         ) => GHC.Records.HasField "un_F4" (Ptr.Ptr F4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F4")

instance HsBindgen.Runtime.HasCField.HasCField F4 "un_F4" where

  type CFieldType F4 "un_F4" =
    Ptr.Ptr (Ptr.FunPtr F4_Aux)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F5'

__C declaration:__ @f5@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 19:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F5_Aux = F5_Aux
  { un_F5_Aux :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_6891cbd81d6f42b9_base ::
     IO ()
  -> IO (Ptr.FunPtr (IO ()))

-- __unique:__ @toF5_Aux@
hs_bindgen_6891cbd81d6f42b9 ::
     F5_Aux
  -> IO (Ptr.FunPtr F5_Aux)
hs_bindgen_6891cbd81d6f42b9 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_6891cbd81d6f42b9_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_586f6635c057975f_base ::
     Ptr.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF5_Aux@
hs_bindgen_586f6635c057975f ::
     Ptr.FunPtr F5_Aux
  -> F5_Aux
hs_bindgen_586f6635c057975f =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_586f6635c057975f_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr F5_Aux where

  toFunPtr = hs_bindgen_6891cbd81d6f42b9

instance HsBindgen.Runtime.FunPtr.FromFunPtr F5_Aux where

  fromFunPtr = hs_bindgen_586f6635c057975f

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F5_Aux) "un_F5_Aux")
         ) => GHC.Records.HasField "un_F5_Aux" (Ptr.Ptr F5_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F5_Aux")

instance HsBindgen.Runtime.HasCField.HasCField F5_Aux "un_F5_Aux" where

  type CFieldType F5_Aux "un_F5_Aux" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f5@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 19:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F5 = F5
  { un_F5 :: Ptr.Ptr (Ptr.FunPtr F5_Aux)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F5) "un_F5")
         ) => GHC.Records.HasField "un_F5" (Ptr.Ptr F5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F5")

instance HsBindgen.Runtime.HasCField.HasCField F5 "un_F5" where

  type CFieldType F5 "un_F5" =
    Ptr.Ptr (Ptr.FunPtr F5_Aux)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyInt@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 22:13@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype MyInt = MyInt
  { un_MyInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, HsBindgen.Runtime.Bitfield.Bitfield, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyInt) "un_MyInt")
         ) => GHC.Records.HasField "un_MyInt" (Ptr.Ptr MyInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyInt")

instance HsBindgen.Runtime.HasCField.HasCField MyInt "un_MyInt" where

  type CFieldType MyInt "un_MyInt" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F6'

__C declaration:__ @f6@

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h 23:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F6_Aux = F6_Aux
  { un_F6_Aux :: MyInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_c1baf73f98614f45_base ::
     (GHC.Int.Int32 -> IO ())
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO ()))

-- __unique:__ @toF6_Aux@
hs_bindgen_c1baf73f98614f45 ::
     F6_Aux
  -> IO (Ptr.FunPtr F6_Aux)
hs_bindgen_c1baf73f98614f45 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_c1baf73f98614f45_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_a887947b26e58f0c_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO ())
  -> GHC.Int.Int32 -> IO ()

-- __unique:__ @fromF6_Aux@
hs_bindgen_a887947b26e58f0c ::
     Ptr.FunPtr F6_Aux
  -> F6_Aux
hs_bindgen_a887947b26e58f0c =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_a887947b26e58f0c_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr F6_Aux where

  toFunPtr = hs_bindgen_c1baf73f98614f45

instance HsBindgen.Runtime.FunPtr.FromFunPtr F6_Aux where

  fromFunPtr = hs_bindgen_a887947b26e58f0c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F6_Aux) "un_F6_Aux")
         ) => GHC.Records.HasField "un_F6_Aux" (Ptr.Ptr F6_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F6_Aux")

instance HsBindgen.Runtime.HasCField.HasCField F6_Aux "un_F6_Aux" where

  type CFieldType F6_Aux "un_F6_Aux" = MyInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f6@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h 23:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F6 = F6
  { un_F6 :: Ptr.Ptr (Ptr.FunPtr F6_Aux)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F6) "un_F6")
         ) => GHC.Records.HasField "un_F6" (Ptr.Ptr F6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F6")

instance HsBindgen.Runtime.HasCField.HasCField F6 "un_F6" where

  type CFieldType F6 "un_F6" =
    Ptr.Ptr (Ptr.FunPtr F6_Aux)

  offset# = \_ -> \_ -> 0
