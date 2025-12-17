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
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

{-| Auxiliary type used by 'F1'

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h:7:16@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F1_Deref = F1_Deref
  { un_F1_Deref :: FC.CInt -> FC.CInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toF1_Deref@
foreign import ccall safe "wrapper" hs_bindgen_fa93becf814ab275 ::
     F1_Deref
  -> IO (Ptr.FunPtr F1_Deref)

-- __unique:__ @fromF1_Deref@
foreign import ccall safe "dynamic" hs_bindgen_5a971083f7b8024b ::
     Ptr.FunPtr F1_Deref
  -> F1_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr F1_Deref where

  toFunPtr = hs_bindgen_fa93becf814ab275

instance HsBindgen.Runtime.FunPtr.FromFunPtr F1_Deref where

  fromFunPtr = hs_bindgen_5a971083f7b8024b

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F1_Deref) "un_F1_Deref")
         ) => GHC.Records.HasField "un_F1_Deref" (Ptr.Ptr F1_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F1_Deref")

instance HsBindgen.Runtime.HasCField.HasCField F1_Deref "un_F1_Deref" where

  type CFieldType F1_Deref "un_F1_Deref" =
    FC.CInt -> FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f1@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h:7:16@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
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

{-| Auxiliary type used by 'F2'

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h:10:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F2_Deref = F2_Deref
  { un_F2_Deref :: FC.CInt -> FC.CInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toF2_Deref@
foreign import ccall safe "wrapper" hs_bindgen_8741614e72e842ed ::
     F2_Deref
  -> IO (Ptr.FunPtr F2_Deref)

-- __unique:__ @fromF2_Deref@
foreign import ccall safe "dynamic" hs_bindgen_4b21dd5f07b53aa8 ::
     Ptr.FunPtr F2_Deref
  -> F2_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr F2_Deref where

  toFunPtr = hs_bindgen_8741614e72e842ed

instance HsBindgen.Runtime.FunPtr.FromFunPtr F2_Deref where

  fromFunPtr = hs_bindgen_4b21dd5f07b53aa8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F2_Deref) "un_F2_Deref")
         ) => GHC.Records.HasField "un_F2_Deref" (Ptr.Ptr F2_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F2_Deref")

instance HsBindgen.Runtime.HasCField.HasCField F2_Deref "un_F2_Deref" where

  type CFieldType F2_Deref "un_F2_Deref" =
    FC.CInt -> FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f2@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h:10:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F2 = F2
  { un_F2 :: Ptr.Ptr (Ptr.FunPtr F2_Deref)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F2) "un_F2")
         ) => GHC.Records.HasField "un_F2" (Ptr.Ptr F2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F2")

instance HsBindgen.Runtime.HasCField.HasCField F2 "un_F2" where

  type CFieldType F2 "un_F2" =
    Ptr.Ptr (Ptr.FunPtr F2_Deref)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F3'

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h:13:18@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F3_Deref = F3_Deref
  { un_F3_Deref :: FC.CInt -> FC.CInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toF3_Deref@
foreign import ccall safe "wrapper" hs_bindgen_04d39f91c5c62278 ::
     F3_Deref
  -> IO (Ptr.FunPtr F3_Deref)

-- __unique:__ @fromF3_Deref@
foreign import ccall safe "dynamic" hs_bindgen_aee01b3475cc645c ::
     Ptr.FunPtr F3_Deref
  -> F3_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr F3_Deref where

  toFunPtr = hs_bindgen_04d39f91c5c62278

instance HsBindgen.Runtime.FunPtr.FromFunPtr F3_Deref where

  fromFunPtr = hs_bindgen_aee01b3475cc645c

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F3_Deref) "un_F3_Deref")
         ) => GHC.Records.HasField "un_F3_Deref" (Ptr.Ptr F3_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F3_Deref")

instance HsBindgen.Runtime.HasCField.HasCField F3_Deref "un_F3_Deref" where

  type CFieldType F3_Deref "un_F3_Deref" =
    FC.CInt -> FC.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f3@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h:13:18@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F3 = F3
  { un_F3 :: Ptr.Ptr (Ptr.Ptr (Ptr.FunPtr F3_Deref))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F3) "un_F3")
         ) => GHC.Records.HasField "un_F3" (Ptr.Ptr F3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F3")

instance HsBindgen.Runtime.HasCField.HasCField F3 "un_F3" where

  type CFieldType F3 "un_F3" =
    Ptr.Ptr (Ptr.Ptr (Ptr.FunPtr F3_Deref))

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F4'

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h:16:16@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F4_Deref = F4_Deref
  { un_F4_Deref :: IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toF4_Deref@
foreign import ccall safe "wrapper" hs_bindgen_b1dc6cdfd1046f6c ::
     F4_Deref
  -> IO (Ptr.FunPtr F4_Deref)

-- __unique:__ @fromF4_Deref@
foreign import ccall safe "dynamic" hs_bindgen_bf80b45e357edad9 ::
     Ptr.FunPtr F4_Deref
  -> F4_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr F4_Deref where

  toFunPtr = hs_bindgen_b1dc6cdfd1046f6c

instance HsBindgen.Runtime.FunPtr.FromFunPtr F4_Deref where

  fromFunPtr = hs_bindgen_bf80b45e357edad9

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F4_Deref) "un_F4_Deref")
         ) => GHC.Records.HasField "un_F4_Deref" (Ptr.Ptr F4_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F4_Deref")

instance HsBindgen.Runtime.HasCField.HasCField F4_Deref "un_F4_Deref" where

  type CFieldType F4_Deref "un_F4_Deref" = IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f4@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h:16:16@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F4 = F4
  { un_F4 :: Ptr.Ptr (Ptr.FunPtr F4_Deref)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F4) "un_F4")
         ) => GHC.Records.HasField "un_F4" (Ptr.Ptr F4) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F4")

instance HsBindgen.Runtime.HasCField.HasCField F4 "un_F4" where

  type CFieldType F4 "un_F4" =
    Ptr.Ptr (Ptr.FunPtr F4_Deref)

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F5'

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h:19:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F5_Deref = F5_Deref
  { un_F5_Deref :: IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toF5_Deref@
foreign import ccall safe "wrapper" hs_bindgen_82c23fe0d26463bd ::
     F5_Deref
  -> IO (Ptr.FunPtr F5_Deref)

-- __unique:__ @fromF5_Deref@
foreign import ccall safe "dynamic" hs_bindgen_9e30690aa537cac5 ::
     Ptr.FunPtr F5_Deref
  -> F5_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr F5_Deref where

  toFunPtr = hs_bindgen_82c23fe0d26463bd

instance HsBindgen.Runtime.FunPtr.FromFunPtr F5_Deref where

  fromFunPtr = hs_bindgen_9e30690aa537cac5

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F5_Deref) "un_F5_Deref")
         ) => GHC.Records.HasField "un_F5_Deref" (Ptr.Ptr F5_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F5_Deref")

instance HsBindgen.Runtime.HasCField.HasCField F5_Deref "un_F5_Deref" where

  type CFieldType F5_Deref "un_F5_Deref" = IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f5@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h:19:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F5 = F5
  { un_F5 :: Ptr.Ptr (Ptr.FunPtr F5_Deref)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F5) "un_F5")
         ) => GHC.Records.HasField "un_F5" (Ptr.Ptr F5) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F5")

instance HsBindgen.Runtime.HasCField.HasCField F5 "un_F5" where

  type CFieldType F5 "un_F5" =
    Ptr.Ptr (Ptr.FunPtr F5_Deref)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyInt@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h:22:13@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype MyInt = MyInt
  { un_MyInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyInt) "un_MyInt")
         ) => GHC.Records.HasField "un_MyInt" (Ptr.Ptr MyInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyInt")

instance HsBindgen.Runtime.HasCField.HasCField MyInt "un_MyInt" where

  type CFieldType MyInt "un_MyInt" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'F6'

__defined at:__ @types\/typedefs\/multi_level_function_pointer.h:23:17@

__exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F6_Deref = F6_Deref
  { un_F6_Deref :: MyInt -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

-- __unique:__ @toF6_Deref@
foreign import ccall safe "wrapper" hs_bindgen_8c739a057f4c416d ::
     F6_Deref
  -> IO (Ptr.FunPtr F6_Deref)

-- __unique:__ @fromF6_Deref@
foreign import ccall safe "dynamic" hs_bindgen_95e55fee8d4c2b5b ::
     Ptr.FunPtr F6_Deref
  -> F6_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr F6_Deref where

  toFunPtr = hs_bindgen_8c739a057f4c416d

instance HsBindgen.Runtime.FunPtr.FromFunPtr F6_Deref where

  fromFunPtr = hs_bindgen_95e55fee8d4c2b5b

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F6_Deref) "un_F6_Deref")
         ) => GHC.Records.HasField "un_F6_Deref" (Ptr.Ptr F6_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F6_Deref")

instance HsBindgen.Runtime.HasCField.HasCField F6_Deref "un_F6_Deref" where

  type CFieldType F6_Deref "un_F6_Deref" =
    MyInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f6@

    __defined at:__ @types\/typedefs\/multi_level_function_pointer.h:23:17@

    __exported by:__ @types\/typedefs\/multi_level_function_pointer.h@
-}
newtype F6 = F6
  { un_F6 :: Ptr.Ptr (Ptr.FunPtr F6_Deref)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType F6) "un_F6")
         ) => GHC.Records.HasField "un_F6" (Ptr.Ptr F6) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_F6")

instance HsBindgen.Runtime.HasCField.HasCField F6 "un_F6" where

  type CFieldType F6 "un_F6" =
    Ptr.Ptr (Ptr.FunPtr F6_Deref)

  offset# = \_ -> \_ -> 0
