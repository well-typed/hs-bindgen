{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import qualified Prelude as P
import Data.Void (Void)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, IO, Int, Ord, Show, pure)

{-| Auxiliary type used by 'Fun_ptr'

__C declaration:__ @fun_ptr@

__defined at:__ @functions\/circular_dependency_fun.h 3:16@

__exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr_Aux = Fun_ptr_Aux
  { unwrapFun_ptr_Aux :: (Ptr.Ptr Forward_declaration) -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_5964bbadb359ee4a_base ::
     ((Ptr.Ptr Void) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO ()))

-- __unique:__ @toFun_ptr_Aux@
hs_bindgen_5964bbadb359ee4a ::
     Fun_ptr_Aux
  -> IO (Ptr.FunPtr Fun_ptr_Aux)
hs_bindgen_5964bbadb359ee4a =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_5964bbadb359ee4a_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_f8391e85af67fcb6_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> IO ())
  -> (Ptr.Ptr Void) -> IO ()

-- __unique:__ @fromFun_ptr_Aux@
hs_bindgen_f8391e85af67fcb6 ::
     Ptr.FunPtr Fun_ptr_Aux
  -> Fun_ptr_Aux
hs_bindgen_f8391e85af67fcb6 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_f8391e85af67fcb6_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr Fun_ptr_Aux where

  toFunPtr = hs_bindgen_5964bbadb359ee4a

instance HsBindgen.Runtime.FunPtr.FromFunPtr Fun_ptr_Aux where

  fromFunPtr = hs_bindgen_f8391e85af67fcb6

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Fun_ptr_Aux) "unwrapFun_ptr_Aux")
         ) => GHC.Records.HasField "unwrapFun_ptr_Aux" (Ptr.Ptr Fun_ptr_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFun_ptr_Aux")

instance HsBindgen.Runtime.HasCField.HasCField Fun_ptr_Aux "unwrapFun_ptr_Aux" where

  type CFieldType Fun_ptr_Aux "unwrapFun_ptr_Aux" =
    (Ptr.Ptr Forward_declaration) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fun_ptr@

    __defined at:__ @functions\/circular_dependency_fun.h 3:16@

    __exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr = Fun_ptr
  { unwrapFun_ptr :: Ptr.FunPtr Fun_ptr_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (HsBindgen.Runtime.Marshal.StaticSize, HsBindgen.Runtime.Marshal.ReadRaw, HsBindgen.Runtime.Marshal.WriteRaw, F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Fun_ptr) "unwrapFun_ptr")
         ) => GHC.Records.HasField "unwrapFun_ptr" (Ptr.Ptr Fun_ptr) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapFun_ptr")

instance HsBindgen.Runtime.HasCField.HasCField Fun_ptr "unwrapFun_ptr" where

  type CFieldType Fun_ptr "unwrapFun_ptr" =
    Ptr.FunPtr Fun_ptr_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct forward_declaration@

    __defined at:__ @functions\/circular_dependency_fun.h 5:8@

    __exported by:__ @functions\/circular_dependency_fun.h@
-}
data Forward_declaration = Forward_declaration
  { forward_declaration_f :: Fun_ptr
    {- ^ __C declaration:__ @f@

         __defined at:__ @functions\/circular_dependency_fun.h 6:11@

         __exported by:__ @functions\/circular_dependency_fun.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Forward_declaration where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Forward_declaration where

  readRaw =
    \ptr0 ->
          pure Forward_declaration
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"forward_declaration_f") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Forward_declaration where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Forward_declaration forward_declaration_f2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"forward_declaration_f") ptr0 forward_declaration_f2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Forward_declaration instance F.Storable Forward_declaration

instance HsBindgen.Runtime.HasCField.HasCField Forward_declaration "forward_declaration_f" where

  type CFieldType Forward_declaration "forward_declaration_f" =
    Fun_ptr

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Forward_declaration) "forward_declaration_f")
         ) => GHC.Records.HasField "forward_declaration_f" (Ptr.Ptr Forward_declaration) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"forward_declaration_f")

foreign import ccall safe "wrapper" hs_bindgen_fbe9c5dca66824d3_base ::
     ((Ptr.Ptr Void) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO ()))

-- __unique:__ @instance ToFunPtr ((Ptr.Ptr Forward_declaration) -> IO ())@
hs_bindgen_fbe9c5dca66824d3 ::
     ((Ptr.Ptr Forward_declaration) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Forward_declaration) -> IO ()))
hs_bindgen_fbe9c5dca66824d3 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_fbe9c5dca66824d3_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_b3640137a9cf92cc_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> IO ())
  -> (Ptr.Ptr Void) -> IO ()

-- __unique:__ @instance FromFunPtr ((Ptr.Ptr Forward_declaration) -> IO ())@
hs_bindgen_b3640137a9cf92cc ::
     Ptr.FunPtr ((Ptr.Ptr Forward_declaration) -> IO ())
  -> (Ptr.Ptr Forward_declaration) -> IO ()
hs_bindgen_b3640137a9cf92cc =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_b3640137a9cf92cc_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Forward_declaration) -> IO ()) where

  toFunPtr = hs_bindgen_fbe9c5dca66824d3

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Forward_declaration) -> IO ()) where

  fromFunPtr = hs_bindgen_b3640137a9cf92cc
