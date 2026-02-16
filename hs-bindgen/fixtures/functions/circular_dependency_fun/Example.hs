{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Auxiliary type used by 'Fun_ptr'

__C declaration:__ @fun_ptr@

__defined at:__ @functions\/circular_dependency_fun.h 3:16@

__exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr_Aux = Fun_ptr_Aux
  { unwrapFun_ptr_Aux :: (RIP.Ptr Forward_declaration) -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_5964bbadb359ee4a_base ::
     ((RIP.Ptr RIP.Void) -> IO ())
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ()))

-- __unique:__ @toFun_ptr_Aux@
hs_bindgen_5964bbadb359ee4a ::
     Fun_ptr_Aux
  -> IO (RIP.FunPtr Fun_ptr_Aux)
hs_bindgen_5964bbadb359ee4a =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_5964bbadb359ee4a_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_f8391e85af67fcb6_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ())
  -> (RIP.Ptr RIP.Void) -> IO ()

-- __unique:__ @fromFun_ptr_Aux@
hs_bindgen_f8391e85af67fcb6 ::
     RIP.FunPtr Fun_ptr_Aux
  -> Fun_ptr_Aux
hs_bindgen_f8391e85af67fcb6 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_f8391e85af67fcb6_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Fun_ptr_Aux where

  toFunPtr = hs_bindgen_5964bbadb359ee4a

instance RIP.FromFunPtr Fun_ptr_Aux where

  fromFunPtr = hs_bindgen_f8391e85af67fcb6

instance ( ((~) ty) ((RIP.Ptr Forward_declaration) -> IO ())
         ) => RIP.HasField "unwrapFun_ptr_Aux" (RIP.Ptr Fun_ptr_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFun_ptr_Aux")

instance HasCField.HasCField Fun_ptr_Aux "unwrapFun_ptr_Aux" where

  type CFieldType Fun_ptr_Aux "unwrapFun_ptr_Aux" =
    (RIP.Ptr Forward_declaration) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fun_ptr@

    __defined at:__ @functions\/circular_dependency_fun.h 3:16@

    __exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr = Fun_ptr
  { unwrapFun_ptr :: RIP.FunPtr Fun_ptr_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr Fun_ptr_Aux)
         ) => RIP.HasField "unwrapFun_ptr" (RIP.Ptr Fun_ptr) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFun_ptr")

instance HasCField.HasCField Fun_ptr "unwrapFun_ptr" where

  type CFieldType Fun_ptr "unwrapFun_ptr" =
    RIP.FunPtr Fun_ptr_Aux

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Forward_declaration where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Forward_declaration where

  readRaw =
    \ptr0 ->
          pure Forward_declaration
      <*> HasCField.readRaw (RIP.Proxy @"forward_declaration_f") ptr0

instance Marshal.WriteRaw Forward_declaration where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Forward_declaration forward_declaration_f2 ->
            HasCField.writeRaw (RIP.Proxy @"forward_declaration_f") ptr0 forward_declaration_f2

deriving via Marshal.EquivStorable Forward_declaration instance RIP.Storable Forward_declaration

instance HasCField.HasCField Forward_declaration "forward_declaration_f" where

  type CFieldType Forward_declaration "forward_declaration_f" =
    Fun_ptr

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Fun_ptr
         ) => RIP.HasField "forward_declaration_f" (RIP.Ptr Forward_declaration) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"forward_declaration_f")
