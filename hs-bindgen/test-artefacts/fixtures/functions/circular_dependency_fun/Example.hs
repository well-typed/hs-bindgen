{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Fun_ptr_Aux(..)
    , Example.Fun_ptr(..)
    , Example.Forward_declaration(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| Auxiliary type used by 'Fun_ptr'

    __C declaration:__ @fun_ptr@

    __defined at:__ @functions\/circular_dependency_fun.h 3:16@

    __exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr_Aux = Fun_ptr_Aux
  { unwrapFun_ptr_Aux :: BG.Ptr Forward_declaration -> IO ()
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toFun_ptr_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5964bbadb359ee4a_base ::
     (BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toFun_ptr_Aux@
hs_bindgen_5964bbadb359ee4a ::
     Fun_ptr_Aux
  -> IO (BG.FunPtr Fun_ptr_Aux)
hs_bindgen_5964bbadb359ee4a =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_5964bbadb359ee4a_base (BG.toFFIType fun0))

-- __unique:__ @fromFun_ptr_Aux@
foreign import ccall safe "dynamic" hs_bindgen_f8391e85af67fcb6_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromFun_ptr_Aux@
hs_bindgen_f8391e85af67fcb6 ::
     BG.FunPtr Fun_ptr_Aux
  -> Fun_ptr_Aux
hs_bindgen_f8391e85af67fcb6 =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_f8391e85af67fcb6_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr Fun_ptr_Aux where

  toFunPtr = hs_bindgen_5964bbadb359ee4a

instance BG.FromFunPtr Fun_ptr_Aux where

  fromFunPtr = hs_bindgen_f8391e85af67fcb6

instance ( ty ~ (BG.Ptr Forward_declaration -> IO ())
         ) => BG.CompatHasField.HasField "unwrapFun_ptr_Aux" Fun_ptr_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Fun_ptr_Aux {unwrapFun_ptr_Aux = y1}
      , BG.getField @"unwrapFun_ptr_Aux" x0
      )

instance ( ty ~ (BG.Ptr Forward_declaration -> IO ())
         ) => BG.HasField "unwrapFun_ptr_Aux" (BG.Ptr Fun_ptr_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFun_ptr_Aux")

instance HasCField.HasCField Fun_ptr_Aux "unwrapFun_ptr_Aux" where

  type CFieldType Fun_ptr_Aux "unwrapFun_ptr_Aux" =
    BG.Ptr Forward_declaration -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fun_ptr@

    __defined at:__ @functions\/circular_dependency_fun.h 3:16@

    __exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr = Fun_ptr
  { unwrapFun_ptr :: BG.FunPtr Fun_ptr_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Fun_ptr_Aux
         ) => BG.CompatHasField.HasField "unwrapFun_ptr" Fun_ptr ty where

  hasField =
    \x0 ->
      (\y1 ->
         Fun_ptr {unwrapFun_ptr = y1}, BG.getField @"unwrapFun_ptr" x0)

instance ( ty ~ BG.FunPtr Fun_ptr_Aux
         ) => BG.HasField "unwrapFun_ptr" (BG.Ptr Fun_ptr) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFun_ptr")

instance HasCField.HasCField Fun_ptr "unwrapFun_ptr" where

  type CFieldType Fun_ptr "unwrapFun_ptr" =
    BG.FunPtr Fun_ptr_Aux

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Forward_declaration where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Forward_declaration where

  readRaw =
    \ptr0 ->
          pure Forward_declaration
      <*> HasCField.readRaw (BG.Proxy @"forward_declaration_f") ptr0

instance Marshal.WriteRaw Forward_declaration where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Forward_declaration forward_declaration_f2 ->
            HasCField.writeRaw (BG.Proxy @"forward_declaration_f") ptr0 forward_declaration_f2

deriving via Marshal.EquivStorable Forward_declaration instance BG.Storable Forward_declaration

instance ( ty ~ Fun_ptr
         ) => BG.CompatHasField.HasField "forward_declaration_f" Forward_declaration ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Forward_declaration {forward_declaration_f = y1}
      , BG.getField @"forward_declaration_f" x0
      )

instance ( ty ~ Fun_ptr
         ) => BG.HasField "forward_declaration_f" (BG.Ptr Forward_declaration) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"forward_declaration_f")

instance HasCField.HasCField Forward_declaration "forward_declaration_f" where

  type CFieldType Forward_declaration "forward_declaration_f" =
    Fun_ptr

  offset# = \_ -> \_ -> 0
