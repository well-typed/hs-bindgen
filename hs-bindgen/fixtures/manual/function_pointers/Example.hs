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

{-| __C declaration:__ @int2int@

    __defined at:__ @manual\/function_pointers.h 19:13@

    __exported by:__ @manual\/function_pointers.h@
-}
newtype Int2int = Int2int
  { unwrapInt2int :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_a6c7dd49f5b9d470_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toInt2int@
hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (RIP.FunPtr Int2int)
hs_bindgen_a6c7dd49f5b9d470 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_a6c7dd49f5b9d470_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_65378a8a3cf640ad_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromInt2int@
hs_bindgen_65378a8a3cf640ad ::
     RIP.FunPtr Int2int
  -> Int2int
hs_bindgen_65378a8a3cf640ad =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_65378a8a3cf640ad_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr Int2int where

  toFunPtr = hs_bindgen_a6c7dd49f5b9d470

instance RIP.FromFunPtr Int2int where

  fromFunPtr = hs_bindgen_65378a8a3cf640ad

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapInt2int" (RIP.Ptr Int2int) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapInt2int")

instance HasCField.HasCField Int2int "unwrapInt2int" where

  type CFieldType Int2int "unwrapInt2int" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| A struct field pointing to a function like apply1_nopointer().

__C declaration:__ @struct Apply1Struct@

__defined at:__ @manual\/function_pointers.h 37:8@

__exported by:__ @manual\/function_pointers.h@
-}
data Apply1Struct = Apply1Struct
  { apply1Struct_apply1_nopointer_struct_field :: RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)
    {- ^ __C declaration:__ @apply1_nopointer_struct_field@

         __defined at:__ @manual\/function_pointers.h 38:16@

         __exported by:__ @manual\/function_pointers.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Apply1Struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Apply1Struct where

  readRaw =
    \ptr0 ->
          pure Apply1Struct
      <*> HasCField.readRaw (RIP.Proxy @"apply1Struct_apply1_nopointer_struct_field") ptr0

instance Marshal.WriteRaw Apply1Struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Apply1Struct apply1Struct_apply1_nopointer_struct_field2 ->
            HasCField.writeRaw (RIP.Proxy @"apply1Struct_apply1_nopointer_struct_field") ptr0 apply1Struct_apply1_nopointer_struct_field2

deriving via Marshal.EquivStorable Apply1Struct instance RIP.Storable Apply1Struct

instance HasCField.HasCField Apply1Struct "apply1Struct_apply1_nopointer_struct_field" where

  type CFieldType Apply1Struct "apply1Struct_apply1_nopointer_struct_field" =
    RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt))
         ) => RIP.HasField "apply1Struct_apply1_nopointer_struct_field" (RIP.Ptr Apply1Struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"apply1Struct_apply1_nopointer_struct_field")

{-| A union field pointing to a function like apply1_nopointer().

__C declaration:__ @union Apply1Union@

__defined at:__ @manual\/function_pointers.h 43:7@

__exported by:__ @manual\/function_pointers.h@
-}
newtype Apply1Union = Apply1Union
  { unwrapApply1Union :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.StaticSize Apply1Union

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.ReadRaw Apply1Union

deriving via (RIP.SizedByteArray 8) 8 instance Marshal.WriteRaw Apply1Union

deriving via Marshal.EquivStorable Apply1Union instance RIP.Storable Apply1Union

{-|

  __See:__ 'set_apply1Union_apply1_nopointer_union_field'

__C declaration:__ @apply1_nopointer_union_field@

__defined at:__ @manual\/function_pointers.h 44:16@

__exported by:__ @manual\/function_pointers.h@
-}
get_apply1Union_apply1_nopointer_union_field ::
     Apply1Union
  -> RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)
get_apply1Union_apply1_nopointer_union_field =
  RIP.getUnionPayload

{-|

  __See:__ 'get_apply1Union_apply1_nopointer_union_field'

-}
set_apply1Union_apply1_nopointer_union_field ::
     RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)
  -> Apply1Union
set_apply1Union_apply1_nopointer_union_field =
  RIP.setUnionPayload

instance HasCField.HasCField Apply1Union "apply1Union_apply1_nopointer_union_field" where

  type CFieldType Apply1Union "apply1Union_apply1_nopointer_union_field" =
    RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt))
         ) => RIP.HasField "apply1Union_apply1_nopointer_union_field" (RIP.Ptr Apply1Union) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"apply1Union_apply1_nopointer_union_field")

foreign import ccall safe "wrapper" hs_bindgen_61d7176781c1adfc_base ::
     ((RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr ((RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @instance ToFunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)@
hs_bindgen_61d7176781c1adfc ::
     ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)
  -> IO (RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt))
hs_bindgen_61d7176781c1adfc =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_61d7176781c1adfc_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_767f8bdd743f5066_base ::
     RIP.FunPtr ((RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO RIP.Int32)
  -> (RIP.FunPtr RIP.Void) -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @instance FromFunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)@
hs_bindgen_767f8bdd743f5066 ::
     RIP.FunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt)
  -> (RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt
hs_bindgen_767f8bdd743f5066 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_767f8bdd743f5066_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt) where

  toFunPtr = hs_bindgen_61d7176781c1adfc

instance RIP.FromFunPtr ((RIP.FunPtr Int2int) -> RIP.CInt -> IO RIP.CInt) where

  fromFunPtr = hs_bindgen_767f8bdd743f5066
