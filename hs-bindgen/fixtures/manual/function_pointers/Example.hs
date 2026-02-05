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

import qualified Data.Array.Byte
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.ByteArray
import qualified HsBindgen.Runtime.Internal.FunPtr
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Internal.SizedByteArray
import qualified HsBindgen.Runtime.Marshal
import qualified Prelude as P
import Data.Void (Void)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, IO, Int, Show, pure)

{-| __C declaration:__ @int2int@

    __defined at:__ @manual\/function_pointers.h 19:13@

    __exported by:__ @manual\/function_pointers.h@
-}
newtype Int2int = Int2int
  { unwrapInt2int :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_a6c7dd49f5b9d470_base ::
     (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32))

-- __unique:__ @toInt2int@
hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (Ptr.FunPtr Int2int)
hs_bindgen_a6c7dd49f5b9d470 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_a6c7dd49f5b9d470_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_65378a8a3cf640ad_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> IO GHC.Int.Int32

-- __unique:__ @fromInt2int@
hs_bindgen_65378a8a3cf640ad ::
     Ptr.FunPtr Int2int
  -> Int2int
hs_bindgen_65378a8a3cf640ad =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_65378a8a3cf640ad_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr Int2int where

  toFunPtr = hs_bindgen_a6c7dd49f5b9d470

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr Int2int where

  fromFunPtr = hs_bindgen_65378a8a3cf640ad

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int2int) "unwrapInt2int")
         ) => GHC.Records.HasField "unwrapInt2int" (Ptr.Ptr Int2int) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapInt2int")

instance HsBindgen.Runtime.HasCField.HasCField Int2int "unwrapInt2int" where

  type CFieldType Int2int "unwrapInt2int" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| A struct field pointing to a function like apply1_nopointer().

__C declaration:__ @struct Apply1Struct@

__defined at:__ @manual\/function_pointers.h 37:8@

__exported by:__ @manual\/function_pointers.h@
-}
data Apply1Struct = Apply1Struct
  { apply1Struct_apply1_nopointer_struct_field :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
    {- ^ __C declaration:__ @apply1_nopointer_struct_field@

         __defined at:__ @manual\/function_pointers.h 38:16@

         __exported by:__ @manual\/function_pointers.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Apply1Struct where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Apply1Struct where

  readRaw =
    \ptr0 ->
          pure Apply1Struct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"apply1Struct_apply1_nopointer_struct_field") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Apply1Struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Apply1Struct apply1Struct_apply1_nopointer_struct_field2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"apply1Struct_apply1_nopointer_struct_field") ptr0 apply1Struct_apply1_nopointer_struct_field2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Apply1Struct instance F.Storable Apply1Struct

instance HsBindgen.Runtime.HasCField.HasCField Apply1Struct "apply1Struct_apply1_nopointer_struct_field" where

  type CFieldType Apply1Struct "apply1Struct_apply1_nopointer_struct_field" =
    Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Apply1Struct) "apply1Struct_apply1_nopointer_struct_field")
         ) => GHC.Records.HasField "apply1Struct_apply1_nopointer_struct_field" (Ptr.Ptr Apply1Struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"apply1Struct_apply1_nopointer_struct_field")

{-| A union field pointing to a function like apply1_nopointer().

__C declaration:__ @union Apply1Union@

__defined at:__ @manual\/function_pointers.h 43:7@

__exported by:__ @manual\/function_pointers.h@
-}
newtype Apply1Union = Apply1Union
  { unwrapApply1Union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.StaticSize Apply1Union

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.ReadRaw Apply1Union

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 8 instance HsBindgen.Runtime.Marshal.WriteRaw Apply1Union

deriving via HsBindgen.Runtime.Marshal.EquivStorable Apply1Union instance F.Storable Apply1Union

{-|

  __See:__ 'set_apply1Union_apply1_nopointer_union_field'

__C declaration:__ @apply1_nopointer_union_field@

__defined at:__ @manual\/function_pointers.h 44:16@

__exported by:__ @manual\/function_pointers.h@
-}
get_apply1Union_apply1_nopointer_union_field ::
     Apply1Union
  -> Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
get_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_apply1Union_apply1_nopointer_union_field'

-}
set_apply1Union_apply1_nopointer_union_field ::
     Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
  -> Apply1Union
set_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Apply1Union "apply1Union_apply1_nopointer_union_field" where

  type CFieldType Apply1Union "apply1Union_apply1_nopointer_union_field" =
    Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Apply1Union) "apply1Union_apply1_nopointer_union_field")
         ) => GHC.Records.HasField "apply1Union_apply1_nopointer_union_field" (Ptr.Ptr Apply1Union) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"apply1Union_apply1_nopointer_union_field")

foreign import ccall safe "wrapper" hs_bindgen_fe02c1e534fc52ea_base ::
     ((Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr ((Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO GHC.Int.Int32))

-- __unique:__ @instance ToFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)@
hs_bindgen_fe02c1e534fc52ea ::
     ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))
hs_bindgen_fe02c1e534fc52ea =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_fe02c1e534fc52ea_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_fc27363846cb6139_base ::
     Ptr.FunPtr ((Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> (Ptr.FunPtr Void) -> GHC.Int.Int32 -> IO GHC.Int.Int32

-- __unique:__ @instance FromFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)@
hs_bindgen_fc27363846cb6139 ::
     Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
  -> (Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt
hs_bindgen_fc27363846cb6139 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_fc27363846cb6139_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt) where

  toFunPtr = hs_bindgen_fe02c1e534fc52ea

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt) where

  fromFunPtr = hs_bindgen_fc27363846cb6139
