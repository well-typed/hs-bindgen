{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.SizedByteArray
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, IO, Int, Show, pure)

{-| __C declaration:__ @int2int@

    __defined at:__ @manual\/function_pointers.h:19:13@

    __exported by:__ @manual\/function_pointers.h@
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Int2int) "un_Int2int")
         ) => GHC.Records.HasField "un_Int2int" (Ptr.Ptr Int2int) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Int2int")

instance HsBindgen.Runtime.HasCField.HasCField Int2int "un_Int2int" where

  type CFieldType Int2int "un_Int2int" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| A struct field pointing to a function like apply1_nopointer().

__C declaration:__ @Apply1Struct@

__defined at:__ @manual\/function_pointers.h:37:8@

__exported by:__ @manual\/function_pointers.h@
-}
data Apply1Struct = Apply1Struct
  { apply1Struct_apply1_nopointer_struct_field :: Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
    {- ^ __C declaration:__ @apply1_nopointer_struct_field@

         __defined at:__ @manual\/function_pointers.h:38:16@

         __exported by:__ @manual\/function_pointers.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Apply1Struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Apply1Struct
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"apply1Struct_apply1_nopointer_struct_field") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Apply1Struct apply1Struct_apply1_nopointer_struct_field2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"apply1Struct_apply1_nopointer_struct_field") ptr0 apply1Struct_apply1_nopointer_struct_field2

instance HsBindgen.Runtime.HasCField.HasCField Apply1Struct "apply1Struct_apply1_nopointer_struct_field" where

  type CFieldType Apply1Struct "apply1Struct_apply1_nopointer_struct_field" =
    Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Apply1Struct) "apply1Struct_apply1_nopointer_struct_field")
         ) => GHC.Records.HasField "apply1Struct_apply1_nopointer_struct_field" (Ptr.Ptr Apply1Struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"apply1Struct_apply1_nopointer_struct_field")

{-| A union field pointing to a function like apply1_nopointer().

__C declaration:__ @Apply1Union@

__defined at:__ @manual\/function_pointers.h:43:7@

__exported by:__ @manual\/function_pointers.h@
-}
newtype Apply1Union = Apply1Union
  { un_Apply1Union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance F.Storable Apply1Union

{-|

  __See:__ 'set_apply1Union_apply1_nopointer_union_field'

__C declaration:__ @apply1_nopointer_union_field@

__defined at:__ @manual\/function_pointers.h:44:16@

__exported by:__ @manual\/function_pointers.h@
-}
get_apply1Union_apply1_nopointer_union_field ::
     Apply1Union
  -> Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
get_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_apply1Union_apply1_nopointer_union_field'

-}
set_apply1Union_apply1_nopointer_union_field ::
     Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
  -> Apply1Union
set_apply1Union_apply1_nopointer_union_field =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Apply1Union "apply1Union_apply1_nopointer_union_field" where

  type CFieldType Apply1Union "apply1Union_apply1_nopointer_union_field" =
    Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Apply1Union) "apply1Union_apply1_nopointer_union_field")
         ) => GHC.Records.HasField "apply1Union_apply1_nopointer_union_field" (Ptr.Ptr Apply1Union) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"apply1Union_apply1_nopointer_union_field")

{-| __unique:__ @instance ToFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)@
-}
foreign import ccall safe "wrapper" hs_bindgen_fe02c1e534fc52ea ::
     ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt))

{-| __unique:__ @instance FromFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)@
-}
foreign import ccall safe "dynamic" hs_bindgen_fc27363846cb6139 ::
     Ptr.FunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt)
  -> (Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt) where

  toFunPtr = hs_bindgen_fe02c1e534fc52ea

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.FunPtr Int2int) -> FC.CInt -> IO FC.CInt) where

  fromFunPtr = hs_bindgen_fc27363846cb6139
