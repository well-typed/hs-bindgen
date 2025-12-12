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

import qualified Data.Proxy
import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, IO, Int, Ord, Show, pure)

{-| Auxiliary type used by 'Fun_ptr'

__defined at:__ @functions\/circular_dependency_fun.h:3:16@

__exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr_Deref = Fun_ptr_Deref
  { un_Fun_ptr_Deref :: (Ptr.Ptr Forward_declaration) -> IO ()
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" toFun_ptr_Deref ::
     Fun_ptr_Deref
  -> IO (Ptr.FunPtr Fun_ptr_Deref)

foreign import ccall safe "dynamic" fromFun_ptr_Deref ::
     Ptr.FunPtr Fun_ptr_Deref
  -> Fun_ptr_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Fun_ptr_Deref where

  toFunPtr = toFun_ptr_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Fun_ptr_Deref where

  fromFunPtr = fromFun_ptr_Deref

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Fun_ptr_Deref) "un_Fun_ptr_Deref")
         ) => GHC.Records.HasField "un_Fun_ptr_Deref" (Ptr.Ptr Fun_ptr_Deref) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Fun_ptr_Deref")

instance HsBindgen.Runtime.HasCField.HasCField Fun_ptr_Deref "un_Fun_ptr_Deref" where

  type CFieldType Fun_ptr_Deref "un_Fun_ptr_Deref" =
    (Ptr.Ptr Forward_declaration) -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fun_ptr@

    __defined at:__ @functions\/circular_dependency_fun.h:3:16@

    __exported by:__ @functions\/circular_dependency_fun.h@
-}
newtype Fun_ptr = Fun_ptr
  { un_Fun_ptr :: Ptr.FunPtr Fun_ptr_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Fun_ptr) "un_Fun_ptr")
         ) => GHC.Records.HasField "un_Fun_ptr" (Ptr.Ptr Fun_ptr) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Fun_ptr")

instance HsBindgen.Runtime.HasCField.HasCField Fun_ptr "un_Fun_ptr" where

  type CFieldType Fun_ptr "un_Fun_ptr" =
    Ptr.FunPtr Fun_ptr_Deref

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @forward_declaration@

    __defined at:__ @functions\/circular_dependency_fun.h:5:8@

    __exported by:__ @functions\/circular_dependency_fun.h@
-}
data Forward_declaration = Forward_declaration
  { forward_declaration_f :: Fun_ptr
    {- ^ __C declaration:__ @f@

         __defined at:__ @functions\/circular_dependency_fun.h:6:11@

         __exported by:__ @functions\/circular_dependency_fun.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Forward_declaration where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Forward_declaration
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"forward_declaration_f") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Forward_declaration forward_declaration_f2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"forward_declaration_f") ptr0 forward_declaration_f2

instance HsBindgen.Runtime.HasCField.HasCField Forward_declaration "forward_declaration_f" where

  type CFieldType Forward_declaration "forward_declaration_f" =
    Fun_ptr

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Forward_declaration) "forward_declaration_f")
         ) => GHC.Records.HasField "forward_declaration_f" (Ptr.Ptr Forward_declaration) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"forward_declaration_f")

-- | __unique:__ @instance ToFunPtr ((Ptr.Ptr Forward_declaration) -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_fbe9c5dca66824d3 ::
     ((Ptr.Ptr Forward_declaration) -> IO ())
  -> IO (Ptr.FunPtr ((Ptr.Ptr Forward_declaration) -> IO ()))

-- | __unique:__ @instance FromFunPtr ((Ptr.Ptr Forward_declaration) -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_b3640137a9cf92cc ::
     Ptr.FunPtr ((Ptr.Ptr Forward_declaration) -> IO ())
  -> (Ptr.Ptr Forward_declaration) -> IO ()

instance HsBindgen.Runtime.FunPtr.ToFunPtr ((Ptr.Ptr Forward_declaration) -> IO ()) where

  toFunPtr = hs_bindgen_fbe9c5dca66824d3

instance HsBindgen.Runtime.FunPtr.FromFunPtr ((Ptr.Ptr Forward_declaration) -> IO ()) where

  fromFunPtr = hs_bindgen_b3640137a9cf92cc
