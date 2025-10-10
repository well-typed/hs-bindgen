{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.FunPtr.Class
import Prelude ((<*>), Eq, IO, Int, Ord, Show, pure)

{-| Auxiliary type used by 'Fun_ptr'

__defined at:__ @circular_dependency_fun.h:3:16@

__exported by:__ @circular_dependency_fun.h@
-}
newtype Fun_ptr_Deref = Fun_ptr_Deref
  { un_Fun_ptr_Deref :: (Ptr.Ptr Forward_declaration) -> IO ()
  }

foreign import ccall safe "wrapper" toFun_ptr_Deref
  :: Fun_ptr_Deref
  -> IO (Ptr.FunPtr Fun_ptr_Deref)

foreign import ccall safe "dynamic" fromFun_ptr_Deref
  :: Ptr.FunPtr Fun_ptr_Deref
  -> Fun_ptr_Deref

instance HsBindgen.Runtime.FunPtr.Class.ToFunPtr Fun_ptr_Deref where

  toFunPtr = toFun_ptr_Deref

instance HsBindgen.Runtime.FunPtr.Class.FromFunPtr Fun_ptr_Deref where

  fromFunPtr = fromFun_ptr_Deref

{-| __C declaration:__ @fun_ptr@

    __defined at:__ @circular_dependency_fun.h:3:16@

    __exported by:__ @circular_dependency_fun.h@
-}
newtype Fun_ptr = Fun_ptr
  { un_Fun_ptr :: Ptr.FunPtr Fun_ptr_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @forward_declaration@

    __defined at:__ @circular_dependency_fun.h:5:8@

    __exported by:__ @circular_dependency_fun.h@
-}
data Forward_declaration = Forward_declaration
  { forward_declaration_f :: Fun_ptr
    {- ^ __C declaration:__ @f@

         __defined at:__ @circular_dependency_fun.h:6:11@

         __exported by:__ @circular_dependency_fun.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Forward_declaration where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Forward_declaration
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Forward_declaration forward_declaration_f2 ->
            F.pokeByteOff ptr0 (0 :: Int) forward_declaration_f2
