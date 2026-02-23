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

{-| Auxiliary type used by 'FunPtr'

__C declaration:__ @FunPtr@

__defined at:__ @edge-cases\/aux_funptr_newtypes.h 6:16@

__exported by:__ @edge-cases\/aux_funptr_newtypes.h@
-}
newtype FunPtr_Aux = FunPtr_Aux
  { unwrapFunPtr_Aux :: Foo -> IO ()
  }
  deriving stock (RIP.Generic)

instance ( ((~) ty) (Foo -> IO ())
         ) => RIP.HasField "unwrapFunPtr_Aux" (RIP.Ptr FunPtr_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunPtr_Aux")

instance HasCField.HasCField FunPtr_Aux "unwrapFunPtr_Aux" where

  type CFieldType FunPtr_Aux "unwrapFunPtr_Aux" =
    Foo -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FunPtr@

    __defined at:__ @edge-cases\/aux_funptr_newtypes.h 6:16@

    __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
-}
newtype FunPtr = FunPtr
  { unwrapFunPtr :: RIP.FunPtr FunPtr_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr FunPtr_Aux)
         ) => RIP.HasField "unwrapFunPtr" (RIP.Ptr FunPtr) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapFunPtr")

instance HasCField.HasCField FunPtr "unwrapFunPtr" where

  type CFieldType FunPtr "unwrapFunPtr" =
    RIP.FunPtr FunPtr_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/aux_funptr_newtypes.h 8:8@

    __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
-}
data Foo = Foo
  { foo_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/aux_funptr_newtypes.h 9:7@

         __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
    -}
  , foo_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/aux_funptr_newtypes.h 10:7@

         __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (RIP.Proxy @"foo_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"foo_y") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_x2 foo_y3 ->
               HasCField.writeRaw (RIP.Proxy @"foo_x") ptr0 foo_x2
            >> HasCField.writeRaw (RIP.Proxy @"foo_y") ptr0 foo_y3

deriving via Marshal.EquivStorable Foo instance RIP.Storable Foo

instance HasCField.HasCField Foo "foo_x" where

  type CFieldType Foo "foo_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "foo_x" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_x")

instance HasCField.HasCField Foo "foo_y" where

  type CFieldType Foo "foo_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "foo_y" (RIP.Ptr Foo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_y")
