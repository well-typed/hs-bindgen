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
    ( Example.FunPtr_Aux(..)
    , Example.FunPtr(..)
    , Example.Foo(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| Auxiliary type used by 'FunPtr'

    __C declaration:__ @FunPtr@

    __defined at:__ @edge-cases\/aux_funptr_newtypes.h 6:16@

    __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
-}
newtype FunPtr_Aux = FunPtr_Aux
  { unwrapFunPtr_Aux :: Foo -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (Foo -> IO ())
         ) => BG.CompatHasField.HasField "unwrapFunPtr_Aux" FunPtr_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> FunPtr_Aux {unwrapFunPtr_Aux = y1}
      , BG.getField @"unwrapFunPtr_Aux" x0
      )

instance ( ty ~ (Foo -> IO ())
         ) => BG.HasField "unwrapFunPtr_Aux" (BG.Ptr FunPtr_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunPtr_Aux")

instance HasCField.HasCField FunPtr_Aux "unwrapFunPtr_Aux" where

  type CFieldType FunPtr_Aux "unwrapFunPtr_Aux" =
    Foo -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @FunPtr@

    __defined at:__ @edge-cases\/aux_funptr_newtypes.h 6:16@

    __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
-}
newtype FunPtr = FunPtr
  { unwrapFunPtr :: BG.FunPtr FunPtr_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr FunPtr_Aux
         ) => BG.CompatHasField.HasField "unwrapFunPtr" FunPtr ty where

  hasField =
    \x0 ->
      (\y1 ->
         FunPtr {unwrapFunPtr = y1}, BG.getField @"unwrapFunPtr" x0)

instance ( ty ~ BG.FunPtr FunPtr_Aux
         ) => BG.HasField "unwrapFunPtr" (BG.Ptr FunPtr) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapFunPtr")

instance HasCField.HasCField FunPtr "unwrapFunPtr" where

  type CFieldType FunPtr "unwrapFunPtr" =
    BG.FunPtr FunPtr_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/aux_funptr_newtypes.h 8:8@

    __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
-}
data Foo = Foo
  { foo_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/aux_funptr_newtypes.h 9:7@

         __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
    -}
  , foo_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/aux_funptr_newtypes.h 10:7@

         __exported by:__ @edge-cases\/aux_funptr_newtypes.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (BG.Proxy @"foo_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"foo_y") ptr0

instance Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_x2 foo_y3 ->
               HasCField.writeRaw (BG.Proxy @"foo_x") ptr0 foo_x2
            >> HasCField.writeRaw (BG.Proxy @"foo_y") ptr0 foo_y3

deriving via Marshal.EquivStorable Foo instance BG.Storable Foo

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_x" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_x = y1, foo_y = BG.getField @"foo_y" x0}
      , BG.getField @"foo_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "foo_x" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_x")

instance HasCField.HasCField Foo "foo_x" where

  type CFieldType Foo "foo_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_y" Foo ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo {foo_y = y1, foo_x = BG.getField @"foo_x" x0}
      , BG.getField @"foo_y" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "foo_y" (BG.Ptr Foo) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_y")

instance HasCField.HasCField Foo "foo_y" where

  type CFieldType Foo "foo_y" = BG.CInt

  offset# = \_ -> \_ -> 4
