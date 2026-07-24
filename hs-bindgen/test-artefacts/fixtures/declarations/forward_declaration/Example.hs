{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S1_t(..)
    , Example.S2(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct S1@

    __defined at:__ @declarations\/forward_declaration.h 3:8@

    __exported by:__ @declarations\/forward_declaration.h@
-}
data S1_t = S1_t
  { s1_t_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @declarations\/forward_declaration.h 4:7@

         __exported by:__ @declarations\/forward_declaration.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S1_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1_t where

  readRaw =
    \ptr0 ->
          pure S1_t
      <*> HasCField.readRaw (BG.Proxy @"s1_t_a") ptr0

instance Marshal.WriteRaw S1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_t s1_t_a2 ->
            HasCField.writeRaw (BG.Proxy @"s1_t_a") ptr0 s1_t_a2

deriving via Marshal.EquivStorable S1_t instance BG.Storable S1_t

deriving via Struct.IsStructViaStorable S1_t instance Struct.IsStruct S1_t

{-| __C declaration:__ @a@

    __defined at:__ @declarations\/forward_declaration.h 4:7@

    __exported by:__ @declarations\/forward_declaration.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s1_t_a" S1_t ty where

  hasField =
    \x0 ->
      (\y1 -> S1_t {s1_t_a = y1}, BG.getField @"s1_t_a" x0)

instance (ty ~ BG.CInt) => BG.HasField "s1_t_a" (BG.Ptr S1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s1_t_a")

instance HasCField.HasCField S1_t "s1_t_a" where

  type CFieldType S1_t "s1_t_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct S2@

    __defined at:__ @declarations\/forward_declaration.h 9:8@

    __exported by:__ @declarations\/forward_declaration.h@
-}
data S2 = S2
  { s2_a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @declarations\/forward_declaration.h 10:7@

         __exported by:__ @declarations\/forward_declaration.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HasCField.readRaw (BG.Proxy @"s2_a") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_a2 ->
            HasCField.writeRaw (BG.Proxy @"s2_a") ptr0 s2_a2

deriving via Marshal.EquivStorable S2 instance BG.Storable S2

deriving via Struct.IsStructViaStorable S2 instance Struct.IsStruct S2

{-| __C declaration:__ @a@

    __defined at:__ @declarations\/forward_declaration.h 10:7@

    __exported by:__ @declarations\/forward_declaration.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "s2_a" S2 ty where

  hasField =
    \x0 ->
      (\y1 -> S2 {s2_a = y1}, BG.getField @"s2_a" x0)

instance (ty ~ BG.CInt) => BG.HasField "s2_a" (BG.Ptr S2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"s2_a")

instance HasCField.HasCField S2 "s2_a" where

  type CFieldType S2 "s2_a" = BG.CInt

  offset# = \_ -> \_ -> 0
