{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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

{-| __C declaration:__ @struct S1@

    __defined at:__ @declarations\/forward_declaration.h 3:8@

    __exported by:__ @declarations\/forward_declaration.h@
-}
data S1_t = S1_t
  { s1_t_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @declarations\/forward_declaration.h 4:7@

         __exported by:__ @declarations\/forward_declaration.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S1_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S1_t where

  readRaw =
    \ptr0 ->
          pure S1_t
      <*> HasCField.readRaw (RIP.Proxy @"s1_t_a") ptr0

instance Marshal.WriteRaw S1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_t s1_t_a2 ->
            HasCField.writeRaw (RIP.Proxy @"s1_t_a") ptr0 s1_t_a2

deriving via Marshal.EquivStorable S1_t instance RIP.Storable S1_t

instance HasCField.HasCField S1_t "s1_t_a" where

  type CFieldType S1_t "s1_t_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s1_t_a" (RIP.Ptr S1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s1_t_a")

{-| __C declaration:__ @struct S2@

    __defined at:__ @declarations\/forward_declaration.h 9:8@

    __exported by:__ @declarations\/forward_declaration.h@
-}
data S2 = S2
  { s2_a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @declarations\/forward_declaration.h 10:7@

         __exported by:__ @declarations\/forward_declaration.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HasCField.readRaw (RIP.Proxy @"s2_a") ptr0

instance Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_a2 ->
            HasCField.writeRaw (RIP.Proxy @"s2_a") ptr0 s2_a2

deriving via Marshal.EquivStorable S2 instance RIP.Storable S2

instance HasCField.HasCField S2 "s2_a" where

  type CFieldType S2 "s2_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "s2_a" (RIP.Ptr S2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s2_a")
