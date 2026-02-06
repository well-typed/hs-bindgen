{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct S1@

    __defined at:__ @declarations\/forward_declaration.h 3:8@

    __exported by:__ @declarations\/forward_declaration.h@
-}
data S1_t = S1_t
  { s1_t_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @declarations\/forward_declaration.h 4:7@

         __exported by:__ @declarations\/forward_declaration.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S1_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S1_t where

  readRaw =
    \ptr0 ->
          pure S1_t
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s1_t_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_t s1_t_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s1_t_a") ptr0 s1_t_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable S1_t instance F.Storable S1_t

instance HsBindgen.Runtime.HasCField.HasCField S1_t "s1_t_a" where

  type CFieldType S1_t "s1_t_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1_t) "s1_t_a")
         ) => GHC.Records.HasField "s1_t_a" (Ptr.Ptr S1_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s1_t_a")

{-| __C declaration:__ @struct S2@

    __defined at:__ @declarations\/forward_declaration.h 9:8@

    __exported by:__ @declarations\/forward_declaration.h@
-}
data S2 = S2
  { s2_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @declarations\/forward_declaration.h 10:7@

         __exported by:__ @declarations\/forward_declaration.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize S2 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw S2 where

  readRaw =
    \ptr0 ->
          pure S2
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"s2_a") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw S2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_a2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"s2_a") ptr0 s2_a2

deriving via HsBindgen.Runtime.Marshal.EquivStorable S2 instance F.Storable S2

instance HsBindgen.Runtime.HasCField.HasCField S2 "s2_a" where

  type CFieldType S2 "s2_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2) "s2_a")
         ) => GHC.Records.HasField "s2_a" (Ptr.Ptr S2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s2_a")
