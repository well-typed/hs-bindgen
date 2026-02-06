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
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct b@

    __defined at:__ @types\/structs\/circular_dependency_struct.h 3:8@

    __exported by:__ @types\/structs\/circular_dependency_struct.h@
-}
data B = B
  { b_toA :: Ptr.Ptr A
    {- ^ __C declaration:__ @toA@

         __defined at:__ @types\/structs\/circular_dependency_struct.h 4:13@

         __exported by:__ @types\/structs\/circular_dependency_struct.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize B where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"b_toA") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_toA2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"b_toA") ptr0 b_toA2

deriving via HsBindgen.Runtime.Marshal.EquivStorable B instance F.Storable B

instance HsBindgen.Runtime.HasCField.HasCField B "b_toA" where

  type CFieldType B "b_toA" = Ptr.Ptr A

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "b_toA")
         ) => GHC.Records.HasField "b_toA" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b_toA")

{-| __C declaration:__ @struct a@

    __defined at:__ @types\/structs\/circular_dependency_struct.h 7:8@

    __exported by:__ @types\/structs\/circular_dependency_struct.h@
-}
data A = A
  { a_toB :: B
    {- ^ __C declaration:__ @toB@

         __defined at:__ @types\/structs\/circular_dependency_struct.h 8:12@

         __exported by:__ @types\/structs\/circular_dependency_struct.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize A where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw A where

  readRaw =
    \ptr0 ->
          pure A
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"a_toB") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_toB2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"a_toB") ptr0 a_toB2

deriving via HsBindgen.Runtime.Marshal.EquivStorable A instance F.Storable A

instance HsBindgen.Runtime.HasCField.HasCField A "a_toB" where

  type CFieldType A "a_toB" = B

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "a_toB")
         ) => GHC.Records.HasField "a_toB" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a_toB")
