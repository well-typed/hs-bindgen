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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.TypeEquality (TyEq)
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

instance Data.Primitive.Types.Prim S1_t where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S1_t (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S1_t v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S1_t s1_t_a4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 s1_t_a4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S1_t (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S1_t v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S1_t s1_t_a4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 s1_t_a4 s3

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

instance Data.Primitive.Types.Prim S2 where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S2 (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S2 v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2 s2_a4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 s2_a4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S2 (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S2 v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2 s2_a4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 s2_a4 s3

instance HsBindgen.Runtime.HasCField.HasCField S2 "s2_a" where

  type CFieldType S2 "s2_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2) "s2_a")
         ) => GHC.Records.HasField "s2_a" (Ptr.Ptr S2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s2_a")
