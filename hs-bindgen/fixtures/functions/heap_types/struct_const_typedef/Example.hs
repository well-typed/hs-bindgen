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
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct S@

    __defined at:__ @functions\/heap_types\/struct_const_typedef.h 3:8@

    __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
-}
data S = S
  { s_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/heap_types\/struct_const_typedef.h 4:7@

         __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"s_x") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_x2 ->
            HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"s_x") ptr0 s_x2

instance Data.Primitive.Types.Prim S where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S s_x4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 s_x4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S s_x4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 s_x4 s3

instance HsBindgen.Runtime.HasCField.HasCField S "s_x" where

  type CFieldType S "s_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S) "s_x")
         ) => GHC.Records.HasField "s_x" (Ptr.Ptr S) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s_x")

{-| __C declaration:__ @T@

    __defined at:__ @functions\/heap_types\/struct_const_typedef.h 7:24@

    __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
-}
newtype T = T
  { unwrapT :: S
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType T) "unwrapT")
         ) => GHC.Records.HasField "unwrapT" (Ptr.Ptr T) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapT")

instance HsBindgen.Runtime.HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = S

  offset# = \_ -> \_ -> 0
