{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
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

{-| __C declaration:__ @struct struct2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 13:8@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
data Struct2 = Struct2
  { struct2_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/special\/parse_failure_long_double.h 14:7@

         __exported by:__ @types\/special\/parse_failure_long_double.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"struct2_x") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_x2 ->
            HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"struct2_x") ptr0 struct2_x2

instance Data.Primitive.Types.Prim Struct2 where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Struct2 (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct2 v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2 struct2_x4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 struct2_x4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Struct2 (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Struct2 v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2 struct2_x4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 struct2_x4 s3

instance HsBindgen.Runtime.HasCField.HasCField Struct2 "struct2_x" where

  type CFieldType Struct2 "struct2_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Struct2) "struct2_x")
         ) => GHC.Records.HasField "struct2_x" (Ptr.Ptr Struct2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"struct2_x")
