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
import qualified HsBindgen.Runtime.FlexibleArrayMember
import qualified HsBindgen.Runtime.HasCField
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
data Vector_Aux = Vector
  { vector_length :: FC.CInt
    {- ^ __C declaration:__ @length@

         __defined at:__ @edge-cases\/flam_functions.h 2:7@

         __exported by:__ @edge-cases\/flam_functions.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Vector_Aux where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Vector
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"vector_length") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_length2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"vector_length") ptr0 vector_length2

instance Data.Primitive.Types.Prim Vector_Aux where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (8#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Vector (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Vector v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Vector vector_length4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 vector_length4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Vector (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Vector v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Vector vector_length4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 vector_length4 s3

instance HsBindgen.Runtime.FlexibleArrayMember.Offset FC.CLong Vector_Aux where

  offset = \_ty0 -> 8

{-| __C declaration:__ @struct Vector@

    __defined at:__ @edge-cases\/flam_functions.h 1:8@

    __exported by:__ @edge-cases\/flam_functions.h@
-}
type Vector =
  (HsBindgen.Runtime.FlexibleArrayMember.WithFlexibleArrayMember FC.CLong) Vector_Aux

instance HsBindgen.Runtime.HasCField.HasCField Vector_Aux "vector_length" where

  type CFieldType Vector_Aux "vector_length" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Vector_Aux) "vector_length")
         ) => GHC.Records.HasField "vector_length" (Ptr.Ptr Vector_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"vector_length")
