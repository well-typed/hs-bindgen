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

{-| __C declaration:__ @struct OkBefore@

    __defined at:__ @program-analysis\/selection_fail.h 1:8@

    __exported by:__ @program-analysis\/selection_fail.h@
-}
data OkBefore = OkBefore
  { okBefore_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_fail.h 2:7@

         __exported by:__ @program-analysis\/selection_fail.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize OkBefore where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw OkBefore where

  readRaw =
    \ptr0 ->
          pure OkBefore
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"okBefore_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw OkBefore where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkBefore okBefore_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"okBefore_x") ptr0 okBefore_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable OkBefore instance F.Storable OkBefore

instance Data.Primitive.Types.Prim OkBefore where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        OkBefore (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, OkBefore v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              OkBefore okBefore_x4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 okBefore_x4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        OkBefore (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, OkBefore v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              OkBefore okBefore_x4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 okBefore_x4 s3

instance HsBindgen.Runtime.HasCField.HasCField OkBefore "okBefore_x" where

  type CFieldType OkBefore "okBefore_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType OkBefore) "okBefore_x")
         ) => GHC.Records.HasField "okBefore_x" (Ptr.Ptr OkBefore) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"okBefore_x")

{-| __C declaration:__ @struct OkAfter@

    __defined at:__ @program-analysis\/selection_fail.h 26:8@

    __exported by:__ @program-analysis\/selection_fail.h@
-}
data OkAfter = OkAfter
  { okAfter_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_fail.h 27:7@

         __exported by:__ @program-analysis\/selection_fail.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize OkAfter where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw OkAfter where

  readRaw =
    \ptr0 ->
          pure OkAfter
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"okAfter_x") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw OkAfter where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkAfter okAfter_x2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"okAfter_x") ptr0 okAfter_x2

deriving via HsBindgen.Runtime.Marshal.EquivStorable OkAfter instance F.Storable OkAfter

instance Data.Primitive.Types.Prim OkAfter where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        OkAfter (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, OkAfter v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              OkAfter okAfter_x4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 okAfter_x4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        OkAfter (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, OkAfter v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              OkAfter okAfter_x4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 okAfter_x4 s3

instance HsBindgen.Runtime.HasCField.HasCField OkAfter "okAfter_x" where

  type CFieldType OkAfter "okAfter_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType OkAfter) "okAfter_x")
         ) => GHC.Records.HasField "okAfter_x" (Ptr.Ptr OkAfter) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"okAfter_x")
