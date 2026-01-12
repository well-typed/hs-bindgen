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
import qualified M
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct MyStruct@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 4:8@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
data MyStruct = MyStruct
  { myStruct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/fun_arg\/struct.h 4:23@

         __exported by:__ @binding-specs\/fun_arg\/struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable MyStruct where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure MyStruct
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"myStruct_x") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyStruct myStruct_x2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"myStruct_x") ptr0 myStruct_x2

instance Data.Primitive.Types.Prim MyStruct where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        MyStruct (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, MyStruct v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              MyStruct myStruct_x4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 myStruct_x4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        MyStruct (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, MyStruct v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              MyStruct myStruct_x4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 myStruct_x4 s3

instance HsBindgen.Runtime.HasCField.HasCField MyStruct "myStruct_x" where

  type CFieldType MyStruct "myStruct_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyStruct) "myStruct_x")
         ) => GHC.Records.HasField "myStruct_x" (Ptr.Ptr MyStruct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"myStruct_x")

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 7:25@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
newtype A = A
  { un_A :: MyStruct
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "un_A")
         ) => GHC.Records.HasField "un_A" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_A")

instance HsBindgen.Runtime.HasCField.HasCField A "un_A" where

  type CFieldType A "un_A" = MyStruct

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 8:11@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable, Data.Primitive.Types.Prim)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "un_B")
         ) => GHC.Records.HasField "un_B" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_B")

instance HsBindgen.Runtime.HasCField.HasCField B "un_B" where

  type CFieldType B "un_B" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/struct.h 21:11@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
newtype E = E
  { un_E :: M.C
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType E) "un_E")
         ) => GHC.Records.HasField "un_E" (Ptr.Ptr E) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_E")

instance HsBindgen.Runtime.HasCField.HasCField E "un_E" where

  type CFieldType E "un_E" = M.C

  offset# = \_ -> \_ -> 0
