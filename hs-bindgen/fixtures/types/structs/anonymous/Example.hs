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
import GHC.Prim ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct \@S1_c@

    __defined at:__ @types\/structs\/anonymous.h 3:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S1_c = S1_c
  { s1_c_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 4:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s1_c_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 5:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1_c where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1_c
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_c_a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_c_b") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_c s1_c_a2 s1_c_b3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_c_a") ptr0 s1_c_a2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_c_b") ptr0 s1_c_b3

instance Data.Primitive.Types.Prim S1_c where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S1_c (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S1_c v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S1_c s1_c_a4 s1_c_b5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s1_c_a4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s1_c_b5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S1_c (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S1_c v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S1_c s1_c_a4 s1_c_b5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s1_c_a4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s1_c_b5 s6

instance HsBindgen.Runtime.HasCField.HasCField S1_c "s1_c_a" where

  type CFieldType S1_c "s1_c_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1_c) "s1_c_a")
         ) => GHC.Records.HasField "s1_c_a" (Ptr.Ptr S1_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_c_a")

instance HsBindgen.Runtime.HasCField.HasCField S1_c "s1_c_b" where

  type CFieldType S1_c "s1_c_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1_c) "s1_c_b")
         ) => GHC.Records.HasField "s1_c_b" (Ptr.Ptr S1_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_c_b")

{-| __C declaration:__ @struct S1@

    __defined at:__ @types\/structs\/anonymous.h 2:8@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S1 = S1
  { s1_c :: S1_c
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/anonymous.h 6:5@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s1_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 8:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s1_d") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_c2 s1_d3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_c") ptr0 s1_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s1_d") ptr0 s1_d3

instance Data.Primitive.Types.Prim S1 where

  sizeOf# = \_ -> (12#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S1 (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S1 v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S1 s1_c4 s1_d5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s1_c4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s1_d5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S1 (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S1 v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S1 s1_c4 s1_d5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s1_c4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s1_d5 s6

instance HsBindgen.Runtime.HasCField.HasCField S1 "s1_c" where

  type CFieldType S1 "s1_c" = S1_c

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1) "s1_c")
         ) => GHC.Records.HasField "s1_c" (Ptr.Ptr S1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_c")

instance HsBindgen.Runtime.HasCField.HasCField S1 "s1_d" where

  type CFieldType S1 "s1_d" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S1) "s1_d")
         ) => GHC.Records.HasField "s1_d" (Ptr.Ptr S1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s1_d")

{-| __C declaration:__ @struct \@S2_inner_deep@

    __defined at:__ @types\/structs\/anonymous.h 15:5@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2_inner_deep = S2_inner_deep
  { s2_inner_deep_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 16:11@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2_inner_deep where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner_deep
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner_deep_b") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner_deep s2_inner_deep_b2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner_deep_b") ptr0 s2_inner_deep_b2

instance Data.Primitive.Types.Prim S2_inner_deep where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S2_inner_deep (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S2_inner_deep v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2_inner_deep s2_inner_deep_b4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 s2_inner_deep_b4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S2_inner_deep (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, S2_inner_deep v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2_inner_deep s2_inner_deep_b4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 s2_inner_deep_b4 s3

instance HsBindgen.Runtime.HasCField.HasCField S2_inner_deep "s2_inner_deep_b" where

  type CFieldType S2_inner_deep "s2_inner_deep_b" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2_inner_deep) "s2_inner_deep_b")
         ) => GHC.Records.HasField "s2_inner_deep_b" (Ptr.Ptr S2_inner_deep) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner_deep_b")

{-| __C declaration:__ @struct \@S2_inner@

    __defined at:__ @types\/structs\/anonymous.h 13:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2_inner = S2_inner
  { s2_inner_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 14:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s2_inner_deep :: S2_inner_deep
    {- ^ __C declaration:__ @deep@

         __defined at:__ @types\/structs\/anonymous.h 17:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2_inner where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner_a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner_deep") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner s2_inner_a2 s2_inner_deep3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner_a") ptr0 s2_inner_a2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner_deep") ptr0 s2_inner_deep3

instance Data.Primitive.Types.Prim S2_inner where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S2_inner (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S2_inner v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2_inner s2_inner_a4 s2_inner_deep5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2_inner_a4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s2_inner_deep5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S2_inner (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S2_inner v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2_inner s2_inner_a4 s2_inner_deep5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2_inner_a4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s2_inner_deep5 s6

instance HsBindgen.Runtime.HasCField.HasCField S2_inner "s2_inner_a" where

  type CFieldType S2_inner "s2_inner_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2_inner) "s2_inner_a")
         ) => GHC.Records.HasField "s2_inner_a" (Ptr.Ptr S2_inner) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner_a")

instance HsBindgen.Runtime.HasCField.HasCField S2_inner "s2_inner_deep" where

  type CFieldType S2_inner "s2_inner_deep" =
    S2_inner_deep

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2_inner) "s2_inner_deep")
         ) => GHC.Records.HasField "s2_inner_deep" (Ptr.Ptr S2_inner) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner_deep")

{-| __C declaration:__ @struct S2@

    __defined at:__ @types\/structs\/anonymous.h 12:8@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S2 = S2
  { s2_inner :: S2_inner
    {- ^ __C declaration:__ @inner@

         __defined at:__ @types\/structs\/anonymous.h 18:5@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s2_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 20:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_inner") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s2_d") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_inner2 s2_d3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_inner") ptr0 s2_inner2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s2_d") ptr0 s2_d3

instance Data.Primitive.Types.Prim S2 where

  sizeOf# = \_ -> (12#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S2 (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S2 v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2 s2_inner4 s2_d5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2_inner4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s2_d5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S2 (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S2 v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S2 s2_inner4 s2_d5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2_inner4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s2_d5 s6

instance HsBindgen.Runtime.HasCField.HasCField S2 "s2_inner" where

  type CFieldType S2 "s2_inner" = S2_inner

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2) "s2_inner")
         ) => GHC.Records.HasField "s2_inner" (Ptr.Ptr S2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_inner")

instance HsBindgen.Runtime.HasCField.HasCField S2 "s2_d" where

  type CFieldType S2 "s2_d" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S2) "s2_d")
         ) => GHC.Records.HasField "s2_d" (Ptr.Ptr S2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s2_d")

{-| __C declaration:__ @struct S3@

    __defined at:__ @types\/structs\/anonymous.h 24:8@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S3 = S3
  { s3_c :: Ptr.Ptr (Ptr.Ptr S3_c)
    {- ^ __C declaration:__ @c@

         __defined at:__ @types\/structs\/anonymous.h 28:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s3_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @types\/structs\/anonymous.h 30:7@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S3 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure S3
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_c") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_d") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3 s3_c2 s3_d3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_c") ptr0 s3_c2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_d") ptr0 s3_d3

instance HsBindgen.Runtime.HasCField.HasCField S3 "s3_c" where

  type CFieldType S3 "s3_c" = Ptr.Ptr (Ptr.Ptr S3_c)

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3) "s3_c")
         ) => GHC.Records.HasField "s3_c" (Ptr.Ptr S3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_c")

instance HsBindgen.Runtime.HasCField.HasCField S3 "s3_d" where

  type CFieldType S3 "s3_d" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3) "s3_d")
         ) => GHC.Records.HasField "s3_d" (Ptr.Ptr S3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_d")

{-| __C declaration:__ @struct \@S3_c@

    __defined at:__ @types\/structs\/anonymous.h 25:3@

    __exported by:__ @types\/structs\/anonymous.h@
-}
data S3_c = S3_c
  { s3_c_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @types\/structs\/anonymous.h 26:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  , s3_c_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @types\/structs\/anonymous.h 27:9@

         __exported by:__ @types\/structs\/anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S3_c where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S3_c
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_c_a") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"s3_c_b") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_c s3_c_a2 s3_c_b3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_c_a") ptr0 s3_c_a2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"s3_c_b") ptr0 s3_c_b3

instance Data.Primitive.Types.Prim S3_c where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        S3_c (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S3_c v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S3_c s3_c_a4 s3_c_b5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s3_c_a4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3_c_b5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        S3_c (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, S3_c v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              S3_c s3_c_a4 s3_c_b5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s3_c_a4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3_c_b5 s6

instance HsBindgen.Runtime.HasCField.HasCField S3_c "s3_c_a" where

  type CFieldType S3_c "s3_c_a" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3_c) "s3_c_a")
         ) => GHC.Records.HasField "s3_c_a" (Ptr.Ptr S3_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_c_a")

instance HsBindgen.Runtime.HasCField.HasCField S3_c "s3_c_b" where

  type CFieldType S3_c "s3_c_b" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType S3_c) "s3_c_b")
         ) => GHC.Records.HasField "s3_c_b" (Ptr.Ptr S3_c) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"s3_c_b")
