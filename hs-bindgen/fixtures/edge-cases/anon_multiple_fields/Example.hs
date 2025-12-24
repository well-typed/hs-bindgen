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

{-| __C declaration:__ @struct \@some_struct_field1@

    __defined at:__ @edge-cases\/anon_multiple_fields.h:5:3@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
data Some_struct_field1 = Some_struct_field1
  { some_struct_field1_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_fields.h:5:16@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field1_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_fields.h:5:23@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Some_struct_field1 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Some_struct_field1
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"some_struct_field1_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"some_struct_field1_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct_field1 some_struct_field1_x2 some_struct_field1_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"some_struct_field1_x") ptr0 some_struct_field1_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"some_struct_field1_y") ptr0 some_struct_field1_y3

instance Data.Primitive.Types.Prim Some_struct_field1 where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Some_struct_field1 (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Some_struct_field1 v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Some_struct_field1 some_struct_field1_x4 some_struct_field1_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) some_struct_field1_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) some_struct_field1_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Some_struct_field1 (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Some_struct_field1 v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Some_struct_field1 some_struct_field1_x4 some_struct_field1_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) some_struct_field1_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) some_struct_field1_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Some_struct_field1 "some_struct_field1_x" where

  type CFieldType Some_struct_field1 "some_struct_field1_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Some_struct_field1) "some_struct_field1_x")
         ) => GHC.Records.HasField "some_struct_field1_x" (Ptr.Ptr Some_struct_field1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"some_struct_field1_x")

instance HsBindgen.Runtime.HasCField.HasCField Some_struct_field1 "some_struct_field1_y" where

  type CFieldType Some_struct_field1 "some_struct_field1_y" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Some_struct_field1) "some_struct_field1_y")
         ) => GHC.Records.HasField "some_struct_field1_y" (Ptr.Ptr Some_struct_field1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"some_struct_field1_y")

{-| __C declaration:__ @struct some_struct@

    __defined at:__ @edge-cases\/anon_multiple_fields.h:4:8@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
data Some_struct = Some_struct
  { some_struct_field1 :: Some_struct_field1
    {- ^ __C declaration:__ @field1@

         __defined at:__ @edge-cases\/anon_multiple_fields.h:5:28@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field2 :: Some_struct_field1
    {- ^ __C declaration:__ @field2@

         __defined at:__ @edge-cases\/anon_multiple_fields.h:5:36@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field3 :: Some_struct_field1
    {- ^ __C declaration:__ @field3@

         __defined at:__ @edge-cases\/anon_multiple_fields.h:5:44@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Some_struct where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Some_struct
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"some_struct_field1") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"some_struct_field2") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"some_struct_field3") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct some_struct_field12 some_struct_field23 some_struct_field34 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"some_struct_field1") ptr0 some_struct_field12
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"some_struct_field2") ptr0 some_struct_field23
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"some_struct_field3") ptr0 some_struct_field34

instance Data.Primitive.Types.Prim Some_struct where

  sizeOf# = \_ -> (24#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Some_struct (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, Some_struct v4 v6 v8 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Some_struct some_struct_field14 some_struct_field25 some_struct_field36 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (0#)) some_struct_field14 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (1#)) some_struct_field25 s7 of
                      s8 ->
                        Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (3#) i1) (2#)) some_struct_field36 s8

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Some_struct (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) s5 of
                    (# s7, v8 #) -> (# s7, Some_struct v4 v6 v8 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Some_struct some_struct_field14 some_struct_field25 some_struct_field36 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (0#)) some_struct_field14 s3 of
                  s7 ->
                    case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (1#)) some_struct_field25 s7 of
                      s8 ->
                        Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (3#) i1) (2#)) some_struct_field36 s8

instance HsBindgen.Runtime.HasCField.HasCField Some_struct "some_struct_field1" where

  type CFieldType Some_struct "some_struct_field1" =
    Some_struct_field1

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Some_struct) "some_struct_field1")
         ) => GHC.Records.HasField "some_struct_field1" (Ptr.Ptr Some_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"some_struct_field1")

instance HsBindgen.Runtime.HasCField.HasCField Some_struct "some_struct_field2" where

  type CFieldType Some_struct "some_struct_field2" =
    Some_struct_field1

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Some_struct) "some_struct_field2")
         ) => GHC.Records.HasField "some_struct_field2" (Ptr.Ptr Some_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"some_struct_field2")

instance HsBindgen.Runtime.HasCField.HasCField Some_struct "some_struct_field3" where

  type CFieldType Some_struct "some_struct_field3" =
    Some_struct_field1

  offset# = \_ -> \_ -> 16

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Some_struct) "some_struct_field3")
         ) => GHC.Records.HasField "some_struct_field3" (Ptr.Ptr Some_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"some_struct_field3")
