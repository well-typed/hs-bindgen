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
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FLAM
import qualified HsBindgen.Runtime.HasCField
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Pascal_Aux = Pascal
  { pascal_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 3:9@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Pascal_Aux where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Pascal
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"pascal_len") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pascal pascal_len2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"pascal_len") ptr0 pascal_len2

instance Data.Primitive.Types.Prim Pascal_Aux where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Pascal (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Pascal v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Pascal pascal_len4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 pascal_len4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Pascal (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Pascal v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Pascal pascal_len4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 pascal_len4 s3

instance HsBindgen.Runtime.HasCField.HasCField Pascal_Aux "pascal_len" where

  type CFieldType Pascal_Aux "pascal_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Pascal_Aux) "pascal_len")
         ) => GHC.Records.HasField "pascal_len" (Ptr.Ptr Pascal_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"pascal_len")

instance HsBindgen.Runtime.FLAM.Offset FC.CChar Pascal_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Pascal =
  (HsBindgen.Runtime.FLAM.WithFlam FC.CChar) Pascal_Aux

{-| __C declaration:__ @struct \@foo_bar@

    __defined at:__ @edge-cases\/flam.h 10:2@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_bar = Foo_bar
  { foo_bar_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/flam.h 11:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , foo_bar_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/flam.h 12:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo_bar where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo_bar
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_bar_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_bar_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_bar foo_bar_x2 foo_bar_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_bar_x") ptr0 foo_bar_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_bar_y") ptr0 foo_bar_y3

instance Data.Primitive.Types.Prim Foo_bar where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Foo_bar (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Foo_bar v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Foo_bar foo_bar_x4 foo_bar_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) foo_bar_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) foo_bar_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Foo_bar (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Foo_bar v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Foo_bar foo_bar_x4 foo_bar_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) foo_bar_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) foo_bar_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Foo_bar "foo_bar_x" where

  type CFieldType Foo_bar "foo_bar_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo_bar) "foo_bar_x")
         ) => GHC.Records.HasField "foo_bar_x" (Ptr.Ptr Foo_bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_bar_x")

instance HsBindgen.Runtime.HasCField.HasCField Foo_bar "foo_bar_y" where

  type CFieldType Foo_bar "foo_bar_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo_bar) "foo_bar_y")
         ) => GHC.Records.HasField "foo_bar_y" (Ptr.Ptr Foo_bar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_bar_y")

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_Aux = Foo
  { foo_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 9:6@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo_Aux where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"foo_len") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_len2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"foo_len") ptr0 foo_len2

instance Data.Primitive.Types.Prim Foo_Aux where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Foo (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Foo v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Foo foo_len4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 foo_len4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Foo (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Foo v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Foo foo_len4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 foo_len4 s3

instance HsBindgen.Runtime.HasCField.HasCField Foo_Aux "foo_len" where

  type CFieldType Foo_Aux "foo_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo_Aux) "foo_len")
         ) => GHC.Records.HasField "foo_len" (Ptr.Ptr Foo_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"foo_len")

instance HsBindgen.Runtime.FLAM.Offset Foo_bar Foo_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Foo =
  (HsBindgen.Runtime.FLAM.WithFlam Foo_bar) Foo_Aux

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Diff_Aux = Diff
  { diff_first :: FC.CLong
    {- ^ __C declaration:__ @first@

         __defined at:__ @edge-cases\/flam.h 18:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , diff_second :: FC.CChar
    {- ^ __C declaration:__ @second@

         __defined at:__ @edge-cases\/flam.h 19:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Diff_Aux where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Diff
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"diff_first") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"diff_second") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Diff diff_first2 diff_second3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"diff_first") ptr0 diff_first2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"diff_second") ptr0 diff_second3

instance Data.Primitive.Types.Prim Diff_Aux where

  sizeOf# = \_ -> (16#)

  alignment# = \_ -> (8#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Diff (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Diff v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Diff diff_first4 diff_second5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) diff_first4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) diff_second5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Diff (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Diff v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Diff diff_first4 diff_second5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) diff_first4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) diff_second5 s6

instance HsBindgen.Runtime.HasCField.HasCField Diff_Aux "diff_first" where

  type CFieldType Diff_Aux "diff_first" = FC.CLong

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Diff_Aux) "diff_first")
         ) => GHC.Records.HasField "diff_first" (Ptr.Ptr Diff_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"diff_first")

instance HsBindgen.Runtime.HasCField.HasCField Diff_Aux "diff_second" where

  type CFieldType Diff_Aux "diff_second" = FC.CChar

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Diff_Aux) "diff_second")
         ) => GHC.Records.HasField "diff_second" (Ptr.Ptr Diff_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"diff_second")

instance HsBindgen.Runtime.FLAM.Offset FC.CChar Diff_Aux where

  offset = \_ty0 -> 9

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Diff =
  (HsBindgen.Runtime.FLAM.WithFlam FC.CChar) Diff_Aux

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

__C declaration:__ @struct triplets@

__defined at:__ @edge-cases\/flam.h 26:8@

__exported by:__ @edge-cases\/flam.h@
-}
data Triplets_Aux = Triplets
  { triplets_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 27:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Triplets_Aux where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Triplets
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"triplets_len") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Triplets triplets_len2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"triplets_len") ptr0 triplets_len2

instance Data.Primitive.Types.Prim Triplets_Aux where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Triplets (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Triplets v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Triplets triplets_len4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 triplets_len4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Triplets (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Triplets v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Triplets triplets_len4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 triplets_len4 s3

instance HsBindgen.Runtime.HasCField.HasCField Triplets_Aux "triplets_len" where

  type CFieldType Triplets_Aux "triplets_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplets_Aux) "triplets_len")
         ) => GHC.Records.HasField "triplets_len" (Ptr.Ptr Triplets_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"triplets_len")

instance HsBindgen.Runtime.FLAM.Offset ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) Triplets_Aux where

  offset = \_ty0 -> 4

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

__C declaration:__ @struct triplets@

__defined at:__ @edge-cases\/flam.h 26:8@

__exported by:__ @edge-cases\/flam.h@
-}
type Triplets =
  (HsBindgen.Runtime.FLAM.WithFlam ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) Triplets_Aux
