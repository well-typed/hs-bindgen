{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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

import qualified Data.Array.Byte
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.SizedByteArray
import GHC.Prim ((*#), (+#), Int#)
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @opaque@

    __defined at:__ @functions\/decls_in_signature.h:2:8@

    __exported by:__ @functions\/decls_in_signature.h@
-}
data Opaque

{-| __C declaration:__ @outside@

    __defined at:__ @functions\/decls_in_signature.h:3:8@

    __exported by:__ @functions\/decls_in_signature.h@
-}
data Outside = Outside
  { outside_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h:4:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , outside_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h:5:7@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Outside where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Outside
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"outside_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"outside_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outside outside_x2 outside_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"outside_x") ptr0 outside_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"outside_y") ptr0 outside_y3

instance Data.Primitive.Types.Prim Outside where

  sizeOf# = \_ -> (8# :: Int#)

  alignment# = \_ -> (4# :: Int#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Outside (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) s3 of
                (# s5, v6 #) -> (# s5, Outside v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Outside outside_x4 outside_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) outside_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) outside_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Outside (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) s3 of
                (# s5, v6 #) -> (# s5, Outside v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Outside outside_x4 outside_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) outside_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) outside_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Outside "outside_x" where

  type CFieldType Outside "outside_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Outside) "outside_x")
         ) => GHC.Records.HasField "outside_x" (Ptr.Ptr Outside) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"outside_x")

instance HsBindgen.Runtime.HasCField.HasCField Outside "outside_y" where

  type CFieldType Outside "outside_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Outside) "outside_y")
         ) => GHC.Records.HasField "outside_y" (Ptr.Ptr Outside) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"outside_y")

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @named_struct@

__defined at:__ @functions\/decls_in_signature.h:17:16@

__exported by:__ @functions\/decls_in_signature.h@
-}
data Named_struct = Named_struct
  { named_struct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @functions\/decls_in_signature.h:17:35@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  , named_struct_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @functions\/decls_in_signature.h:17:42@

         __exported by:__ @functions\/decls_in_signature.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Named_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Named_struct
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"named_struct_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"named_struct_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named_struct named_struct_x2 named_struct_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"named_struct_x") ptr0 named_struct_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"named_struct_y") ptr0 named_struct_y3

instance Data.Primitive.Types.Prim Named_struct where

  sizeOf# = \_ -> (8# :: Int#)

  alignment# = \_ -> (4# :: Int#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Named_struct (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) s3 of
                (# s5, v6 #) -> (# s5, Named_struct v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Named_struct named_struct_x4 named_struct_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) named_struct_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) named_struct_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Named_struct (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) s3 of
                (# s5, v6 #) -> (# s5, Named_struct v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Named_struct named_struct_x4 named_struct_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (0# :: Int#)) named_struct_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2# :: Int#) i1) (1# :: Int#)) named_struct_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Named_struct "named_struct_x" where

  type CFieldType Named_struct "named_struct_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named_struct) "named_struct_x")
         ) => GHC.Records.HasField "named_struct_x" (Ptr.Ptr Named_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"named_struct_x")

instance HsBindgen.Runtime.HasCField.HasCField Named_struct "named_struct_y" where

  type CFieldType Named_struct "named_struct_y" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named_struct) "named_struct_y")
         ) => GHC.Records.HasField "named_struct_y" (Ptr.Ptr Named_struct) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"named_struct_y")

{-| __C declaration:__ @named_union@

    __defined at:__ @functions\/decls_in_signature.h:20:15@

    __exported by:__ @functions\/decls_in_signature.h@
-}
newtype Named_union = Named_union
  { un_Named_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance F.Storable Named_union

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 4) 4 instance Data.Primitive.Types.Prim Named_union

{-|

  __See:__ 'set_named_union_x'

__C declaration:__ @x@

__defined at:__ @functions\/decls_in_signature.h:20:33@

__exported by:__ @functions\/decls_in_signature.h@
-}
get_named_union_x ::
     Named_union
  -> FC.CInt
get_named_union_x =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_named_union_x'

-}
set_named_union_x ::
     FC.CInt
  -> Named_union
set_named_union_x =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_named_union_y'

__C declaration:__ @y@

__defined at:__ @functions\/decls_in_signature.h:20:41@

__exported by:__ @functions\/decls_in_signature.h@
-}
get_named_union_y ::
     Named_union
  -> FC.CChar
get_named_union_y =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_named_union_y'

-}
set_named_union_y ::
     FC.CChar
  -> Named_union
set_named_union_y =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Named_union "named_union_x" where

  type CFieldType Named_union "named_union_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named_union) "named_union_x")
         ) => GHC.Records.HasField "named_union_x" (Ptr.Ptr Named_union) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"named_union_x")

instance HsBindgen.Runtime.HasCField.HasCField Named_union "named_union_y" where

  type CFieldType Named_union "named_union_y" =
    FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named_union) "named_union_y")
         ) => GHC.Records.HasField "named_union_y" (Ptr.Ptr Named_union) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"named_union_y")
