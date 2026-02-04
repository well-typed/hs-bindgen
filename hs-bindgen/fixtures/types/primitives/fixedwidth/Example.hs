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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct foo@

    __defined at:__ @types\/primitives\/fixedwidth.h 3:8@

    __exported by:__ @types\/primitives\/fixedwidth.h@
-}
data Foo = Foo
  { foo_sixty_four :: HsBindgen.Runtime.LibC.Word64
    {- ^ __C declaration:__ @sixty_four@

         __defined at:__ @types\/primitives\/fixedwidth.h 4:11@

         __exported by:__ @types\/primitives\/fixedwidth.h@
    -}
  , foo_thirty_two :: HsBindgen.Runtime.LibC.Word32
    {- ^ __C declaration:__ @thirty_two@

         __defined at:__ @types\/primitives\/fixedwidth.h 5:11@

         __exported by:__ @types\/primitives\/fixedwidth.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Foo where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Foo where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_sixty_four") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_thirty_two") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Foo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_sixty_four") ptr0 foo_sixty_four2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_thirty_two") ptr0 foo_thirty_two3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Foo instance F.Storable Foo

instance Data.Primitive.Types.Prim Foo where

  sizeOf# = \_ -> (16#)

  alignment# = \_ -> (8#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Foo (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Foo v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Foo foo_sixty_four4 foo_thirty_two5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) foo_sixty_four4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) foo_thirty_two5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Foo (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Foo v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Foo foo_sixty_four4 foo_thirty_two5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) foo_sixty_four4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) foo_thirty_two5 s6

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_sixty_four" where

  type CFieldType Foo "foo_sixty_four" =
    HsBindgen.Runtime.LibC.Word64

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_sixty_four")
         ) => GHC.Records.HasField "foo_sixty_four" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_sixty_four")

instance HsBindgen.Runtime.HasCField.HasCField Foo "foo_thirty_two" where

  type CFieldType Foo "foo_thirty_two" =
    HsBindgen.Runtime.LibC.Word32

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Foo) "foo_thirty_two")
         ) => GHC.Records.HasField "foo_thirty_two" (Ptr.Ptr Foo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_thirty_two")
