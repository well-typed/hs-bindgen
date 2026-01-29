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
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/name\/squash_typedef.h 1:16@

    __exported by:__ @binding-specs\/name\/squash_typedef.h@
-}
data Piyo = Piyo
  { piyo_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:26@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  , piyo_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:29@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Piyo where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Piyo
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"piyo_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peek (Data.Proxy.Proxy @"piyo_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Piyo piyo_x2 piyo_y3 ->
               HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"piyo_x") ptr0 piyo_x2
            >> HsBindgen.Runtime.HasCField.poke (Data.Proxy.Proxy @"piyo_y") ptr0 piyo_y3

instance Data.Primitive.Types.Prim Piyo where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Piyo (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Piyo v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Piyo piyo_x4 piyo_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) piyo_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) piyo_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Piyo (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Piyo v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Piyo piyo_x4 piyo_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) piyo_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) piyo_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Piyo "piyo_x" where

  type CFieldType Piyo "piyo_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Piyo) "piyo_x")
         ) => GHC.Records.HasField "piyo_x" (Ptr.Ptr Piyo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"piyo_x")

instance HsBindgen.Runtime.HasCField.HasCField Piyo "piyo_y" where

  type CFieldType Piyo "piyo_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Piyo) "piyo_y")
         ) => GHC.Records.HasField "piyo_y" (Ptr.Ptr Piyo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"piyo_y")
