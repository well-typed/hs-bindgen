{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
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

{-| __C declaration:__ @struct rect@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 5:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Rect

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 9:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Foo

{-| __C declaration:__ @struct oaa@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 13:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Oaa

{-| __C declaration:__ @struct named@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 26:12@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Named = Named
  { named_e :: FC.CInt
    {- ^ __C declaration:__ @e@

         __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 27:11@

         __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
    -}
  , named_f :: FC.CInt
    {- ^ __C declaration:__ @f@

         __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 27:14@

         __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Named where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Named
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"named_e") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"named_f") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named named_e2 named_f3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"named_e") ptr0 named_e2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"named_f") ptr0 named_f3

instance Data.Primitive.Types.Prim Named where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Named (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Named v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Named named_e4 named_f5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) named_e4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) named_f5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Named (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Named v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Named named_e4 named_f5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) named_e4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) named_f5 s6

instance HsBindgen.Runtime.HasCField.HasCField Named "named_e" where

  type CFieldType Named "named_e" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named) "named_e")
         ) => GHC.Records.HasField "named_e" (Ptr.Ptr Named) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"named_e")

instance HsBindgen.Runtime.HasCField.HasCField Named "named_f" where

  type CFieldType Named "named_f" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named) "named_f")
         ) => GHC.Records.HasField "named_f" (Ptr.Ptr Named) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"named_f")

{-| __C declaration:__ @struct oan@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 24:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Oan
