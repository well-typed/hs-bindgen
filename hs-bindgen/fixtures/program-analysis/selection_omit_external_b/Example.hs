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
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct Omitted@

    __defined at:__ @selection_omit_external_root.h:1:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data Omitted = Omitted
  { omitted_n :: FC.CInt
    {- ^ __C declaration:__ @n@

         __defined at:__ @selection_omit_external_root.h:2:7@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Omitted where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Omitted
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"omitted_n") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Omitted omitted_n2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"omitted_n") ptr0 omitted_n2

instance Data.Primitive.Types.Prim Omitted where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Omitted (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Omitted v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Omitted omitted_n4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 omitted_n4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Omitted (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Omitted v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Omitted omitted_n4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 omitted_n4 s3

instance HsBindgen.Runtime.HasCField.HasCField Omitted "omitted_n" where

  type CFieldType Omitted "omitted_n" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Omitted) "omitted_n")
         ) => GHC.Records.HasField "omitted_n" (Ptr.Ptr Omitted) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"omitted_n")

{-| __C declaration:__ @struct DirectlyDependsOnOmitted@

    __defined at:__ @program-analysis\/selection_omit_external_b.h:4:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data DirectlyDependsOnOmitted = DirectlyDependsOnOmitted
  { directlyDependsOnOmitted_o :: Omitted
    {- ^ __C declaration:__ @o@

         __defined at:__ @program-analysis\/selection_omit_external_b.h:5:18@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable DirectlyDependsOnOmitted where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure DirectlyDependsOnOmitted
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"directlyDependsOnOmitted_o") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          DirectlyDependsOnOmitted directlyDependsOnOmitted_o2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"directlyDependsOnOmitted_o") ptr0 directlyDependsOnOmitted_o2

instance Data.Primitive.Types.Prim DirectlyDependsOnOmitted where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        DirectlyDependsOnOmitted (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, DirectlyDependsOnOmitted v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              DirectlyDependsOnOmitted directlyDependsOnOmitted_o4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 directlyDependsOnOmitted_o4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        DirectlyDependsOnOmitted (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, DirectlyDependsOnOmitted v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              DirectlyDependsOnOmitted directlyDependsOnOmitted_o4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 directlyDependsOnOmitted_o4 s3

instance HsBindgen.Runtime.HasCField.HasCField DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" where

  type CFieldType DirectlyDependsOnOmitted "directlyDependsOnOmitted_o" =
    Omitted

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DirectlyDependsOnOmitted) "directlyDependsOnOmitted_o")
         ) => GHC.Records.HasField "directlyDependsOnOmitted_o" (Ptr.Ptr DirectlyDependsOnOmitted) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"directlyDependsOnOmitted_o")

{-| __C declaration:__ @struct IndirectlyDependsOnOmitted@

    __defined at:__ @program-analysis\/selection_omit_external_b.h:8:8@

    __exported by:__ @program-analysis\/selection_omit_external_b.h@
-}
data IndirectlyDependsOnOmitted = IndirectlyDependsOnOmitted
  { indirectlyDependsOnOmitted_d :: DirectlyDependsOnOmitted
    {- ^ __C declaration:__ @d@

         __defined at:__ @program-analysis\/selection_omit_external_b.h:9:35@

         __exported by:__ @program-analysis\/selection_omit_external_b.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable IndirectlyDependsOnOmitted where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure IndirectlyDependsOnOmitted
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"indirectlyDependsOnOmitted_d") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          IndirectlyDependsOnOmitted indirectlyDependsOnOmitted_d2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"indirectlyDependsOnOmitted_d") ptr0 indirectlyDependsOnOmitted_d2

instance Data.Primitive.Types.Prim IndirectlyDependsOnOmitted where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        IndirectlyDependsOnOmitted (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) ->
              (# s3, IndirectlyDependsOnOmitted v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              IndirectlyDependsOnOmitted indirectlyDependsOnOmitted_d4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 indirectlyDependsOnOmitted_d4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        IndirectlyDependsOnOmitted (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) ->
              (# s3, IndirectlyDependsOnOmitted v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              IndirectlyDependsOnOmitted indirectlyDependsOnOmitted_d4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 indirectlyDependsOnOmitted_d4 s3

instance HsBindgen.Runtime.HasCField.HasCField IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" where

  type CFieldType IndirectlyDependsOnOmitted "indirectlyDependsOnOmitted_d" =
    DirectlyDependsOnOmitted

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType IndirectlyDependsOnOmitted) "indirectlyDependsOnOmitted_d")
         ) => GHC.Records.HasField "indirectlyDependsOnOmitted_d" (Ptr.Ptr IndirectlyDependsOnOmitted) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"indirectlyDependsOnOmitted_d")
