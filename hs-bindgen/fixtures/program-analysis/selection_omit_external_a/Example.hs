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

{-| __C declaration:__ @struct UnrelatedDeclaration@

    __defined at:__ @program-analysis\/selection_omit_external_a.h 4:8@

    __exported by:__ @program-analysis\/selection_omit_external_a.h@
-}
data UnrelatedDeclaration = UnrelatedDeclaration
  { unrelatedDeclaration_m :: FC.CInt
    {- ^ __C declaration:__ @m@

         __defined at:__ @program-analysis\/selection_omit_external_a.h 5:7@

         __exported by:__ @program-analysis\/selection_omit_external_a.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable UnrelatedDeclaration where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure UnrelatedDeclaration
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"unrelatedDeclaration_m") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          UnrelatedDeclaration unrelatedDeclaration_m2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"unrelatedDeclaration_m") ptr0 unrelatedDeclaration_m2

instance Data.Primitive.Types.Prim UnrelatedDeclaration where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        UnrelatedDeclaration (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, UnrelatedDeclaration v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              UnrelatedDeclaration unrelatedDeclaration_m4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 unrelatedDeclaration_m4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        UnrelatedDeclaration (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, UnrelatedDeclaration v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              UnrelatedDeclaration unrelatedDeclaration_m4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 unrelatedDeclaration_m4 s3

instance HsBindgen.Runtime.HasCField.HasCField UnrelatedDeclaration "unrelatedDeclaration_m" where

  type CFieldType UnrelatedDeclaration "unrelatedDeclaration_m" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType UnrelatedDeclaration) "unrelatedDeclaration_m")
         ) => GHC.Records.HasField "unrelatedDeclaration_m" (Ptr.Ptr UnrelatedDeclaration) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"unrelatedDeclaration_m")
