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

{-| __C declaration:__ @struct StructWithAssignedHaskellNameByPrescriptiveBindingSpecs@

    __defined at:__ @program-analysis\/selection_matches_c_names.h 7:8@

    __exported by:__ @program-analysis\/selection_matches_c_names.h@
-}
data NewName = NewName
  { newName_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @program-analysis\/selection_matches_c_names.h 8:7@

         __exported by:__ @program-analysis\/selection_matches_c_names.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable NewName where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure NewName
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"newName_x") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          NewName newName_x2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"newName_x") ptr0 newName_x2

instance Data.Primitive.Types.Prim NewName where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        NewName (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, NewName v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              NewName newName_x4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 newName_x4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        NewName (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, NewName v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              NewName newName_x4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 newName_x4 s3

instance HsBindgen.Runtime.HasCField.HasCField NewName "newName_x" where

  type CFieldType NewName "newName_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType NewName) "newName_x")
         ) => GHC.Records.HasField "newName_x" (Ptr.Ptr NewName) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"newName_x")
