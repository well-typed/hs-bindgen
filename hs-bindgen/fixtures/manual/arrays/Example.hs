{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude (Eq, Show)

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/arrays.h 32:13@

    __exported by:__ @manual\/arrays.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplet) "unwrapTriplet")
         ) => GHC.Records.HasField "unwrapTriplet" (Ptr.Ptr Triplet) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTriplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @manual\/arrays.h 34:17@

    __exported by:__ @manual\/arrays.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Matrix) "unwrapMatrix")
         ) => GHC.Records.HasField "unwrapMatrix" (Ptr.Ptr Matrix) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMatrix")

instance HsBindgen.Runtime.HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet

  offset# = \_ -> \_ -> 0

{-| A typedef representing a an array of unknown size, where each element is a pointer to an array of known size 3, where each element is an int.

__C declaration:__ @triplet_ptrs@

__defined at:__ @manual\/arrays.h 44:15@

__exported by:__ @manual\/arrays.h@
-}
newtype Triplet_ptrs = Triplet_ptrs
  { unwrapTriplet_ptrs :: HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  }
  deriving stock (Eq, Show)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplet_ptrs) "unwrapTriplet_ptrs")
         ) => GHC.Records.HasField "unwrapTriplet_ptrs" (Ptr.Ptr Triplet_ptrs) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTriplet_ptrs")

instance HsBindgen.Runtime.HasCField.HasCField Triplet_ptrs "unwrapTriplet_ptrs" where

  type CFieldType Triplet_ptrs "unwrapTriplet_ptrs" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

  offset# = \_ -> \_ -> 0
