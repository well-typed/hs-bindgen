{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/arrays.h 32:13@

    __exported by:__ @manual\/arrays.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapTriplet" (RIP.Ptr Triplet) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTriplet")

instance HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @manual\/arrays.h 34:17@

    __exported by:__ @manual\/arrays.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (CA.ConstantArray 3) Triplet
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) Triplet)
         ) => RIP.HasField "unwrapMatrix" (RIP.Ptr Matrix) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMatrix")

instance HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (CA.ConstantArray 3) Triplet

  offset# = \_ -> \_ -> 0

{-| A typedef representing a an array of unknown size, where each element is a pointer to an array of known size 3, where each element is an int.

__C declaration:__ @triplet_ptrs@

__defined at:__ @manual\/arrays.h 44:15@

__exported by:__ @manual\/arrays.h@
-}
newtype Triplet_ptrs = Triplet_ptrs
  { unwrapTriplet_ptrs :: IA.IncompleteArray (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ((~) ty) (IA.IncompleteArray (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)))
         ) => RIP.HasField "unwrapTriplet_ptrs" (RIP.Ptr Triplet_ptrs) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTriplet_ptrs")

instance HasCField.HasCField Triplet_ptrs "unwrapTriplet_ptrs" where

  type CFieldType Triplet_ptrs "unwrapTriplet_ptrs" =
    IA.IncompleteArray (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))

  offset# = \_ -> \_ -> 0
