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
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @matrix@

    __defined at:__ @arrays\/multi_dim.h 12:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
         ) => RIP.HasField "unwrapMatrix" (RIP.Ptr Matrix) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMatrix")

instance HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @triplets@

    __defined at:__ @arrays\/multi_dim.h 17:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Triplets = Triplets
  { unwrapTriplets :: IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)
  }
  deriving stock (Eq, RIP.Generic, Show)

instance ( ((~) ty) (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
         ) => RIP.HasField "unwrapTriplets" (RIP.Ptr Triplets) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTriplets")

instance HasCField.HasCField Triplets "unwrapTriplets" where

  type CFieldType Triplets "unwrapTriplets" =
    IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)

  offset# = \_ -> \_ -> 0
