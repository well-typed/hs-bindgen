{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Marshal
import Prelude (Eq, Show)

{-| __C declaration:__ @matrix@

    __defined at:__ @arrays\/multi_dim.h 12:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapMatrix" (Ptr.Ptr Matrix) (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMatrix")

instance HsBindgen.Runtime.HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @triplets@

    __defined at:__ @arrays\/multi_dim.h 17:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Triplets = Triplets
  { unwrapTriplets :: HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance GHC.Records.HasField "unwrapTriplets" (Ptr.Ptr Triplets) (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTriplets")

instance HsBindgen.Runtime.HasCField.HasCField Triplets "unwrapTriplets" where

  type CFieldType Triplets "unwrapTriplets" =
    HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

  offset# = \_ -> \_ -> 0
