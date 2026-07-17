{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Matrix(..)
    , Example.Triplets(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @matrix@

    __defined at:__ @arrays\/multi_dim.h 12:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapMatrix" Matrix ty where

  hasField =
    \x0 ->
      (\y1 ->
         Matrix {unwrapMatrix = y1}, BG.getField @"unwrapMatrix" x0)

instance ( ty ~ CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)
         ) => BG.HasField "unwrapMatrix" (BG.Ptr Matrix) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapMatrix")

instance HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @triplets@

    __defined at:__ @arrays\/multi_dim.h 17:13@

    __exported by:__ @arrays\/multi_dim.h@
-}
newtype Triplets = Triplets
  { unwrapTriplets :: IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype (IsA.IsArray)

instance ( ty ~ IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapTriplets" Triplets ty where

  hasField =
    \x0 ->
      (\y1 ->
         Triplets {unwrapTriplets = y1}, BG.getField @"unwrapTriplets" x0)

instance ( ty ~ IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)
         ) => BG.HasField "unwrapTriplets" (BG.Ptr Triplets) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTriplets")

instance HasCField.HasCField Triplets "unwrapTriplets" where

  type CFieldType Triplets "unwrapTriplets" =
    IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)

  offset# = \_ -> \_ -> 0
