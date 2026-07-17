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
    ( Example.Triplet(..)
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @triplet@

    __defined at:__ @documentation\/data_kind_pragma.h 3:13@

    __exported by:__ @documentation\/data_kind_pragma.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: CA.ConstantArray 3 BG.CInt
  }
  deriving stock (Eq, BG.Generic, Show)
  deriving newtype
    ( IsA.IsArray
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.CompatHasField.HasField "unwrapTriplet" Triplet ty where

  hasField =
    \x0 ->
      (\y1 ->
         Triplet {unwrapTriplet = y1}, BG.getField @"unwrapTriplet" x0)

instance ( ty ~ CA.ConstantArray 3 BG.CInt
         ) => BG.HasField "unwrapTriplet" (BG.Ptr Triplet) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTriplet")

instance HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    CA.ConstantArray 3 BG.CInt

  offset# = \_ -> \_ -> 0
