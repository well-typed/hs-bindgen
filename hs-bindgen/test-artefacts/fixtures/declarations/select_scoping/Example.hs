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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.ParsedAndSelected1(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @ParsedAndSelected1@

    __defined at:__ @declarations\/select_scoping.h 6:13@

    __exported by:__ @declarations\/select_scoping.h@
-}
newtype ParsedAndSelected1 = ParsedAndSelected1
  { unwrapParsedAndSelected1 :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapParsedAndSelected1" ParsedAndSelected1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          ParsedAndSelected1 {unwrapParsedAndSelected1 = y1}
      , BG.getField @"unwrapParsedAndSelected1" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapParsedAndSelected1" (BG.Ptr ParsedAndSelected1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapParsedAndSelected1")

instance HasCField.HasCField ParsedAndSelected1 "unwrapParsedAndSelected1" where

  type CFieldType ParsedAndSelected1 "unwrapParsedAndSelected1" =
    BG.CInt

  offset# = \_ -> \_ -> 0
