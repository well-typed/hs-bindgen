{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.F(..)
    , pattern Example.F_A
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, type (~))

{-| __C declaration:__ @enum F@

    __defined at:__ @program-analysis\/program-slicing\/enum_unselected.h 14:6@

    __exported by:__ @program-analysis\/program-slicing\/enum_unselected.h@
-}
newtype F = F
  { unwrapF :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize F where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw F where

  readRaw =
    \ptr0 ->
          pure F
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw F where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          F unwrapF2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapF2

deriving via Marshal.EquivStorable F instance BG.Storable F

deriving via BG.CUInt instance BG.Prim F

instance CEnum.CEnum F where

  type CEnumZ F = BG.CUInt

  toCEnum = F

  fromCEnum = BG.getField @"unwrapF"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "F_A")]

  showsUndeclared = CEnum.showsWrappedUndeclared "F"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "F"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum F where

  minDeclaredValue = F_A

  maxDeclaredValue = F_A

instance Show F where

  showsPrec = CEnum.shows

instance Read F where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrapF" F ty where

  hasField =
    \x0 ->
      (\y1 -> F {unwrapF = y1}, BG.getField @"unwrapF" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrapF" (BG.Ptr F) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrapF")

instance HasCField.HasCField F "unwrapF" where

  type CFieldType F "unwrapF" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @F_A@

    __defined at:__ @program-analysis\/program-slicing\/enum_unselected.h 14:10@

    __exported by:__ @program-analysis\/program-slicing\/enum_unselected.h@
-}
pattern F_A :: F
pattern F_A = F 0
