{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ModuleNames.Same_basename
    ( ModuleNames.Same_basename.sAME_BASENAME_MAX
    , ModuleNames.Same_basename.Same_basename(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified ModuleNames.Nested.Same_basename
import Prelude (Eq, Ord, Show, type (~))

{-| __C declaration:__ @macro SAME_BASENAME_MAX@

    __defined at:__ @..\/same_basename.h 4:9@

    __exported by:__ @module_names.h@
-}
sAME_BASENAME_MAX :: BG.CInt
sAME_BASENAME_MAX = (4096 :: BG.CInt)

{-| __C declaration:__ @same_basename@

    __defined at:__ @..\/same_basename.h 9:31@

    __exported by:__ @module_names.h@
-}
newtype Same_basename = Same_basename
  { unwrapSame_basename :: BG.Ptr ModuleNames.Nested.Same_basename.Same_basename
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.Ptr ModuleNames.Nested.Same_basename.Same_basename
         ) => BG.CompatHasField.HasField "unwrapSame_basename" Same_basename ty where

  hasField =
    \x0 ->
      ( \y1 -> Same_basename {unwrapSame_basename = y1}
      , BG.getField @"unwrapSame_basename" x0
      )

instance ( ty ~ BG.Ptr ModuleNames.Nested.Same_basename.Same_basename
         ) => BG.HasField "unwrapSame_basename" (BG.Ptr Same_basename) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSame_basename")

instance HasCField.HasCField Same_basename "unwrapSame_basename" where

  type CFieldType Same_basename "unwrapSame_basename" =
    BG.Ptr ModuleNames.Nested.Same_basename.Same_basename

  offset# = \_ -> \_ -> 0
