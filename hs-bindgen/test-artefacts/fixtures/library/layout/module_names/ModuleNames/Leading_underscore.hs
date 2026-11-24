{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ModuleNames.Leading_underscore
    ( ModuleNames.Leading_underscore.Leading_underscore(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct leading_underscore@

    __defined at:__ @module_names\/_leading_underscore.h 5:8@

    __exported by:__ @module_names.h@
-}
data Leading_underscore = Leading_underscore
  { leading_underscore_value :: BG.CInt
    {- ^ __C declaration:__ @value@

         __defined at:__ @module_names\/_leading_underscore.h 6:7@

         __exported by:__ @module_names.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Leading_underscore where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Leading_underscore where

  readRaw =
    \ptr0 ->
          pure Leading_underscore
      <*> HasCField.readRaw (BG.Proxy @"leading_underscore_value") ptr0

instance Marshal.WriteRaw Leading_underscore where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Leading_underscore leading_underscore_value2 ->
            HasCField.writeRaw (BG.Proxy @"leading_underscore_value") ptr0 leading_underscore_value2

deriving via Marshal.EquivStorable Leading_underscore instance BG.Storable Leading_underscore

deriving via Struct.IsStructViaReadRaw Leading_underscore instance Struct.IsStruct Leading_underscore

{-| __C declaration:__ @value@

    __defined at:__ @module_names\/_leading_underscore.h 6:7@

    __exported by:__ @module_names.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "leading_underscore_value" Leading_underscore ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Leading_underscore {leading_underscore_value = y1}
      , BG.getField @"leading_underscore_value" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "leading_underscore_value" (BG.Ptr Leading_underscore) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"leading_underscore_value")

instance HasCField.HasCField Leading_underscore "leading_underscore_value" where

  type CFieldType Leading_underscore "leading_underscore_value" =
    BG.CInt

  offset# = \_ -> \_ -> 0
