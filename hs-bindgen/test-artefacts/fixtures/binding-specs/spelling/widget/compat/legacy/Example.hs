{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Spelling_user(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified Spelling.Core

{-| __C declaration:__ @struct spelling_user@

    __defined at:__ @binding-specs\/spelling\/widget\/compat\/legacy.h 10:8@

    __exported by:__ @binding-specs\/spelling\/widget\/compat\/legacy.h@
-}
data Spelling_user = Spelling_user
  { spelling_user_core :: Spelling.Core.Spelling_core
    {- ^ __C declaration:__ @core@

         __defined at:__ @binding-specs\/spelling\/widget\/compat\/legacy.h 11:24@

         __exported by:__ @binding-specs\/spelling\/widget\/compat\/legacy.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Spelling_user where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Spelling_user where

  readRaw =
    \ptr0 ->
          pure Spelling_user
      <*> HasCField.readRaw (BG.Proxy @"spelling_user_core") ptr0

instance Marshal.WriteRaw Spelling_user where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Spelling_user spelling_user_core2 ->
            HasCField.writeRaw (BG.Proxy @"spelling_user_core") ptr0 spelling_user_core2

deriving via Marshal.EquivStorable Spelling_user instance BG.Storable Spelling_user

deriving via Struct.IsStructViaReadRaw Spelling_user instance Struct.IsStruct Spelling_user

{-| __C declaration:__ @core@

    __defined at:__ @binding-specs\/spelling\/widget\/compat\/legacy.h 11:24@

    __exported by:__ @binding-specs\/spelling\/widget\/compat\/legacy.h@
-}
instance ( ty ~ Spelling.Core.Spelling_core
         ) => BG.CompatHasField.HasField "spelling_user_core" Spelling_user ty where

  hasField =
    \x0 ->
      ( \y1 -> Spelling_user {spelling_user_core = y1}
      , BG.getField @"spelling_user_core" x0
      )

instance ( ty ~ Spelling.Core.Spelling_core
         ) => BG.HasField "spelling_user_core" (BG.Ptr Spelling_user) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"spelling_user_core")

instance HasCField.HasCField Spelling_user "spelling_user_core" where

  type CFieldType Spelling_user "spelling_user_core" =
    Spelling.Core.Spelling_core

  offset# = \_ -> \_ -> 0
