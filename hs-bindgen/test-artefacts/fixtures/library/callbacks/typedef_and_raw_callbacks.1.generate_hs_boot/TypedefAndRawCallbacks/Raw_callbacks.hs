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

module TypedefAndRawCallbacks.Raw_callbacks
    ( TypedefAndRawCallbacks.Raw_callbacks.Loop(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified TypedefAndRawCallbacks.Typedef_callback
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure, type (~))

{-| __C declaration:__ @struct loop@

    __defined at:__ @raw_callbacks.h 8:8@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
data Loop = Loop
  { loop_typedef_field :: TypedefAndRawCallbacks.Typedef_callback.Typedef_callback
    {- ^ __C declaration:__ @typedef_field@

         __defined at:__ @raw_callbacks.h 9:20@

         __exported by:__ @typedef_and_raw_callbacks.h@
    -}
  , loop_raw_field :: BG.FunPtr (BG.Ptr Loop -> IO ())
    {- ^ __C declaration:__ @raw_field@

         __defined at:__ @raw_callbacks.h 10:10@

         __exported by:__ @typedef_and_raw_callbacks.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Loop where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Loop where

  readRaw =
    \ptr0 ->
          pure Loop
      <*> HasCField.readRaw (BG.Proxy @"loop_typedef_field") ptr0
      <*> HasCField.readRaw (BG.Proxy @"loop_raw_field") ptr0

instance Marshal.WriteRaw Loop where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Loop loop_typedef_field2 loop_raw_field3 ->
               HasCField.writeRaw (BG.Proxy @"loop_typedef_field") ptr0 loop_typedef_field2
            >> HasCField.writeRaw (BG.Proxy @"loop_raw_field") ptr0 loop_raw_field3

deriving via Marshal.EquivStorable Loop instance BG.Storable Loop

deriving via Struct.IsStructViaReadRaw Loop instance Struct.IsStruct Loop

{-| __C declaration:__ @typedef_field@

    __defined at:__ @raw_callbacks.h 9:20@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
instance ( ty ~ TypedefAndRawCallbacks.Typedef_callback.Typedef_callback
         ) => BG.CompatHasField.HasField "loop_typedef_field" Loop ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Loop {loop_typedef_field = y1, loop_raw_field = BG.getField @"loop_raw_field" x0}
      , BG.getField @"loop_typedef_field" x0
      )

instance ( ty ~ TypedefAndRawCallbacks.Typedef_callback.Typedef_callback
         ) => BG.HasField "loop_typedef_field" (BG.Ptr Loop) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"loop_typedef_field")

instance HasCField.HasCField Loop "loop_typedef_field" where

  type CFieldType Loop "loop_typedef_field" =
    TypedefAndRawCallbacks.Typedef_callback.Typedef_callback

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @raw_field@

    __defined at:__ @raw_callbacks.h 10:10@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Loop -> IO ())
         ) => BG.CompatHasField.HasField "loop_raw_field" Loop ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Loop {loop_raw_field = y1, loop_typedef_field = BG.getField @"loop_typedef_field" x0}
      , BG.getField @"loop_raw_field" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Loop -> IO ())
         ) => BG.HasField "loop_raw_field" (BG.Ptr Loop) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"loop_raw_field")

instance HasCField.HasCField Loop "loop_raw_field" where

  type CFieldType Loop "loop_raw_field" =
    BG.FunPtr (BG.Ptr Loop -> IO ())

  offset# = \_ -> \_ -> 8
