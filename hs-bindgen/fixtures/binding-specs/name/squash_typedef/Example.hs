{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/name\/squash_typedef.h 1:16@

    __exported by:__ @binding-specs\/name\/squash_typedef.h@
-}
data Piyo = Piyo
  { piyo_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:26@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  , piyo_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:29@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Piyo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Piyo where

  readRaw =
    \ptr0 ->
          pure Piyo
      <*> HasCField.readRaw (RIP.Proxy @"piyo_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"piyo_y") ptr0

instance Marshal.WriteRaw Piyo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Piyo piyo_x2 piyo_y3 ->
               HasCField.writeRaw (RIP.Proxy @"piyo_x") ptr0 piyo_x2
            >> HasCField.writeRaw (RIP.Proxy @"piyo_y") ptr0 piyo_y3

deriving via Marshal.EquivStorable Piyo instance RIP.Storable Piyo

instance HasCField.HasCField Piyo "piyo_x" where

  type CFieldType Piyo "piyo_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "piyo_x" (RIP.Ptr Piyo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"piyo_x")

instance HasCField.HasCField Piyo "piyo_y" where

  type CFieldType Piyo "piyo_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "piyo_y" (RIP.Ptr Piyo) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"piyo_y")
