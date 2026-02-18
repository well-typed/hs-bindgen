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

    __defined at:__ @binding-specs\/name\/squash_both.h 1:16@

    __exported by:__ @binding-specs\/name\/squash_both.h@
-}
data Hoge = Hoge
  { hoge_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/name\/squash_both.h 1:26@

         __exported by:__ @binding-specs\/name\/squash_both.h@
    -}
  , hoge_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/name\/squash_both.h 1:29@

         __exported by:__ @binding-specs\/name\/squash_both.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Hoge where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Hoge where

  readRaw =
    \ptr0 ->
          pure Hoge
      <*> HasCField.readRaw (RIP.Proxy @"hoge_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"hoge_y") ptr0

instance Marshal.WriteRaw Hoge where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Hoge hoge_x2 hoge_y3 ->
               HasCField.writeRaw (RIP.Proxy @"hoge_x") ptr0 hoge_x2
            >> HasCField.writeRaw (RIP.Proxy @"hoge_y") ptr0 hoge_y3

deriving via Marshal.EquivStorable Hoge instance RIP.Storable Hoge

instance HasCField.HasCField Hoge "hoge_x" where

  type CFieldType Hoge "hoge_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "hoge_x" (RIP.Ptr Hoge) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"hoge_x")

instance HasCField.HasCField Hoge "hoge_y" where

  type CFieldType Hoge "hoge_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "hoge_y" (RIP.Ptr Hoge) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"hoge_y")
