{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/name\/squash_both.h 1:16@

    __exported by:__ @binding-specs\/name\/squash_both.h@
-}
data Hoge = Hoge
  { hoge_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/name\/squash_both.h 1:26@

         __exported by:__ @binding-specs\/name\/squash_both.h@
    -}
  , hoge_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/name\/squash_both.h 1:29@

         __exported by:__ @binding-specs\/name\/squash_both.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Hoge where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Hoge where

  readRaw =
    \ptr0 ->
          pure Hoge
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"hoge_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"hoge_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Hoge where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Hoge hoge_x2 hoge_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"hoge_x") ptr0 hoge_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"hoge_y") ptr0 hoge_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Hoge instance F.Storable Hoge

instance HsBindgen.Runtime.HasCField.HasCField Hoge "hoge_x" where

  type CFieldType Hoge "hoge_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "hoge_x" (Ptr.Ptr Hoge) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"hoge_x")

instance HsBindgen.Runtime.HasCField.HasCField Hoge "hoge_y" where

  type CFieldType Hoge "hoge_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "hoge_y" (Ptr.Ptr Hoge) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"hoge_y")
