{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/name\/squash_typedef.h 1:16@

    __exported by:__ @binding-specs\/name\/squash_typedef.h@
-}
data Piyo = Piyo
  { piyo_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:26@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  , piyo_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @binding-specs\/name\/squash_typedef.h 1:29@

         __exported by:__ @binding-specs\/name\/squash_typedef.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Piyo where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Piyo where

  readRaw =
    \ptr0 ->
          pure Piyo
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"piyo_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"piyo_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Piyo where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Piyo piyo_x2 piyo_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"piyo_x") ptr0 piyo_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"piyo_y") ptr0 piyo_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Piyo instance F.Storable Piyo

instance HsBindgen.Runtime.HasCField.HasCField Piyo "piyo_x" where

  type CFieldType Piyo "piyo_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Piyo) "piyo_x")
         ) => GHC.Records.HasField "piyo_x" (Ptr.Ptr Piyo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"piyo_x")

instance HsBindgen.Runtime.HasCField.HasCField Piyo "piyo_y" where

  type CFieldType Piyo "piyo_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Piyo) "piyo_y")
         ) => GHC.Records.HasField "piyo_y" (Ptr.Ptr Piyo) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"piyo_y")
