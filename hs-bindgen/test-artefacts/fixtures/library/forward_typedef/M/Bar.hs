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

module M.Bar
    ( M.Bar.Bar(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct bar@

    __defined at:__ @bar.h 5:8@

    __exported by:__ @root.h@
-}
data Bar = Bar
  { bar_length :: BG.CInt
    {- ^ __C declaration:__ @length@

         __defined at:__ @bar.h 5:18@

         __exported by:__ @root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (BG.Proxy @"bar_length") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_length2 ->
            HasCField.writeRaw (BG.Proxy @"bar_length") ptr0 bar_length2

deriving via Marshal.EquivStorable Bar instance BG.Storable Bar

deriving via Struct.IsStructViaReadRaw Bar instance Struct.IsStruct Bar

{-| __C declaration:__ @length@

    __defined at:__ @bar.h 5:18@

    __exported by:__ @root.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "bar_length" Bar ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bar {bar_length = y1}, BG.getField @"bar_length" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "bar_length" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_length")

instance HasCField.HasCField Bar "bar_length" where

  type CFieldType Bar "bar_length" = BG.CInt

  offset# = \_ -> \_ -> 0
