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
    ( Example.Bar(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct bar@

    __defined at:__ @macros\/macro_typedef_struct.h 3:9@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
data Bar = Bar
  { bar_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/macro_typedef_struct.h 4:7@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  , bar_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @macros\/macro_typedef_struct.h 5:11@

         __exported by:__ @macros\/macro_typedef_struct.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar where

  readRaw =
    \ptr0 ->
          pure Bar
      <*> HasCField.readRaw (BG.Proxy @"bar_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"bar_y") ptr0

instance Marshal.WriteRaw Bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               HasCField.writeRaw (BG.Proxy @"bar_x") ptr0 bar_x2
            >> HasCField.writeRaw (BG.Proxy @"bar_y") ptr0 bar_y3

deriving via Marshal.EquivStorable Bar instance BG.Storable Bar

{-| __C declaration:__ @x@

    __defined at:__ @macros\/macro_typedef_struct.h 4:7@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "bar_x" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_x = y1, bar_y = BG.getField @"bar_y" x0}
      , BG.getField @"bar_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "bar_x" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_x")

instance HasCField.HasCField Bar "bar_x" where

  type CFieldType Bar "bar_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @macros\/macro_typedef_struct.h 5:11@

    __exported by:__ @macros\/macro_typedef_struct.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "bar_y" Bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar {bar_y = y1, bar_x = BG.getField @"bar_x" x0}
      , BG.getField @"bar_y" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "bar_y" (BG.Ptr Bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"bar_y")

instance HasCField.HasCField Bar "bar_y" where

  type CFieldType Bar "bar_y" = BG.CInt

  offset# = \_ -> \_ -> 4
