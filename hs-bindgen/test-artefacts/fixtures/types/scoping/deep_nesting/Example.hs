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
    ( Example.Baz(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct baz@

    __defined at:__ @types\/scoping\/deep_nesting.h 18:12@

    __exported by:__ @types\/scoping\/deep_nesting.h@
-}
data Baz = Baz
  { baz_x1_2_1 :: RIP.CInt
    {- ^ __C declaration:__ @x1_2_1@

         __defined at:__ @types\/scoping\/deep_nesting.h 19:11@

         __exported by:__ @types\/scoping\/deep_nesting.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Baz where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Baz where

  readRaw =
    \ptr0 ->
          pure Baz
      <*> HasCField.readRaw (RIP.Proxy @"baz_x1_2_1") ptr0

instance Marshal.WriteRaw Baz where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_x1_2_12 ->
            HasCField.writeRaw (RIP.Proxy @"baz_x1_2_1") ptr0 baz_x1_2_12

deriving via Marshal.EquivStorable Baz instance RIP.Storable Baz

instance HasCField.HasCField Baz "baz_x1_2_1" where

  type CFieldType Baz "baz_x1_2_1" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "baz_x1_2_1" (RIP.Ptr Baz) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"baz_x1_2_1")
