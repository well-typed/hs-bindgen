{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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

{-| __C declaration:__ @struct rect@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 5:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Rect

{-| __C declaration:__ @struct foo@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 9:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Foo

{-| __C declaration:__ @struct oaa@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 13:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Oaa

{-| __C declaration:__ @struct named@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 26:12@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Named = Named
  { named_e :: RIP.CInt
    {- ^ __C declaration:__ @e@

         __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 27:11@

         __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
    -}
  , named_f :: RIP.CInt
    {- ^ __C declaration:__ @f@

         __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 27:14@

         __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Named where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Named where

  readRaw =
    \ptr0 ->
          pure Named
      <*> HasCField.readRaw (RIP.Proxy @"named_e") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"named_f") ptr0

instance Marshal.WriteRaw Named where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named named_e2 named_f3 ->
               HasCField.writeRaw (RIP.Proxy @"named_e") ptr0 named_e2
            >> HasCField.writeRaw (RIP.Proxy @"named_f") ptr0 named_f3

deriving via Marshal.EquivStorable Named instance RIP.Storable Named

instance HasCField.HasCField Named "named_e" where

  type CFieldType Named "named_e" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "named_e" (RIP.Ptr Named) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"named_e")

instance HasCField.HasCField Named "named_f" where

  type CFieldType Named "named_f" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "named_f" (RIP.Ptr Named) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"named_f")

{-| __C declaration:__ @struct oan@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 24:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Oan
