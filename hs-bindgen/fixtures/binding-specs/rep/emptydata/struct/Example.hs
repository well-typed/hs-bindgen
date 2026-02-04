{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

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
  { named_e :: FC.CInt
    {- ^ __C declaration:__ @e@

         __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 27:11@

         __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
    -}
  , named_f :: FC.CInt
    {- ^ __C declaration:__ @f@

         __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 27:14@

         __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Named where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Named where

  readRaw =
    \ptr0 ->
          pure Named
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"named_e") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"named_f") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Named where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Named named_e2 named_f3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"named_e") ptr0 named_e2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"named_f") ptr0 named_f3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Named instance F.Storable Named

instance HsBindgen.Runtime.HasCField.HasCField Named "named_e" where

  type CFieldType Named "named_e" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named) "named_e")
         ) => GHC.Records.HasField "named_e" (Ptr.Ptr Named) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"named_e")

instance HsBindgen.Runtime.HasCField.HasCField Named "named_f" where

  type CFieldType Named "named_f" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Named) "named_f")
         ) => GHC.Records.HasField "named_f" (Ptr.Ptr Named) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"named_f")

{-| __C declaration:__ @struct oan@

    __defined at:__ @binding-specs\/rep\/emptydata\/struct.h 24:8@

    __exported by:__ @binding-specs\/rep\/emptydata\/struct.h@
-}
data Oan
