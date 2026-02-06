{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import qualified M
import Prelude (Eq, Show)

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 6:13@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
newtype A = A
  { unwrapA :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr A) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array_known_size.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (GHC.Generics.Generic)

instance GHC.Records.HasField "unwrapE" (Ptr.Ptr E) (Ptr.Ptr M.C) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapE")

instance HsBindgen.Runtime.HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
