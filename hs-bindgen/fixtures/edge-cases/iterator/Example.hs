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
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.Block
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Prelude (IO)

{-| __C declaration:__ @Toggle@

    __defined at:__ @edge-cases\/iterator.h 3:16@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Toggle = Toggle
  { unwrapToggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance GHC.Records.HasField "unwrapToggle" (Ptr.Ptr Toggle) (Ptr.Ptr (HsBindgen.Runtime.Block.Block (IO FC.CBool))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapToggle")

instance HsBindgen.Runtime.HasCField.HasCField Toggle "unwrapToggle" where

  type CFieldType Toggle "unwrapToggle" =
    HsBindgen.Runtime.Block.Block (IO FC.CBool)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Counter@

    __defined at:__ @edge-cases\/iterator.h 10:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Counter = Counter
  { unwrapCounter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance GHC.Records.HasField "unwrapCounter" (Ptr.Ptr Counter) (Ptr.Ptr (HsBindgen.Runtime.Block.Block (IO FC.CInt))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapCounter")

instance HsBindgen.Runtime.HasCField.HasCField Counter "unwrapCounter" where

  type CFieldType Counter "unwrapCounter" =
    HsBindgen.Runtime.Block.Block (IO FC.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VarCounter@

    __defined at:__ @edge-cases\/iterator.h 17:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype VarCounter = VarCounter
  { unwrapVarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance GHC.Records.HasField "unwrapVarCounter" (Ptr.Ptr VarCounter) (Ptr.Ptr (HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt))) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapVarCounter")

instance HsBindgen.Runtime.HasCField.HasCField VarCounter "unwrapVarCounter" where

  type CFieldType VarCounter "unwrapVarCounter" =
    HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)

  offset# = \_ -> \_ -> 0
