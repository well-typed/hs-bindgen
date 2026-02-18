{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.Block as Block
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

{-| __C declaration:__ @Toggle@

    __defined at:__ @edge-cases\/iterator.h 3:16@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Toggle = Toggle
  { unwrapToggle :: Block.Block (IO RIP.CBool)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ((~) ty) (Block.Block (IO RIP.CBool))
         ) => RIP.HasField "unwrapToggle" (RIP.Ptr Toggle) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapToggle")

instance HasCField.HasCField Toggle "unwrapToggle" where

  type CFieldType Toggle "unwrapToggle" =
    Block.Block (IO RIP.CBool)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @Counter@

    __defined at:__ @edge-cases\/iterator.h 10:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype Counter = Counter
  { unwrapCounter :: Block.Block (IO RIP.CInt)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ((~) ty) (Block.Block (IO RIP.CInt))
         ) => RIP.HasField "unwrapCounter" (RIP.Ptr Counter) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapCounter")

instance HasCField.HasCField Counter "unwrapCounter" where

  type CFieldType Counter "unwrapCounter" =
    Block.Block (IO RIP.CInt)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @VarCounter@

    __defined at:__ @edge-cases\/iterator.h 17:14@

    __exported by:__ @edge-cases\/iterator.h@
-}
newtype VarCounter = VarCounter
  { unwrapVarCounter :: Block.Block (RIP.CInt -> IO RIP.CInt)
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ((~) ty) (Block.Block (RIP.CInt -> IO RIP.CInt))
         ) => RIP.HasField "unwrapVarCounter" (RIP.Ptr VarCounter) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapVarCounter")

instance HasCField.HasCField VarCounter "unwrapVarCounter" where

  type CFieldType VarCounter "unwrapVarCounter" =
    Block.Block (RIP.CInt -> IO RIP.CInt)

  offset# = \_ -> \_ -> 0
