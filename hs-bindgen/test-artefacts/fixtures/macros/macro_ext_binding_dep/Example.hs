{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.B(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M

{-| __C declaration:__ @macro B@

    __defined at:__ @macros\/macro_ext_binding_dep.h 6:9@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
newtype B = B
  { unwrapB :: M.A
  }
  deriving stock (RIP.Generic)

instance RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr M.A) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = M.A

  offset# = \_ -> \_ -> 0
