{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.B(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified M

{-| __C declaration:__ @macro B@

    __defined at:__ @macros\/macro_ext_binding_dep.h 6:9@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
newtype B = B
  { unwrapB :: M.A
  }
  deriving stock (RIP.Generic)

instance (ty ~ M.A) => RIP.CompatHasField.HasField "unwrapB" B ty where

  hasField =
    \x0 ->
      (\y1 -> B {unwrapB = y1}, RIP.getField @"unwrapB" x0)

instance (ty ~ M.A) => RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = M.A

  offset# = \_ -> \_ -> 0
