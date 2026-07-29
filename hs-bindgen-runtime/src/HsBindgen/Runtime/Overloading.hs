{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}

-- | Restore the default environment when using @RebindableSyntax@
--
-- The @RebindableSyntax@ extension is currently required when using
-- @OverloadedRecordUpdate@, but when using this extension, a number of
-- functions are suddenly no longer in scope that normally are. This module
-- restores those functions to their standard definition.
module HsBindgen.Runtime.Overloading (
    module Prelude
  , Control.Arrow.app
  , Control.Arrow.arr
  , Control.Arrow.first
  , Control.Arrow.loop
  , (Control.Arrow.>>>)
  , (Control.Arrow.|||)
  , Data.String.fromString
  , GHC.OverloadedLabels.fromLabel
  , GHC.Records.getField
    -- * New definitions
  , ifThenElse
  , setField
  ) where

import Control.Arrow qualified
import Data.String qualified
import GHC.OverloadedLabels qualified
import GHC.Records qualified
import GHC.Records.Compat qualified

{-------------------------------------------------------------------------------
  Other exports
-------------------------------------------------------------------------------}

ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y = if b then x else y

-- | Set a field in a record.
--
-- NOTE: the order of arguments is GHC version dependent.
#if __GLASGOW_HASKELL__ >=914
setField :: forall x r a. GHC.Records.Compat.HasField x r a => a -> r -> r
setField = flip (fst . GHC.Records.Compat.hasField @x)
#else
setField :: forall x r a. GHC.Records.Compat.HasField x r a => r -> a -> r
setField = fst . GHC.Records.Compat.hasField @x
#endif
