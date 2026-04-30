module C.Expr.Util.Panic (
    panicPure
  , panicIO
  ) where

import Control.Exception
import GHC.Stack
import Control.Monad.IO.Class

-- | Unexpected (e.g. invariant violation) conditions.
data PanicException = PanicException !CallStack !String
  deriving Show

instance Exception PanicException where
    displayException (PanicException cs  msg) = unlines
        [ "PANIC!: the impossible happened"
        , pleaseReport
        , msg
        , prettyCallStack cs
        ]

-- TODO <https://github.com/well-typed/hs-bindgen/issues/943>
--
-- Amend when `c-expr-dsl` is separated from `hs-bindgen`.
pleaseReport :: String
pleaseReport = "Please report this as a bug at https://github.com/well-typed/hs-bindgen/issues/"

-- | Panic in pure context
panicPure :: HasCallStack => String -> a
panicPure msg = throw (PanicException callStack msg)

-- | Panic in IO
panicIO :: (HasCallStack, MonadIO m) => String -> m a
panicIO msg = liftIO (throwIO (PanicException callStack msg))
