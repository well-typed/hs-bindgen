module HsBindgen.Errors (
    HsBindgenException (..),
    hsBindgenExceptionToException,
    hsBindgenExceptionFromException,
    TODOException (..),
    throwPure_TODO,
    PanicException,
    panicPure,
    panicIO,
) where

import GHC.Stack (CallStack, callStack, prettyCallStack)
import Control.Exception (SomeException (..), Exception (..), throw)
import Data.Typeable (cast)
import HsBindgen.Imports

-------------------------------------------------------------------------------
-- HsBindgenException
-------------------------------------------------------------------------------

-- | Superclass for @hs-bindgen@ exceptions
data HsBindgenException where
    HsBindgenException :: Exception e => e -> HsBindgenException

instance Show HsBindgenException where
    showsPrec d (HsBindgenException e) = showsPrec d e

hsBindgenExceptionToException :: Exception e => e -> SomeException
hsBindgenExceptionToException = toException . HsBindgenException

hsBindgenExceptionFromException :: Exception e => SomeException -> Maybe e
hsBindgenExceptionFromException x = do
    HsBindgenException a <- fromException x
    cast a

instance Exception HsBindgenException where
    displayException (HsBindgenException e) = displayException e

-------------------------------------------------------------------------------
-- TODOs
-------------------------------------------------------------------------------

data TODOException = TODOException !CallStack !Int !String
  deriving Show

instance Exception TODOException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (TODOException cs issue msg) = unlines
        [ "hs-bindgen known issue: https://github.com/well-typed/hs-bindgen/issues/" ++ show issue
        , msg
        , prettyCallStack cs
        ]

-- | Throw a pure, known TODO exception.
throwPure_TODO :: HasCallStack => Int -> String -> a
throwPure_TODO issue msg = throw (TODOException callStack issue msg)

-------------------------------------------------------------------------------
-- Panics
-------------------------------------------------------------------------------

-- | Unexpected (e.g. invariant violation) conditions.
data PanicException = PanicException !CallStack !String
  deriving Show

instance Exception PanicException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException (PanicException cs  msg) = unlines
        [ "PANIC!: the impossible happened"
        , "Please report this as a bug at https://github.com/well-typed/hs-bindgen/issues/"
        , msg
        , prettyCallStack cs
        ]

-- | Panic in pure context
panicPure :: HasCallStack => String -> a
panicPure msg = throw (PanicException callStack msg)

-- | Panic in IO
panicIO :: (HasCallStack, MonadIO m) => String -> m a
panicIO msg = liftIO (throwIO (PanicException callStack msg))
