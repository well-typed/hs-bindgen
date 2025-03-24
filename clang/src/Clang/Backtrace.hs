{-# LANGUAGE CPP #-}

-- | Shim to provide backtrace support
module Clang.Backtrace (
    Backtrace
  , prettyBacktrace
  , collectBacktrace
  , CollectedBacktrace(..)
  ) where

import Control.Exception
import Data.Typeable
import GHC.Stack

#if MIN_VERSION_base(4,20,0)
import Control.Exception.Backtrace
#endif

{-------------------------------------------------------------------------------
  Abstract over backtraces
-------------------------------------------------------------------------------}

#if MIN_VERSION_base(4,20,0)

-- Take advantage of the new backtrace support in ghc 9.10 and up.

newtype Backtrace = WrapStack {
      unwrapStack :: Backtraces
    }

instance Show Backtrace where
  show = prettyBacktrace

prettyBacktrace :: Backtrace -> String
prettyBacktrace = displayBacktraces . unwrapStack

collectBacktrace :: HasCallStack => IO Backtrace
collectBacktrace = WrapStack <$> collectBacktraces

#else

-- For older ghc (< 9.10), we just use the 'CallStack'.

newtype Backtrace = WrapStack {
      unwrapStack :: CallStack
    }

instance Show Backtrace where
  show = prettyBacktrace

prettyBacktrace :: Backtrace -> String
prettyBacktrace = prettyCallStack . unwrapStack

collectBacktrace :: HasCallStack => IO Backtrace
collectBacktrace = return $ WrapStack callStack

#endif

{-------------------------------------------------------------------------------
  Avoid duplicate backtraces
-------------------------------------------------------------------------------}

-- | Newtype for deriving-via for exceptions that contain explicit stacks
--
-- In ghc 9.10 and higher, 'throwIO' will include a backtrace immediately, but
-- this is not true for older versions. It is therefore useful to include an
-- explicit backtrace in exceptions, but if we do, we should then not /also/
-- have @ghc@'s automatic backtrace annotation. Example usage:
--
-- > data CallFailed = CallFailed Backtrace
-- >   deriving stock (Show)
-- >   deriving Exception via CollectedBacktrac CallFailed
newtype CollectedBacktrace a = CollectedBacktrace a
  deriving newtype Show

instance (Show a, Typeable a) => Exception (CollectedBacktrace a) where
#if MIN_VERSION_base(4,20,0)
  backtraceDesired _ = False
#endif
