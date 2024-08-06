{-# LANGUAGE CPP #-}

-- | Shim to provide stack support
module HsBindgen.Patterns.Stack (
    Stack
  , prettyStack
  , getStack
  , ContainsStack(..)
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

newtype Stack = WrapStack {
      unwrapStack :: Backtraces
    }

instance Show Stack where
  show = prettyStack

prettyStack :: Stack -> String
prettyStack = displayBacktraces . unwrapStack

getStack :: HasCallStack => IO Stack
getStack = WrapStack <$> collectBacktraces

#else

-- For older ghc (< 9.10), we just use the 'CallStack'.

newtype Stack = WrapStack {
      unwrapStack :: CallStack
    }

instance Show Stack where
  show = prettyStack

prettyStack :: Stack -> String
prettyStack = prettyCallStack . unwrapStack

getStack :: HasCallStack => IO Stack
getStack = return $ WrapStack callStack

#endif

{-------------------------------------------------------------------------------
  Avoid duplicate backtraces
-------------------------------------------------------------------------------}

-- | Newtype for deriving-via for exceptions that contain explicit stacks
--
-- In ghc 9.10 and higher, 'throwIO' will include a backtrace immediately, but
-- this is not true for older versions. It is therefore useful to include an
-- explicit stack in exceptions, but if we do, we should then /also/ have
-- @ghc@'s stack annotation. Example usage:
--
-- > data CallFailed = CallFailed Stack
-- >   deriving stock (Show)
newtype ContainsStack a = ContainsStack a
  deriving newtype Show

instance (Show a, Typeable a) => Exception (ContainsStack a) where
#if MIN_VERSION_base(4,20,0)
    backtraceDesired _ = False
#endif
