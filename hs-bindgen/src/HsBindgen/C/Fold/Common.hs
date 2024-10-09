module HsBindgen.C.Fold.Common (
    -- * Predicates
    Skipped(..)
  , checkPredicate
    -- * Errors
  , UnrecognizedCursor(..)
  , UnrecognizedType(..)
  , unrecognizedCursor
  , unrecognizedType
    -- * Simple folds
  , recurse
  , continue
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Tree
import GHC.Stack

import HsBindgen.C.AST
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Clang.Core
import HsBindgen.Clang.Util.Fold
import HsBindgen.Clang.Util.SourceLoc qualified as SourceLoc
import HsBindgen.Patterns
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Predicates
-------------------------------------------------------------------------------}

data Skipped = Skipped {
      skippedName   :: Text
    , skippedLoc    :: MultiLoc
    , skippedReason :: String
    }

instance PrettyLogMsg Skipped where
  prettyLogMsg Skipped{skippedName, skippedLoc, skippedReason} = concat [
        "Skipped "
      , show skippedName
      , " at "
      , show skippedLoc
      , ": "
      , skippedReason
      ]

checkPredicate :: Tracer IO Skipped -> Predicate -> Fold m a -> Fold m a
checkPredicate tracer p k current = do
    isMatch <- liftIO $ Predicate.match current p
    case isMatch of
      Right ()     -> k current
      Left  reason -> liftIO $ do
        name <- clang_getCursorSpelling current
        loc  <- SourceLoc.clang_getCursorLocation current
        traceWith tracer Info $ Skipped name loc reason
        return $ Continue Nothing

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data UnrecognizedCursor = UnrecognizedCursor {
      unrecognizedCursorKind  :: SimpleEnum CXCursorKind
    , unrecognizedCursorLoc   :: MultiLoc
    , unrecognizedCursorTrace :: Backtrace
    }
  deriving stock (Show)
  deriving Exception via CollectedBacktrace UnrecognizedCursor

data UnrecognizedType = UnrecognizedType {
      unrecognizedTypeKind  :: SimpleEnum CXTypeKind
    , unrecognizedTypeTrace :: Backtrace
    }
  deriving stock (Show)
  deriving Exception via CollectedBacktrace UnrecognizedType

unrecognizedCursor :: (MonadIO m, HasCallStack) => CXCursor -> m a
unrecognizedCursor cursor = liftIO $ do
    unrecognizedCursorKind  <- clang_getCursorKind cursor
    unrecognizedCursorLoc   <- SourceLoc.clang_getCursorLocation cursor
    unrecognizedCursorTrace <- collectBacktrace
    throwIO UnrecognizedCursor{
        unrecognizedCursorKind
      , unrecognizedCursorLoc
      , unrecognizedCursorTrace
      }

unrecognizedType :: (MonadIO m, HasCallStack) => CXType -> m a
unrecognizedType typ = liftIO $ do
    let unrecognizedTypeKind = cxtKind typ
    unrecognizedTypeTrace <- collectBacktrace
    throwIO UnrecognizedType{
        unrecognizedTypeKind
      , unrecognizedTypeTrace
      }

{-------------------------------------------------------------------------------
  Simple folds
-------------------------------------------------------------------------------}

recurse :: forall m a. (CXCursor -> FoldM m a) -> Fold m (Tree a)
recurse f = go
  where
    go :: Fold m (Tree a)
    go current = f current >>= \x -> return (Recurse go $ Just . Node x)

continue :: (CXCursor -> FoldM m a) -> Fold m a
continue f current = f current >>= \x -> return (Continue $ Just x)
