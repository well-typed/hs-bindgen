module HsBindgen.C.Fold.Common (
    -- * Root header
    rootHeaderName
  , rootHeaderContent
    -- * Predicates
  , Skipped(..)
  , whenPredicateMatches
    -- * Errors
  , UnrecognizedCursor(..)
  , UnrecognizedType(..)
  , unrecognizedCursor
  , unrecognizedType
    -- * Simple folds
  , recurse
  , continue
  ) where

import Data.Tree (Tree (Node))

import Clang.Backtrace
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Root header
-------------------------------------------------------------------------------}

rootHeaderName :: SourcePath
rootHeaderName = SourcePath "hs-bindgen-root.h"

rootHeaderContent :: [CHeaderIncludePath] -> String
rootHeaderContent = unlines . map toLine
  where
    toLine :: CHeaderIncludePath -> String
    toLine = \case
      CHeaderSystemIncludePath path -> "#include <" ++ path ++ ">"
      CHeaderQuoteIncludePath  path -> "#include \"" ++ path ++ "\""

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

whenPredicateMatches ::
     MonadIO m
  => Tracer IO Skipped
  -> Predicate
  -> Maybe (CHeaderIncludePath, SourcePath)
  -> CXCursor
  -> SingleLoc
  -> (CHeaderIncludePath -> m (Next m a))
  -> m (Next m a)
whenPredicateMatches tracer p mMainHeader current sloc k =
    case mMainHeader of
      Just (mainHeaderIncludePath, mainSourcePath) -> do
        isMatch <- liftIO $ Predicate.match mainSourcePath current sloc p
        case isMatch of
          Right ()     -> k mainHeaderIncludePath
          Left  reason -> liftIO $ do
            name <- clang_getCursorSpelling current
            loc  <- HighLevel.clang_getCursorLocation current
            traceWith tracer Info $ Skipped name loc reason
            return $ Continue Nothing
      Nothing -> return $ Continue Nothing

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
      unrecognizedTypeKind     :: SimpleEnum CXTypeKind
    , unrecognizedTypeSpelling :: Text
    , unrecognizedTypeLocation :: Maybe SingleLoc
    , unrecognizedTypeTrace    :: Backtrace
    }
  deriving stock (Show)
  deriving Exception via CollectedBacktrace UnrecognizedType

unrecognizedCursor ::
     (MonadIO m, HasCallStack)
  => CXCursor
  -> m a
unrecognizedCursor cursor = liftIO $ do
    unrecognizedCursorKind  <- clang_getCursorKind cursor
    unrecognizedCursorLoc   <- HighLevel.clang_getCursorLocation cursor
    unrecognizedCursorTrace <- collectBacktrace
    throwIO UnrecognizedCursor{
        unrecognizedCursorKind
      , unrecognizedCursorLoc
      , unrecognizedCursorTrace
      }

unrecognizedType ::
     (MonadIO m, HasCallStack)
  => CXType -> Maybe SingleLoc -> m a
unrecognizedType typ unrecognizedTypeLocation = liftIO $ do
    let unrecognizedTypeKind = cxtKind typ
    unrecognizedTypeTrace    <- collectBacktrace
    unrecognizedTypeSpelling <- clang_getTypeSpelling typ
    throwIO UnrecognizedType{
        unrecognizedTypeKind
      , unrecognizedTypeSpelling
      , unrecognizedTypeLocation
      , unrecognizedTypeTrace
      }

{-------------------------------------------------------------------------------
  Simple folds
-------------------------------------------------------------------------------}

recurse :: forall m a. Monad m => (CXCursor -> m a) -> Fold m (Tree a)
recurse f = go
  where
    go :: Fold m (Tree a)
    go current = f current >>= \x -> return (Recurse go $ Just . Node x)

continue :: Monad m => (CXCursor -> m a) -> Fold m a
continue f current = f current >>= \x -> return (Continue $ Just x)
