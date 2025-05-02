module HsBindgen.C.Fold.Common (
    -- * Root header
    rootHeaderName
  , rootHeaderContent
    -- * Predicates
  , Skipped(..)
  , whenPredicateMatches
    -- * Location
  , DeclLoc(..)
  , Relationship(..)
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
        isMatch <- Predicate.match mainSourcePath current sloc p
        case isMatch of
          Right ()     -> k mainHeaderIncludePath
          Left  reason -> do
            name <- clang_getCursorSpelling current
            loc  <- HighLevel.clang_getCursorLocation current
            liftIO $ traceWith tracer Info $ Skipped name loc reason
            return $ Continue Nothing
      Nothing -> return $ Continue Nothing

{-------------------------------------------------------------------------------
  Location
-------------------------------------------------------------------------------}

data DeclLoc =
    -- | We have a precise location
    Precise SingleLoc

    -- | We can only refer to the location of a related declaration
    --
    -- Sometimes we don't get a precise location, but instead can only refer to
    -- the location of another declaration, and provide a hint about the
    -- relationship between the two declarations.
  | RelatedTo DeclLoc Relationship
  deriving stock (Show)

data Relationship =
    Arg               -- ^ clang_getArgType
  | ArrayElement      -- ^ clang_getArrayElementType
  | Named             -- ^ clang_Type_getNamedType
  | Pointee           -- ^ clang_getPointeeType
  | Result            -- ^ clang_getResultType
  | TypedefUnderlying -- ^ clang_getTypedefDeclUnderlyingType
  | EnumInteger       -- ^ clang_getEnumDeclIntegerType
  deriving stock (Show)

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
    , unrecognizedTypeLocation :: DeclLoc
    , unrecognizedTypeTrace    :: Backtrace
    }
  deriving stock (Show)
  deriving Exception via CollectedBacktrace UnrecognizedType

unrecognizedCursor ::
     (MonadIO m, HasCallStack)
  => CXCursor
  -> m a
unrecognizedCursor cursor = do
    unrecognizedCursorKind  <- clang_getCursorKind cursor
    unrecognizedCursorLoc   <- HighLevel.clang_getCursorLocation cursor
    unrecognizedCursorTrace <- collectBacktrace
    liftIO $ throwIO UnrecognizedCursor{
        unrecognizedCursorKind
      , unrecognizedCursorLoc
      , unrecognizedCursorTrace
      }

unrecognizedType :: (MonadIO m, HasCallStack) => CXType -> DeclLoc -> m a
unrecognizedType typ declLoc = do
    mTyLoc <- tryGetTypeLoc typ
    let unrecognizedTypeKind = cxtKind typ
    unrecognizedTypeTrace    <- collectBacktrace
    unrecognizedTypeSpelling <- clang_getTypeSpelling typ
    liftIO $ throwIO UnrecognizedType{
        unrecognizedTypeKind
      , unrecognizedTypeSpelling
      , unrecognizedTypeLocation = maybe declLoc Precise mTyLoc
      , unrecognizedTypeTrace
      }

-- | Get location of a 'CXType', if available
tryGetTypeLoc :: MonadIO m => CXType -> m (Maybe SingleLoc)
tryGetTypeLoc typ = do
    cursor   <- clang_getTypeDeclaration typ
    location <- multiLocExpansion <$> HighLevel.clang_getCursorLocation cursor
    return $ if singleLocLine location /= 0
               then Just location
               else Nothing

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
