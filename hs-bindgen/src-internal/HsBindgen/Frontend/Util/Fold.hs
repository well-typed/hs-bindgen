module HsBindgen.Frontend.Util.Fold (
    -- * Writing folds
    Dispatch(..)
  , dispatchWithArg
    -- * Errors
  , unknownCursorKind
  , unknownTypeKind
    -- * Debugging
  , showNode
  ) where

import Control.Monad.IO.Class
import Data.Kind

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import HsBindgen.Errors
import HsBindgen.Imports
import Data.Text qualified as Text

{-------------------------------------------------------------------------------
  Writing folds
-------------------------------------------------------------------------------}

class Dispatch a where
  type Kind a :: Type
  dispatch :: MonadIO m => a -> (Kind a -> m b) -> m b

-- | Convenience wrapper that repeats the argument
dispatchWithArg :: (Dispatch a, MonadIO m) => a -> (Kind a -> a -> m b) -> m b
dispatchWithArg x f = dispatch x $ \kind -> f kind x

instance Dispatch CXCursor where
  type Kind CXCursor = CXCursorKind

  dispatch curr k = do
      mKind <- fromSimpleEnum <$> clang_getCursorKind curr
      case mKind of
        Right kind -> k kind
        Left  i    -> panicIO $ "Unrecognized CXCursorKind " ++ show i

instance Dispatch CXType where
  type Kind CXType = CXTypeKind

  dispatch curr k = do
      let mKind = fromSimpleEnum $ cxtKind curr
      case mKind of
        Right kind -> k kind
        Left  i    -> panicIO $ "Unrecognized CXTypeKind " ++ show i

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

unknownCursorKind ::
     (MonadIO m, HasCallStack)
  => CXCursorKind -> CXCursor -> m x
unknownCursorKind kind curr = do
    loc      <- HighLevel.clang_getCursorLocation' curr
    spelling <- clang_getCursorKindSpelling (simpleEnum kind)
    panicIO $ concat [
        "Unknown cursor of kind "
      , show kind
      , " ("
      , Text.unpack spelling
      , ") at "
      , show loc
      ]

-- | Unknown type
--
-- TODO: It would be better if we could report a source location here, but clang
-- does not make it easy to associate a 'CXType' with a location. Perhaps it
-- would be possible to add a 'SingleLoc' argument to 'fromCXType', but it might
-- be hard to get right in all cases, and a /wrong/ source location might be
-- worse than none at all.
unknownTypeKind ::
     (MonadIO m, HasCallStack)
  => CXTypeKind -> CXType -> m x
unknownTypeKind kind _ = do
    spelling <- clang_getTypeKindSpelling (simpleEnum kind)
    panicIO $ concat [
        "Unknown cursor of kind "
      , show kind
      , " ("
      , Text.unpack spelling
      , ")"
      ]

{-------------------------------------------------------------------------------
  Debugging
-------------------------------------------------------------------------------}

-- | Debugging: show node and its children
showNode :: forall m a. MonadIO m => Fold m a
showNode = go 0
  where
    go :: Int -> Fold m a
    go n curr = do
        king     <- fromSimpleEnum <$> clang_getCursorKind curr
        spelling <- clang_getCursorSpelling curr
        loc      <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
        liftIO $ putStrLn $ concat [
            replicate n ' '
          , "Node of type "
          , show king
          , ": "
          , show spelling
          , " at "
          , show loc
          ]
        return $ recursePure (go (n + 2)) (const Nothing)
