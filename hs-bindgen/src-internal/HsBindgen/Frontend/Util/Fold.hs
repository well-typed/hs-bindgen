module HsBindgen.Frontend.Util.Fold (
    -- * Writing folds
    Dispatch(..)
  , dispatchWithArg
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
