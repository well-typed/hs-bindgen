module HsBindgen.Frontend.Pass.Parse.Util (
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

  dispatch x f = do
      mKind <- fromSimpleEnum <$> clang_getCursorKind x
      case mKind of
        Right kind   -> f kind
        Left unknown -> panicIO $ "Unrecognized CXCursorKind " ++ show unknown

instance Dispatch CXType where
  type Kind CXType = CXTypeKind

  dispatch x f = do
      let mKind = fromSimpleEnum $ cxtKind x
      case mKind of
        Right kind   -> f kind
        Left unknown -> panicIO $ "Unrecognized CXTypeKind " ++ show unknown

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



