module HsBindgen.Cache (
    Cached(..)
  , cacheWith
  , CacheMsg(..)
  )
where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import Text.SimplePrettyPrint (CtxDoc, (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

newtype Cached a = Cached {  getCached :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Cache a computation with a name.
--
-- Names only serve for debug messages. They need not be unique.
cacheWith :: Tracer CacheMsg -> Maybe String -> Cached a -> IO (Cached a)
cacheWith tracerCache name action = do
  cacheVar <- newMVar Nothing
  pure $ getWithCache tracerCache name cacheVar action

getWithCache ::
     Tracer CacheMsg
  -> Maybe String
  -> MVar (Maybe a)
  -> Cached a
  -> Cached a
getWithCache tracerCache name cacheVar action = Cached $
    modifyMVar cacheVar $ \case
      Nothing -> do
        traceWith tracerCache $ CacheMiss name
        !newRes <- getCached action
        pure (Just newRes, newRes)
      Just cachedRes -> do
        traceWith tracerCache $ CacheHit name
        pure (Just cachedRes, cachedRes)

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data CacheMsg =
    CacheMiss (Maybe String)
  | CacheHit  (Maybe String)
  deriving (Show, Eq, Generic)

prettyForTraceName :: Maybe String -> CtxDoc
prettyForTraceName Nothing = "anonymous computation"
prettyForTraceName (Just name) = PP.string name

instance PrettyForTrace CacheMsg where
  prettyForTrace = \case
    CacheMiss mName -> "Cache miss:" <+> prettyForTraceName mName >< "; computing value"
    CacheHit  mName -> "Cache hit: " <+> prettyForTraceName mName

instance IsTrace SafeLevel CacheMsg where
  getDefaultLogLevel = const SafeDebug
  getSource          = const HsBindgen
  getTraceId         = const "cache"
