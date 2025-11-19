module HsBindgen.Cache (
    cacheWith
  , CacheMsg(..)
  )
where

import Control.Concurrent (MVar, modifyMVar, newMVar)
import GHC.Generics (Generic)
import Text.SimplePrettyPrint (CtxDoc, (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

-- | Cache a computation with a name.
--
-- Names only serve for debug messages. They need not be unique.
cacheWith :: Tracer CacheMsg -> Maybe String -> IO a -> IO (IO a)
cacheWith tracer name computeRes = do
  cacheVar <- newMVar Nothing
  pure $ getWithCache tracer name cacheVar computeRes

getWithCache :: Tracer CacheMsg -> Maybe String -> MVar (Maybe a) -> IO a -> IO a
getWithCache tracer name cacheVar computeRes = modifyMVar cacheVar $ \case
    Nothing -> do
      traceWith tracer $ CacheMiss name
      !newRes <- computeRes
      pure (Just newRes, newRes)
    Just cachedRes -> do
      traceWith tracer $ CacheHit name
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
