module HsBindgen.Cache (
    cacheWith
  , CacheMsg(..)
  )
where

import Control.Concurrent (MVar, newEmptyMVar, tryPutMVar, tryReadMVar)
import GHC.Generics (Generic)
import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Util.Tracer

-- | Cache a computation with a name.
--
-- Names only serve for debug messages. They need not be unique.
cacheWith :: Tracer IO CacheMsg -> Maybe String -> IO a -> IO (IO a)
cacheWith tracer name computeRes = do
  cacheVar <- newEmptyMVar
  pure $ getWithCache tracer name cacheVar computeRes

getWithCache :: Tracer IO CacheMsg -> Maybe String -> MVar a -> IO a -> IO a
getWithCache tracer name cacheVar computeRes = do
  maybeCachedRes <- tryReadMVar cacheVar
  case maybeCachedRes of
    Nothing -> do
      traceWith tracer $ CacheFail name
      newRes <- computeRes
      _ <- tryPutMVar cacheVar newRes
      pure newRes
    Just cachedRes -> do
      traceWith tracer $ CacheHit name
      pure cachedRes

{-------------------------------------------------------------------------------
  Traces
-------------------------------------------------------------------------------}

data CacheMsg =
    CacheFail (Maybe String)
  | CacheHit  (Maybe String)
  deriving (Show, Eq, Generic)

prettyForTraceName :: Maybe String -> CtxDoc
prettyForTraceName Nothing = "anonymous computation"
prettyForTraceName (Just name) = PP.string name

instance PrettyForTrace CacheMsg where
  prettyForTrace = \case
    CacheFail mName -> "Cache miss: " >< prettyForTraceName mName
    CacheHit  mName -> "Cache hit : " >< prettyForTraceName mName

instance IsTrace Level CacheMsg where
  getDefaultLogLevel = const Debug
  getSource          = const HsBindgen
  getTraceId         = const "cache"
