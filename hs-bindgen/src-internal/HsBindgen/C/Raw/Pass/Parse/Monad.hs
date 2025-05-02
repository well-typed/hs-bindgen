module HsBindgen.C.Raw.Pass.Parse.Monad (
    -- * Definition
    M
  , runParseMonad
    -- * Functionaltiy
  , liftIO
  , modify
  ) where

import Control.Monad.IO.Class
import Data.IORef

import HsBindgen.C.Raw.Graph.Includes (IncludeGraph)
import HsBindgen.C.Raw.Graph.Includes qualified as IncludeGraph
import HsBindgen.Eff

{-------------------------------------------------------------------------------
  Definition

  NOTE: We can generate extra output during folding, but the fold itself cannot
  depend /on/ this output ('ParseMonad' supports 'modify' but not 'get').
-------------------------------------------------------------------------------}

-- | Monad used during folding
type M = Eff ParseMonad

data ParseMonad a

type instance Support ParseMonad = IORef IncludeGraph

runParseMonad :: M a -> IO (a, IncludeGraph)
runParseMonad f = do
    refOutput <- newIORef IncludeGraph.empty
    result    <- unwrapEff f refOutput
    (result,) <$> readIORef refOutput

modify :: (IncludeGraph -> IncludeGraph) -> M ()
modify f = wrapEff $ \refOutput ->
    atomicModifyIORef refOutput $ \s -> (f s, ())
