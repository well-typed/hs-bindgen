module Main where

import Control.DeepSeq ()
import Criterion.Main ()
import Data.Map (Map)
import Data.Set qualified as Set

import Data.Either (fromRight)
import GHC.Generics (Generic)
import HsBindgen.Lib

-- instance NFData SourcePath where
--   rnf (SourcePath t) = rnf t

main :: IO ()
main = _
    -- do
    -- print =<<
    --   resolveHeaders args (CHeaderQuoteIncludePath "simple_structs.h")
    -- print =<<
    --   resolveHeaders args (CHeaderQuoteIncludePath "epoxy/gl_generated.h")
    -- defaultMain
    --   [ bench "simple_structs.h" . nfIO $
    --       resolveHeader args (CHeaderQuoteIncludePath "simple_structs.h")
    --   , bench "epoxy/gl_generated.h" . nfIO $
    --       resolveHeader args (CHeaderQuoteIncludePath "epoxy/gl_generated.h")
    --   ]
  where
    clangArgs :: ClangArgs
    clangArgs = def

    tracerConfig :: TracerConfig IO Level BenchMsg
    tracerConfig = def

    resolve :: UncheckedHashIncludeArg -> IO (Map HashIncludeArg FilePath)
    resolve input =
      fmap fromRight $ withTracer tracerConfig $ \tracer -> do
        hashIncludeArg <-
          hashIncludeArgWithTrace
            (contramap BenchHashIncludeArg tracer)
            input
        resolveHeaders
          (contramap BenchResolveHeader tracer)
          clangArgs
          (Set.singleton hashIncludeArg)

data BenchMsg =
    BenchResolveHeader ResolveHeaderMsg
  | BenchHashIncludeArg HashIncludeArgMsg
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyForTrace, IsTrace Level)
