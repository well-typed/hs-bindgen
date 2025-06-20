{-# LANGUAGE TemplateHaskellQuotes #-}
-- | Separate module for TH tests.
--
-- This module exists mainly to avoid hassle with unused imports and packages.
module Test.Internal.TH (
    goldenTh,
) where

import Control.Monad.State.Strict (State, get, put, runState)
import Data.Generics qualified as SYB
import GHC.Stack (HasCallStack)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import System.FilePath (makeRelative)
import Test.Tasty (TestName, TestTree)

import Clang.Paths
import HsBindgen.Guasi
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline
import Test.Internal.Misc
import Test.Internal.Tracer (TracePredicate, withTracePredicate)

goldenTh :: HasCallStack
  => FilePath
  -> TestName
  -> TracePredicate Trace
  -> TestTree
goldenTh packageRoot name predicate = goldenVsStringDiff_ "th" ("fixtures" </> (name ++ ".th.txt")) $ \_ -> do
    withTracePredicate predicate $ \tracer -> do
      -- -<.> does weird stuff for filenames with multiple dots;
      -- I usually simply avoid using it.
      let headerIncludePath = CHeaderQuoteIncludePath $ name ++ ".h"
          opts :: Pipeline.Opts
          opts = def {
              Pipeline.optsClangArgs = getClangArgs packageRoot [
                  "examples/golden"
                , "examples/golden-norust"
                ]
            , Pipeline.optsTracer    = tracer
            }
      unit <- Pipeline.parseCHeaders opts [headerIncludePath]

      let decls :: Qu [TH.Dec]
          decls = Pipeline.genBindingsFromCHeader opts unit

          -- unqualify names, qualified names are noisy *and*
          -- GHC.Base names have moved.
          unqualNames :: [TH.Dec] -> [TH.Dec]
          unqualNames = SYB.everywhere $ SYB.mkT mangleName

          mangleName :: TH.Name -> TH.Name
          mangleName n | n == ''()             = TH.Name (TH.OccName "Unit") TH.NameS
          mangleName (TH.Name occ TH.NameG {}) = TH.Name occ TH.NameS
          mangleName n = n

      let (depfiles, csources, thdecs) = runQu decls
      pure $ unlines $
          -- here we might have headers outside of our package,
          -- but in our test setup that SHOULD cause an error, as we use bundled stdlib,
          -- And we will cause those on CI, which runs tests on different systems
          [ "-- addDependentFile " ++ convertWindows (makeRelative packageRoot fp) | fp <- depfiles ] ++
          [ "-- " ++ l | src <- csources, l <- lines src ] ++
          [ show $ TH.ppr d | d <- unqualNames thdecs ]

convertWindows :: FilePath -> FilePath
convertWindows = map f where
  f '\\' = '/'
  f c    = c

-- | Deterministic monad with TH.Quote instance
newtype Qu a = Qu (State ([FilePath], Integer, [String]) a)
  deriving newtype (Functor, Applicative, Monad)

instance TH.Quote Qu where
    newName n = Qu $ do
        (depfiles, u, csources) <- get
        put $! (depfiles, u + 1, csources)
        return $ TH.Name (TH.OccName n) (TH.NameU u)

instance Guasi Qu where
    -- we don't use unique string to have stable test results
    getModuleUnique = return $ ModuleUnique "test_internal"

    addDependentFile fp = Qu $ do
        (depfiles, u, csources) <- get
        put $! (depfiles ++ [fp], u, csources)

    addCSource src = Qu $ do
        (depfiles, u, csources) <- get
        put $! (depfiles, u, csources ++ [src])

    -- Note: we could mock these better, if we want to test error reporting
    -- Currently (2025-04-15) we only report missing extensions,
    -- so there isn't much to test.
    extsEnabled = return []
    reportError _ = return ()

runQu :: Qu a -> ([FilePath], [String], a)
runQu (Qu m) = case runState m ([], 0, []) of
    (x, (depfiles, _, csources)) -> (depfiles, csources, x)
