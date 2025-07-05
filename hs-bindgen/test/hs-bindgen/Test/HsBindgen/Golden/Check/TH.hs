{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Golden test: TH output
module Test.HsBindgen.Golden.Check.TH (check) where

import Control.Monad.State.Strict (State, get, put, runState)
import Data.Generics qualified as SYB
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import System.FilePath (makeRelative)
import Test.Tasty

import HsBindgen.Guasi
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

import Test.Common.Util.Tasty
import Test.Common.Util.Tasty.Golden (ActualValue(..))
import Test.HsBindgen.Resources
import Test.HsBindgen.Golden.TestCase

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check testResources test =
    goldenAnsiDiff "th" fixture $ \_report ->
      if ghcAtLeast904 then do
        config  <- getTestConfig testResources test
        unit    <- testParse testResources test
        pkgroot <- getTestPackageRoot testResources

        let decls :: Qu [TH.Dec]
            decls = Pipeline.genBindingsFromCHeader config unit

            depfiles :: [FilePath]
            csources :: [String]
            thdecs   :: [TH.Dec]
            (depfiles, csources, thdecs) = runQu decls

            -- Here we might have headers outside of our package, but in our
            -- test setup that SHOULD cause an error, as we use bundled stdlib.
            -- And we will cause those on CI, which runs tests on different
            -- systems.
            output :: String
            output = unlines $ concat [
                [    "-- addDependentFile "
                  ++ convertWindows (makeRelative pkgroot fp)
                | fp <- depfiles
                ]
              , [ "-- " ++ l
                | src <- csources, l <- lines src
                ]
              , [ show $ TH.ppr d
                | d <- unqualNames thdecs
                ]
              ]

        return $ ActualValue output
      else
        return $ ActualSkipped "ghc < 9.4"
  where
    fixture :: FilePath
    fixture = "fixtures" </> (testName test ++ ".th.txt")

{-------------------------------------------------------------------------------
  Internal auxiliary: manipulate output
-------------------------------------------------------------------------------}

-- unqualify names, qualified names are noisy *and* GHC.Base names move
unqualNames :: [TH.Dec] -> [TH.Dec]
unqualNames = SYB.everywhere $ SYB.mkT mangleName
  where
    mangleName :: TH.Name -> TH.Name
    mangleName n | n == ''()             = TH.Name (TH.OccName "Unit") TH.NameS
    mangleName (TH.Name occ TH.NameG {}) = TH.Name occ TH.NameS
    mangleName n                         = n

convertWindows :: FilePath -> FilePath
convertWindows = map f where
  f '\\' = '/'
  f c    = c

{-------------------------------------------------------------------------------
  Internal auxiliary: 'Qu' monad
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Skip TH for older ghc
ghcAtLeast904 :: Bool
#if __GLASGOW_HASKELL__ >=904
ghcAtLeast904 = True
#else
ghcAtLeast904 = False
#endif
