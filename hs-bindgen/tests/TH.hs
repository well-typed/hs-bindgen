{-# LANGUAGE TemplateHaskellQuotes #-}
-- | Separate module for TH tests.
--
-- This module exists mainly to avoid hassle with unused imports and packages.
module TH (
    goldenTh,
) where

import Control.Monad.State.Strict (State, get, put, evalState)
import Data.Generics qualified as SYB
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import System.FilePath ((</>))
import Test.Tasty (TestTree, TestName)

import Misc
import HsBindgen.Clang.Paths
import HsBindgen.Lib

goldenTh :: FilePath -> TestName -> TestTree
goldenTh packageRoot name = goldenVsStringDiff_ "th" ("fixtures" </> (name ++ ".th.txt")) $ \report -> do
    -- -<.> does weird stuff for filenames with multiple dots;
    -- I usually simply avoid using it.
    relPath <- either fail return $ mkCHeaderRelPath (name ++ ".h")
    let args = clangArgs packageRoot
    includeDirs <- either fail return =<<
      resolveCIncludeAbsPathDirs (clangIncludePathDirs args)
    absPath <- either fail return =<< resolveHeader includeDirs relPath

    let tracer = mkTracer report report report False

    header <- parseC tracer args absPath
    let decls :: Qu [TH.Dec]
        decls = genTH relPath defaultTranslationOpts header

        -- unqualify names, qualified names are noisy *and*
        -- GHC.Base names have moved.
        unqualNames :: [TH.Dec] -> [TH.Dec]
        unqualNames = SYB.everywhere $ SYB.mkT mangleName

        mangleName :: TH.Name -> TH.Name
        mangleName n | n == ''()             = TH.Name (TH.OccName "Unit") TH.NameS
        mangleName (TH.Name occ TH.NameG {}) = TH.Name occ TH.NameS
        mangleName n = n

    return $ unlines $ map (show . TH.ppr) $ unqualNames $ runQu decls

-- | Deterministic monad with TH.Quote instance
newtype Qu a = Qu (State Integer a)
  deriving newtype (Functor, Applicative, Monad)

instance TH.Quote Qu where
    newName n = Qu $ do
        u <- get
        put $! u + 1
        return $ TH.Name (TH.OccName n) (TH.NameU u)

runQu :: Qu a -> a
runQu (Qu m) = evalState m 0

parseC ::
     Tracer IO String
  -> ClangArgs
  -> CHeaderAbsPath
  -> IO CHeader
parseC tracer args headerPath =
    withTranslationUnit tracerD args headerPath $
      parseCHeader tracerP SelectFromMainFile
  where
    tracerD = contramap show tracer
    tracerP = contramap prettyLogMsg tracer
