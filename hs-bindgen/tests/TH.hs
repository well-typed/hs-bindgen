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
    let headerIncludePath = CHeaderQuoteIncludePath $ name ++ ".h"
        args = clangArgs packageRoot
        tracer = mkTracer report report report False
    src <- resolveHeader args headerIncludePath

    header <- parseC tracer args src
    let decls :: Qu [TH.Dec]
        decls = genTH headerIncludePath defaultTranslationOpts header

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
  -> SourcePath
  -> IO CHeader
parseC tracer args src =
    withTranslationUnit tracerD args src $
      parseCHeader tracerP SelectFromMainFile emptyExtBindings
  where
    tracerD = contramap show tracer
    tracerP = contramap prettyLogMsg tracer
