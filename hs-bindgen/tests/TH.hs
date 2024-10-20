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
import HsBindgen.Lib

goldenTh :: FilePath -> TestName -> TestTree
goldenTh packageRoot name = goldenVsStringDiff_ "th" ("fixtures" </> (name ++ ".th.txt")) $ \report -> do
    -- -<.> does weird stuff for filenames with multiple dots;
    -- I usually simply avoid using it.
    let fp = "examples" </> (name ++ ".h")
        args = clangArgs packageRoot

    let tracer = mkTracer report report report False

    header <- parseC tracer args fp
    let decls :: Qu [TH.Dec]
        decls = genTH header

        -- unqualify names, qualified names are noisy *and*
        -- GHC.Base names have moved.
        unqualNames :: [TH.Dec] -> [TH.Dec]
        unqualNames = SYB.everywhere $ SYB.mkT $ \case
            TH.NameG {} -> TH.NameS
            nameFlavour -> nameFlavour

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
  -> FilePath
  -> IO CHeader
parseC tracer args fp =
    withTranslationUnit tracerD args fp $
      parseCHeader tracerP SelectFromMainFile
  where
    tracerD = contramap show tracer
    tracerP = contramap prettyLogMsg tracer
