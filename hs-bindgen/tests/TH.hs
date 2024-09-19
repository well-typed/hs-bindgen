-- | Separate module for TH tests.
--
-- This module exists mainly to avoid hassle with unused imports and packages.
module TH (
    goldenVsStringDiff_,
    goldenTh,
) where

import Control.Monad.State.Strict (State, get, put, evalState)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Generics qualified as SYB
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import System.FilePath ((</>))
import Test.Tasty (TestTree, TestName)

import TastyGolden
import Diff

import HsBindgen.Lib

-- | Like goldenVsString but using our own diff function.
goldenVsStringDiff_ :: TestName -> FilePath -> ((String -> IO ()) -> IO String) -> TestTree
goldenVsStringDiff_ name fp action = goldenTestSteps name correct action cmp update
  where
    correct :: IO String
    correct = do
        contents <- BS.readFile fp
        return $ UTF8.toString contents

    update :: String -> IO ()
    update s = BS.writeFile fp (UTF8.fromString s)

    cmp :: String -> String -> IO (Maybe String)
    cmp xss yss
        | xss == yss = return Nothing
        | otherwise  = return $ Just $ unlines $ ansiLinesDiff (lines xss) (lines yss)

goldenTh :: TestName -> TestTree
goldenTh name = goldenVsStringDiff_ "th" ("fixtures" </> (name ++ ".th.txt")) $ \report -> do
    -- -<.> does weird stuff for filenames with multiple dots;
    -- I usually simply avoid using it.
    let fp = "examples" </> (name ++ ".h")
        args = ["-target", "x86_64-pc-linux-gnu"]

    let tracer = mkTracer report report report False
    let tracerD = contramap show tracer
    let tracerP = contramap prettyLogMsg tracer

    header <- parseCHeader tracerD tracerP SelectFromMainFile args fp
    let decls :: Qu [TH.Dec]
        decls = genDecls header

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
