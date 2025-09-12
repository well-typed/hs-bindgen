-- | Utilities for writing @tasty@ tests
--
-- Intended for unqualified import.
module Test.Common.Util.Tasty (
    -- * Assertions
    assertException
    -- * Golden tests
  , goldenEDiff
  , goldenAnsiDiff
  ) where

import Control.Exception
import Control.Monad
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Either (isRight)
import Data.Proxy
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.TreeDiff
import Prettyprinter.Render.Terminal qualified as PP
import System.Console.ANSI (SGR (Reset), setSGRCode)
import Test.Common.Util.Tasty.Golden
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec qualified as Parsec
import Text.Parsec.Text ()

import AnsiDiff (ansidiff)
import Prettyprinter qualified as PP

{-------------------------------------------------------------------------------
  Assertions
-------------------------------------------------------------------------------}

assertException :: forall e a.
     Exception e
  => String -> Proxy e -> IO a -> Assertion
assertException msg _ action = do
  result <- tryJust (\x -> fromException x :: Maybe e) action
  when (isRight result) $ assertFailure msg

{-------------------------------------------------------------------------------
  Golden tests
-------------------------------------------------------------------------------}

-- | Golden test using 'ediff'
--
-- This is adapted from 'ediffGolden1', but uses our 'goldenTestSteps'.
goldenEDiff :: forall a.
       ToExpr a
    => TestName
    -> FilePath
    -> ((String -> IO ()) -> IO (ActualValue a))
    -> TestTree
goldenEDiff name fp actual =
    -- impl testName expect actual cmp wrt
    goldenTestSteps name correct actual' cmp update
  where
    correct :: IO Expr
    correct = do
        contents <- Text.decodeUtf8 <$> BS.readFile fp
        case Parsec.parse (exprParser <* Parsec.eof) fp contents of
          Left err -> return $ App "ParseError" [toExpr fp, toExpr (show err)]
          Right r  -> return r

    actual' :: (String -> IO ()) -> IO (ActualValue Expr)
    actual' report = fmap toExpr <$> actual report

    cmp :: Expr -> Expr -> IO (Maybe String)
    cmp a b
        | a == b    = return Nothing
        | otherwise = return $ Just $
            setSGRCode [Reset] ++ renderDoc (ansiWlEditExprCompact $ ediff a b)

    update :: Expr -> IO ()
    update expr =
        BS.writeFile fp $ Text.encodeUtf8 $ Text.pack $
          renderDoc (PP.unAnnotate $ ansiWlExpr expr) ++ "\n"

-- | Golden test using 'ansidiff'
goldenAnsiDiff ::
     TestName
  -> FilePath
  -> ((String -> IO ()) -> IO (ActualValue String))
  -> TestTree
goldenAnsiDiff name fp actual =
    goldenTestSteps name correct actual cmp update
  where
    correct :: IO String
    correct = do
        contents <- BS.readFile fp
        return $ UTF8.toString contents

    cmp :: String -> String -> IO (Maybe String)
    cmp xss yss
        | xss == yss = return Nothing
        | otherwise  = return $ Just $ ansidiff xss yss

    update :: String -> IO ()
    update s = BS.writeFile fp (UTF8.fromString s)

{-------------------------------------------------------------------------------
  Internal auxiliary: pretty-printing
-------------------------------------------------------------------------------}

renderDoc :: PP.Doc PP.AnsiStyle -> String
renderDoc =
    Text.Lazy.unpack . PP.renderLazy . PP.layoutSmart opts
  where
    opts :: PP.LayoutOptions
    opts = PP.LayoutOptions{PP.layoutPageWidth = PP.AvailablePerLine 80 0.4}
