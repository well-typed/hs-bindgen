module HsBindgen.Frontend.Pass.PrepareReparse (
    prepareReparse
  ) where

import Prelude hiding (lex, print)

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B
import Data.List qualified as List
import Data.Map.Lazy qualified as Map
import Data.Maybe (fromJust)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Text.Parsec (ParseError)

import HsBindgen.Clang (ClangSetup)
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass (IsPass (Msg))
import HsBindgen.Frontend.Pass.PrepareReparse.AST (Decl, Include (Include),
                                                   PostHeader (targets),
                                                   PreHeader, Tag,
                                                   Target (Target))
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse,
                                                      PrepareReparseMsg (PrepareReparseReadTempHeaderCutContents, PrepareReparseWriteTempHeader))
import HsBindgen.Frontend.Pass.PrepareReparse.Lexer (Token, lex)
import HsBindgen.Frontend.Pass.PrepareReparse.Parser (parse)
import HsBindgen.Frontend.Pass.PrepareReparse.Preprocessor (preprocess)
import HsBindgen.Frontend.Pass.PrepareReparse.Printer (print)
import HsBindgen.Frontend.Pass.PrepareReparse.Simplifier (simplify)
import HsBindgen.Frontend.Pass.PrepareReparse.Tracer (traceImmediate)
import HsBindgen.Frontend.Pass.PrepareReparse.Updater (update)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckMacros)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports (Map, throwIO)
import HsBindgen.Util.Tracer (Tracer)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

prepareReparse ::
     Tracer (Msg PrepareReparse)
  -> ClangSetup
  -> RootHeader
  -> C.TranslationUnit TypecheckMacros
  -> IO (C.TranslationUnit PrepareReparse)
prepareReparse tr setup root unit = do
    withSystemTempDirectory "hs-bindgen_prepare-reparse" $ \dir -> do
      let rootHeaderContents = RootHeader.content root
          rootHeaderHash = hashString rootHeaderContents
          rootHeaderName = rootHeaderHash <.> "h"
          rootHeaderPath = dir </> rootHeaderName
      traceImmediate tr $ PrepareReparseWriteTempHeader rootHeaderPath rootHeaderContents
      writeFile rootHeaderPath rootHeaderContents
      let headerSimplified = runSimplifier rootHeaderName unit
          headerPrinted = runPrinter headerSimplified ""
          headerHash = hashString headerPrinted
          headerPath = dir </> headerHash <.> "h"
      traceImmediate tr $ PrepareReparseWriteTempHeader headerPath headerPrinted
      writeFile headerPath headerPrinted
      res <- preprocess tr setup headerPath
      case res of
        Left e -> throwIO e
        Right x -> do
          let cutContents = fromJust $ cut headerPath x
          traceImmediate tr $ PrepareReparseReadTempHeaderCutContents cutContents
          postHeader <- either (throwIO . userError . show) pure (runParser =<< runLexer cutContents)
          pure $ runUpdater postHeader unit

{-------------------------------------------------------------------------------
  Internal cutter
-------------------------------------------------------------------------------}

cut :: String -> String -> Maybe String
cut target = \contents -> go (lines contents)
  where
    target' = "# 2 " <> show target <> " 2"

    go []     = Nothing
    go (x:xs)
      | target' `List.isInfixOf` x
      = Just $ unlines $ filter (not . ("#" `List.isPrefixOf`)) xs
      | otherwise
      = go xs

{-------------------------------------------------------------------------------
  Internal phases
-------------------------------------------------------------------------------}

runSimplifier :: FilePath -> C.TranslationUnit TypecheckMacros -> PreHeader
runSimplifier rootHeaderPath unit = simplify () unit $ Include rootHeaderPath

runPrinter :: PreHeader -> ShowS
runPrinter = print

runLexer :: String -> Either ParseError [Token]
runLexer = lex

runParser :: [Token] -> Either ParseError PostHeader
runParser = parse

runUpdater ::
     PostHeader
  -> C.TranslationUnit TypecheckMacros
  -> C.TranslationUnit PrepareReparse
runUpdater header unit = update mapping unit
  where
    mapping :: Map Tag Decl
    mapping = Map.fromList [
          (tag, decl)
        | Target tag decl <- header.targets
        ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Construct a hash
--
-- It is not so important that the hash is stable or very good. The main intent
-- is to generate a name for a temporary file that is fairly likely to be
-- unique.
hashString :: String -> String
hashString = B.unpack . B.take 16 . B16.encode . hash . B.pack
