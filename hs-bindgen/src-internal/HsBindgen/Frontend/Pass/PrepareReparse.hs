module HsBindgen.Frontend.Pass.PrepareReparse (
    prepareReparse
  ) where

import Prelude hiding (lex, print)

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as B
import Data.List qualified as List
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import Text.Parsec (ParseError)

import HsBindgen.Clang (ClangSetup)
import HsBindgen.Clang.Discover (ClangExe)
import HsBindgen.Clang.Macros (MacroDefinition)
import HsBindgen.Clang.Macros.Floating (invocationCanFloatInwards)
import HsBindgen.Frontend.AST.TranslationUnit qualified as C
import HsBindgen.Frontend.Pass (IsPass (Msg))
import HsBindgen.Frontend.Pass.PrepareReparse.AST (Decl, Include (Include),
                                                   PostHeader (targets),
                                                   PreHeader, Tag,
                                                   Target (Target))
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse,
                                                      PrepareReparseMsg (..))
import HsBindgen.Frontend.Pass.PrepareReparse.Lexer (Token, lex)
import HsBindgen.Frontend.Pass.PrepareReparse.Parser (parse)
import HsBindgen.Frontend.Pass.PrepareReparse.Preprocessor (preprocess)
import HsBindgen.Frontend.Pass.PrepareReparse.Printer (print)
import HsBindgen.Frontend.Pass.PrepareReparse.Simplifier (simplify)
import HsBindgen.Frontend.Pass.PrepareReparse.Tracer (traceImmediate)
import HsBindgen.Frontend.Pass.PrepareReparse.Update (update)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckMacros)
import HsBindgen.Frontend.RootHeader (RootHeader)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Util.Tracer (Tracer)

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

prepareReparse ::
     forall l.
     Tracer (Msg PrepareReparse)
  -> Maybe ClangExe
  -> ClangSetup
  -> RootHeader
  -> [MacroDefinition]
  -> C.TranslationUnit l TypecheckMacros
  -> IO (C.TranslationUnit l PrepareReparse)
prepareReparse tr clangExeMay setup root macroDefs unit
  | invocationCanFloatInwards macroDefs
  = do
      traceImmediate tr PrepareReparseInvocationsNotFloatable
      pure fallback
  | otherwise
  = do
    case clangExeMay of
      -- When we can't find the @clang@ executable, return the fallback value.
      Nothing -> do
        traceImmediate tr PrepareReparseClangExeNotFound
        pure fallback
      Just clangExe -> do
        withSystemTempDirectory "hs-bindgen_prepare-reparse" $ \dir -> do
          -- The root header contents are written to a temporary root header
          -- file
          let rootHeaderContents = RootHeader.content root
              rootHeaderHash = hashString rootHeaderContents
              rootHeaderName = rootHeaderHash <.> "h"
              rootHeaderPath = dir </> rootHeaderName
          traceImmediate tr $ PrepareReparseWriteTempHeader rootHeaderPath rootHeaderContents
          writeFile rootHeaderPath rootHeaderContents
          -- Reparse targets are written to a temporary header as well
          let headerSimplified = runSimplifier rootHeaderName unit
              headerPrinted = runPrinter headerSimplified ""
              headerHash = hashString headerPrinted
              headerPath = dir </> headerHash <.> "h"
          traceImmediate tr $ PrepareReparseWriteTempHeader headerPath headerPrinted
          writeFile headerPath headerPrinted
          -- Preprocess the temporary headers
          res <- preprocess tr clangExe setup headerPath
          case res of
            -- When the preprocessor failed to run, return the fallback value.
            Left _e -> do
              traceImmediate tr PrepareReparsePreprocessorFailed
              pure fallback
            Right preprocessedContents -> do
              case cut headerPath preprocessedContents of
                -- When we can't single out the interesting lines of code from
                -- the preprocessor output, return the fallback value.
                Nothing -> do
                  traceImmediate tr PrepareReparseInterpretPreprocessorOutputFailed
                  pure fallback
                Just cutContents -> do
                  traceImmediate tr $ PrepareReparseTempHeaderCutContents cutContents
                  case runLexer cutContents >>= runParser of
                    -- When we can't parse the preprocessor output, return the
                    -- fallback value.
                    Left e -> do
                      traceImmediate tr $ PrepareReparseParsePreprocessorOutputFailed e
                      pure fallback
                    Right postHeader -> do
                      pure $ runUpdater postHeader unit
  where
    -- | Default to flattening tokens without expanding macro invocations.
    fallback :: C.TranslationUnit l PrepareReparse
    fallback = update Nothing unit

{-------------------------------------------------------------------------------
  Cut
-------------------------------------------------------------------------------}

cut :: FilePath -> String -> Maybe String
cut headerPath headerContents = go (lines headerContents)
  where
    target' = "# 2 " <> show headerPath <> " 2"

    go []     = Nothing
    go (x:xs)
      | target' `List.isInfixOf` x
      = Just $ unlines $ filter (not . ("#" `List.isPrefixOf`)) xs
      | otherwise
      = go xs

{-------------------------------------------------------------------------------
  Internal phases
-------------------------------------------------------------------------------}

runSimplifier :: FilePath -> C.TranslationUnit l TypecheckMacros -> PreHeader
runSimplifier rootHeaderPath unit = simplify () unit $ Include rootHeaderPath

runPrinter :: PreHeader -> ShowS
runPrinter = print

runLexer :: String -> Either ParseError [Token]
runLexer = lex

runParser :: [Token] -> Either ParseError PostHeader
runParser = parse

runUpdater ::
     PostHeader
  -> C.TranslationUnit l TypecheckMacros
  -> C.TranslationUnit l PrepareReparse
runUpdater header unit = update (Just mapping) unit
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
