-- | @hs-bindgen-cli info builtin-macros@ command
--
-- This command dumps builtin macros using @libclang@.  The same can be done
-- using @clang@ with command @clang -dM -E -x c /dev/null@.  This command
-- exists because @libclang@ exposes some different macros than @clang@.
--
-- This command is different from the @clang@ dump command in that it fully
-- expands macros.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.BuiltinMacros qualified as BuiltinMacros
module HsBindgen.Cli.Info.BuiltinMacros (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Options.Applicative hiding (info)

import Clang.Args
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.App
import HsBindgen.Boot
import HsBindgen.Clang
import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "List LLVM/Clang builtin macros"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

newtype Opts = Opts {
      clangArgsConfig :: ClangArgsConfig FilePath
    }

parseOpts :: Parser Opts
parseOpts = Opts <$> parseClangArgsConfig

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} =
    void . withTracer tracerConfig $ \tracer -> do
      let tracerBoot  = contramap TraceBoot tracer
          tracerClang = contramap (TraceFrontend . FrontendClang) tracer
      (clangArgs, _target) <- getClangArgsAndTarget tracerBoot clangArgsConfig
      -- 1. Get the names of the builtin macros
      names <- getBuiltinMacroNames tracerClang clangArgs
      -- 2. Try to get stringified values for all macros
      results1 <- getMacros tracerClang clangArgs names
      -- 3. Find failed macros, where the stringified macro is the macro name,
      --    and remove them from the results
      let failedNames =
            [n | (n, v) <- Map.toList results1, Text.unpack (C.getName n) == v]
          results2 = foldr Map.delete results1 failedNames
      -- 4. Try to get stringified values for the failed macros by passing a
      --    single paramater
      paramResults <- getParamMacros tracerClang clangArgs failedNames
      -- 5. Compute the final results and the set of all names, with the
      --    parameterized macro names
      let results3 = results2 <> paramResults
          paramNamesSet = Map.keysSet paramResults
          namesSet =
            Set.difference
              (Set.fromList names <> paramNamesSet)
              (Set.map stripParams paramNamesSet)
      -- 6. Print all macros, including any failed ones
      forM_ (Set.toAscList namesSet) $ \name ->
        putStrLn $ renderResult name (Map.lookup name results3)
  where
    -- Output space even when @v@ is empty to match @clang@ behavior
    renderResult :: C.Name -> Maybe String -> String
    renderResult builtinName mValue =
      let n = Text.unpack (C.getName builtinName)
          v = fromMaybe "UNKNOWN_MACRO_DEFINITION" mValue
      in  "#define " ++ n ++ ' ' : v

    stripParams :: C.Name -> C.Name
    stripParams = C.Name . Text.takeWhile (/= '(') . C.getName

-- | Get the names of all builtin macros
--
-- Parameterized macros do /not/ include the parameters.
getBuiltinMacroNames :: Tracer ClangMsg -> ClangArgs -> IO [C.Name]
getBuiltinMacroNames tracer clangArgs =
    fmap (fromMaybe []) . withClang' tracer setup $ \unit -> do
      root <- clang_getTranslationUnitCursor unit
      Just <$> HighLevel.clang_visitChildren root visit
  where
    setup :: ClangSetup
    setup = defaultClangSetup clangArgs $
      ClangInputMemory "hs-bindgen-builtins.h" ""

    visit :: Fold IO C.Name
    visit = simpleFold $ \curr -> do
      mBuiltin <- C.checkIsBuiltin curr
      case mBuiltin of
        Just name -> foldContinueWith (C.Name name)
        Nothing   -> foldBreak

-- | Get stringified definitions of the specified macros
--
-- The stringified definition of a parameteried macro is the macro name itself
-- (not including the parameters).
getMacros :: Tracer ClangMsg -> ClangArgs -> [C.Name] -> IO (Map C.Name String)
getMacros tracer clangArgs names =
    fmap (maybe Map.empty Map.fromList) . withClang' tracer setup $ \unit -> do
      root <- clang_getTranslationUnitCursor unit
      Just <$> HighLevel.clang_visitChildren root visit
  where
    setup :: ClangSetup
    setup =
        defaultClangSetup clangArgs
      . ClangInputMemory "hs-bindgen-builtins.h"
      . unlines
      $ helperMacros ++ map mkDecl names

    -- https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html
    helperMacros :: [String]
    helperMacros = [
        "#define STREX(x) #x"
      , "#define STR(x) STREX(x)"
      ]

    mkDecl :: C.Name -> String
    mkDecl builtinName =
      let name = Text.unpack (C.getName builtinName)
      in  "const char *BUILTIN_X_" ++ name ++ " = STR(" ++ name ++ ");"

    parseName :: C.Name -> Maybe C.Name
    parseName = fmap C.Name . Text.stripPrefix "BUILTIN_X_" . C.getName

    visit :: Fold IO (C.Name, String)
    visit = simpleFold $ \curr -> do
      C.getPrelimDeclId curr C.NameKindOrdinary >>= \case
        C.PrelimDeclIdAnon{}           -> foldContinue
        C.PrelimDeclIdNamed name _kind ->
          (fromSimpleEnum <$> clang_getCursorKind curr) >>= \case
            Right CXCursor_VarDecl -> case parseName name of
              Nothing          -> foldContinue
              Just builtinName -> HighLevel.clang_evaluate curr >>= \case
                Just (EvalResultString s) -> foldContinueWith (builtinName, s)
                _otherwise                -> foldContinue
            _otherwise -> foldContinue

-- | Get stringified definitions of the specified macros with a single parameter
--
-- This is a dirty hack just for the builtin macros.  It just passes a single
-- parameter and expects the stringified result to either be unchanged for
-- append some string.
getParamMacros ::
     Tracer ClangMsg
  -> ClangArgs
  -> [C.Name]
  -> IO (Map C.Name String)
getParamMacros tracer clangArgs names =
    fmap (maybe Map.empty Map.fromList) . withClang' tracer setup $ \unit -> do
      root <- clang_getTranslationUnitCursor unit
      Just <$> HighLevel.clang_visitChildren root visit
  where
    setup :: ClangSetup
    setup =
        defaultClangSetup clangArgs
      . ClangInputMemory "hs-bindgen-builtins.h"
      . unlines
      $ helperMacros ++ map mkDecl names

    -- https://gcc.gnu.org/onlinedocs/cpp/Stringizing.html
    helperMacros :: [String]
    helperMacros = [
        "#define STREX(x) #x"
      , "#define STR(x) STREX(x)"
      ]

    mkDecl :: C.Name -> String
    mkDecl builtinName =
      let name = Text.unpack (C.getName builtinName)
      in  "const char *BUILTIN_X_" ++ name ++ " = STR(" ++ name ++ "(c));"

    parseName :: C.Name -> Maybe C.Name
    parseName = fmap C.Name . Text.stripPrefix "BUILTIN_X_" . C.getName

    visit :: Fold IO (C.Name, String)
    visit = simpleFold $ \curr ->
      C.getPrelimDeclId curr C.NameKindOrdinary >>= \case
        C.PrelimDeclIdAnon{}           -> foldContinue
        C.PrelimDeclIdNamed name _kind ->
          (fromSimpleEnum <$> clang_getCursorKind curr) >>= \case
            Right CXCursor_VarDecl -> case parseName name of
              Nothing          -> foldContinue
              Just builtinName -> HighLevel.clang_evaluate curr >>= \case
                Just (EvalResultString s) -> case mangleValue s of
                  Just s' -> foldContinueWith (mangleName builtinName, s')
                  Nothing -> foldContinue
                _otherwise -> foldContinue
            _otherwise -> foldContinue

    mangleName :: C.Name -> C.Name
    mangleName = C.Name . (<> "(c)") . C.getName

    mangleValue :: String -> Maybe String
    mangleValue = \case
      "c"        -> Just "c"
      'c' : s    -> Just ("c##" ++ s)
      _otherwise -> Nothing
