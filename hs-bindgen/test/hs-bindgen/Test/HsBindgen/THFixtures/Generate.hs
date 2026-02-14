{-# LANGUAGE OverloadedRecordDot #-}

-- | Generate TH modules for compilation testing
--
-- This module generates Haskell source files that use TH to generate bindings,
-- similar to the pattern used in Test.TH.Test01.
--
module Test.HsBindgen.THFixtures.Generate (
    generateModule
  ) where

import System.FilePath ((</>))

import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal
import HsBindgen.Imports

import Test.HsBindgen.Golden.TestCase (TestCase (..))

{-------------------------------------------------------------------------------
  Module generation
-------------------------------------------------------------------------------}

-- | Generate a TH module source that will produce bindings when compiled
--
generateModule :: FilePath -> TestCase -> String
generateModule pkgRoot tc = unlines $ concat [
    languagePragmas
  , [""]
  , moduleHeader
  , [""]
  , imports
  , [""]
  , letBlock
  ]
  where
    includeDir :: FilePath
    includeDir = pkgRoot </> tc.inputDir

    -- Target triple + any test-specific clang args (e.g. -std=c2x)
    --
    -- We pin to x86_64-pc-linux-musl for the same reason the golden tests do
    -- (see 'mkTestClangArgsConfig' in "Test.HsBindgen.Resources").
    --
    allArgsBefore :: [String]
    allArgsBefore = targetArgs ++ extractArgsBefore tc.onBoot

    targetArgs :: [String]
    targetArgs = ["-target", "x86_64-pc-linux-musl"]

    enableBlocks :: Bool
    enableBlocks = extractEnableBlocks tc.onBoot

    bindingSpecLines :: [String]
    bindingSpecLines = concat [
        externalSpecLines tc
      , prescriptiveSpecLines tc
      ]

    languagePragmas :: [String]
    languagePragmas =
        [ "{-# LANGUAGE CApiFFI #-}"
        , "{-# LANGUAGE DataKinds #-}"
        , "{-# LANGUAGE DerivingStrategies #-}"
        , "{-# LANGUAGE DerivingVia #-}"
        , "{-# LANGUAGE MagicHash #-}"
        , "{-# LANGUAGE OverloadedLabels #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# LANGUAGE PatternSynonyms #-}"
        , "{-# LANGUAGE TemplateHaskell #-}"
        , "{-# LANGUAGE TypeFamilies #-}"
        , "{-# LANGUAGE UnboxedTuples #-}"
        , "{-# LANGUAGE UndecidableInstances #-}"
        ]

    moduleHeader :: [String]
    moduleHeader =
        [ "module Example where"
        ]

    imports :: [String]
    imports = concat [
        [ "import Optics ((%), (&), (.~))"
        , ""
        , "import HsBindgen.Runtime.Prelude qualified"
        , ""
        , "import HsBindgen.Runtime.LibC qualified"
        , ""
        , "import HsBindgen.TH"
        , ""
        , "import Foreign.C.Types"
        ]
      ]

    thCategoryChoiceLines :: [String]
    thCategoryChoiceLines =
        [ "    thCategoryChoice :: ByCategory Choice"
        , "    thCategoryChoice = ByCategory"
        , "      { cType   = IncludeTypeCategory"
        , "      , cSafe   = IncludeTermCategory $ RenameTerm (<> \"_safe\")"
        , "      , cUnsafe = IncludeTermCategory $ RenameTerm (<> \"_unsafe\")"
        , "      , cFunPtr = IncludeTermCategory $ RenameTerm (<> \"_funptr\")"
        , "      , cGlobal = IncludeTermCategory def"
        , "      }"
        ]

    letBlock :: [String]
    letBlock = concat [
        [ "let cfg :: Config"
        , "    cfg = def"
        , "      & #clang % #extraIncludeDirs .~ [Dir " ++ show includeDir ++ ", Pkg \"musl-include/x86_64\"]"
        , "      & #clang % #argsBefore .~ " ++ show allArgsBefore
        ]
      , [ "      & #clang % #enableBlocks .~ True"
        | enableBlocks
        ]
      , bindingSpecLines
      , thCategoryChoiceLines
      , [ "    cfgTh :: ConfigTH"
        , "    cfgTh = def"
        , "      & #categoryChoice .~ thCategoryChoice"
        , " in withHsBindgen cfg cfgTh $"
        , "      hashInclude " ++ show tc.inputHeader
        ]
      ]

-- | Extract extra @argsBefore@ from @onBoot@ configuration
--
extractArgsBefore :: (BootConfig -> BootConfig) -> [String]
extractArgsBefore onBoot =
    let BootConfig{clangArgs = ClangArgsConfig{argsBefore = args}} = onBoot def
    in args

-- | Extract @enableBlocks@ from @onBoot@ configuration
--
extractEnableBlocks :: (BootConfig -> BootConfig) -> Bool
extractEnableBlocks onBoot =
    let BootConfig{clangArgs = ClangArgsConfig{enableBlocks = blocks}} = onBoot def
    in blocks

-- | Generate external binding spec configuration lines
--
externalSpecLines :: TestCase -> [String]
externalSpecLines tc
    | null tc.specExternal = []
    | otherwise =
        [ "      & #bindingSpec % #extBindingSpecs .~ " ++ show tc.specExternal
        ]

-- | Generate prescriptive binding spec configuration lines
--
prescriptiveSpecLines :: TestCase -> [String]
prescriptiveSpecLines tc = case tc.specPrescriptive of
    Nothing -> []
    Just path ->
        [ "      & #bindingSpec % #prescriptiveBindingSpec .~ Just " ++ show path
        ]
