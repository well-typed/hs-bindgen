{-# LANGUAGE OverloadedRecordDot #-}

-- | Generate TH modules for compilation testing
--
-- This module generates Haskell source files that use TH to generate bindings,
-- similar to the pattern used in Test.TH.Test01.
--
module Test.THFixtures.Generate (
    generateModule
  ) where

import Data.Default (Default (def))
import System.FilePath ((</>))

import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal

import Test.Common.HsBindgen.TestCase.Spec (TestCaseSpec (..))

{-------------------------------------------------------------------------------
  Module generation
-------------------------------------------------------------------------------}

-- | Generate a TH module source that will produce bindings when compiled
--
generateModule :: FilePath -> TestCaseSpec -> String
generateModule pkgRoot spec = unlines $
    -- Language extensions required for generated bindings
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
    , ""
    , "module Example where"
    , ""
    , "import Optics ((%), (&), (.~))"
    , ""
    , "import HsBindgen.Runtime.Prelude qualified"
    , ""
    , "import HsBindgen.TH"
    , ""
    , "import Foreign.C.Types"
    , ""
    , "let cfg :: Config"
    , "    cfg = def"
    , "      & #clang % #extraIncludeDirs .~ [Dir " ++ show includeDir ++ "]"
    ] ++ extraClangArgsLines ++
    bindingSpecLines ++
    [ "    cfgTh :: ConfigTH"
    , "    cfgTh = def"
    , " in withHsBindgen cfg cfgTh $"
    , "      hashInclude " ++ show spec.inputHeader
    ]
  where
    includeDir :: FilePath
    includeDir = pkgRoot </> spec.inputDir

    extraClangArgsLines :: [String]
    extraClangArgsLines = extractClangArgs spec.onBoot

    bindingSpecLines :: [String]
    bindingSpecLines = concat [
        externalSpecLines spec
      , prescriptiveSpecLines spec
      ]

-- | Extract extra clang arguments from onBoot configuration
--
extractClangArgs :: (BootConfig -> BootConfig) -> [String]
extractClangArgs onBoot =
    -- Apply the modifier to a default config and inspect clangArgs
    let cfg = onBoot def :: BootConfig
    in
    case cfg of
      BootConfig{clangArgs = ClangArgsConfig{argsBefore = args, enableBlocks = blocks}} ->
        concat [
            if null args then [] else ["      & #clang % #argsBefore .~ " ++ show args]
          , if blocks then ["      & #clang % #enableBlocks .~ True"] else []
          ]

-- | Generate external binding spec configuration lines
--
externalSpecLines :: TestCaseSpec -> [String]
externalSpecLines s
    | null s.specExternal = []
    | otherwise =
        [ "      & #bindingSpec % #extBindingSpecs .~ " ++ show s.specExternal
        ]

-- | Generate prescriptive binding spec configuration lines
--
prescriptiveSpecLines :: TestCaseSpec -> [String]
prescriptiveSpecLines s = case s.specPrescriptive of
    Nothing -> []
    Just path ->
        [ "      & #bindingSpec % #prescriptiveBindingSpec .~ Just " ++ show path
        ]
