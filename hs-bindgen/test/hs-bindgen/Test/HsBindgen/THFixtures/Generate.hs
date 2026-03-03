{-# LANGUAGE OverloadedRecordDot #-}

-- | Generate TH modules for compilation testing
--
-- This module generates Haskell source files that use TH to generate bindings,
-- similar to the pattern used in Test.TH.Test01.
module Test.HsBindgen.THFixtures.Generate (
    generateModule
  ) where

import Data.List qualified as List

import HsBindgen.BindingSpec
import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal

import Test.HsBindgen.Golden.TestCase (TestCase (..), getTestBootConfig)
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Module generation
-------------------------------------------------------------------------------}

-- | Generate a TH module source that will produce bindings when compiled
generateModule :: TestResources -> TestCase -> String
generateModule testResources tc = unlines $ List.intercalate [""] [
      languagePragmas
    , moduleHeader bootConfig
    , imports
    , letBlock bootConfig tc.inputHeader
    ]
  where
    bootConfig :: BootConfig
    bootConfig = getTestBootConfig tc testResources

languagePragmas :: [String]
languagePragmas = [
      "{-# LANGUAGE CApiFFI #-}"
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

moduleHeader :: BootConfig -> [String]
moduleHeader bootConfig = List.singleton $ unwords [
      "module"
    , baseModuleNameToString bootConfig.baseModule
    , "where"
    ]

imports :: [String]
imports = [
      "import Foreign.C.Types"
    , "import HsBindgen.Runtime.LibC qualified"
    , "import HsBindgen.Runtime.Prelude qualified"
    , "import HsBindgen.TH"
    , "import Optics ((%), (&), (.~))"
    ]

letBlock :: BootConfig -> FilePath -> [String]
letBlock bootConfig inputHeader = [
      "let cfg :: Config"
    , "    cfg = def"
    , "      & #clang % #builtinIncDir                 .~ " ++ builtinIncDir'
    , "      & #clang % #extraIncludeDirs              .~ " ++ extraIncludeDirs'
    , "      & #clang % #defineMacros                  .~ " ++ defineMacros'
    , "      & #clang % #enableBlocks                  .~ " ++ enableBlocks'
    , "      & #clang % #argsBefore                    .~ " ++ argsBefore'
    , "      & #clang % #argsInner                     .~ " ++ argsInner'
    , "      & #clang % #argsAfter                     .~ " ++ argsAfter'
    , "      & #bindingSpec % #extBindingSpecs         .~ " ++ extBindingSpecs'
    , "      & #bindingSpec % #prescriptiveBindingSpec .~ " ++ prescriptiveBindingSpec'
    , "    thCategoryChoice :: ByCategory Choice"
    , "    thCategoryChoice = ByCategory{"
    , "        cType   = IncludeTypeCategory"
    , "      , cSafe   = IncludeTermCategory $ RenameTerm (<> \"_safe\")"
    , "      , cUnsafe = IncludeTermCategory $ RenameTerm (<> \"_unsafe\")"
    , "      , cFunPtr = IncludeTermCategory $ RenameTerm (<> \"_funptr\")"
    , "      , cGlobal = IncludeTermCategory def"
    , "      }"
    , "    cfgTh :: ConfigTH"
    , "    cfgTh = def"
    , "      & #categoryChoice .~ thCategoryChoice"
    , " in withHsBindgen cfg cfgTh $"
    , "      hashInclude " ++ show inputHeader
    ]
  where
    -- All type 'String':
    builtinIncDir' = show bootConfig.clangArgs.builtinIncDir
    extraIncludeDirs' = ("[" ++) . (++ "]") $ List.intercalate ", " [
        "Dir " ++ show includeDir
      | includeDir <- bootConfig.clangArgs.extraIncludeDirs
      ]
    defineMacros' = show bootConfig.clangArgs.defineMacros
    enableBlocks' = show bootConfig.clangArgs.enableBlocks
    argsBefore' = show bootConfig.clangArgs.argsBefore
    argsInner' = show bootConfig.clangArgs.argsInner
    argsAfter' = show bootConfig.clangArgs.argsAfter
    extBindingSpecs' = show bootConfig.bindingSpec.extBindingSpecs
    prescriptiveBindingSpec' =
      show bootConfig.bindingSpec.prescriptiveBindingSpec
