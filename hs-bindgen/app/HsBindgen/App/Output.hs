{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | Output mode and category selection options for @hs-bindgen@
module HsBindgen.App.Output (
    -- * Output mode
    OutputMode(..)
  , parseOutputMode
    -- * Single-file category selection
  , SingleFileCategory(..)
  , parseSingleFileCategories
    -- * Output options
  , OutputOptions(..)
  , parseOutputOptions
    -- * Builder
  , buildCategoryChoice
  ) where

import Data.Default (Default (..))
import Data.List.NonEmpty (NonEmpty (..), some1)
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative

import HsBindgen.Backend.Category (ByCategory (..), CategoryLvl (..),
                                   Choice (..), RenameTerm (..))

{-------------------------------------------------------------------------------
  Output mode
-------------------------------------------------------------------------------}

-- | Output mode (single-file vs multi-module)
data OutputMode =
    FilePerModule
      -- ^ One file per category (default)
  | SingleFile (NonEmpty SingleFileCategory)
      -- ^ Single file combining categories
  deriving (Show, Eq)

-- | Parse output mode
parseOutputMode :: OutputMode -> Parser OutputMode
parseOutputMode defMode =
  asum [
    (flag' FilePerModule $ mconcat [
        long "file-per-module"
      , help "Generate one file per binding category (default)"
      ])
  , (flag' SingleFile $ mconcat [
        long "single-file"
      , help (  "Generate a single module file. If more than one category selected "
             ++ "they will be combined. If more than one of the same category is "
             ++ "selected only the last suffix is used."
             )
      ]) <*> parseSingleFileCategories
  , pure defMode
  ]

{-------------------------------------------------------------------------------
  Single-file category selection
-------------------------------------------------------------------------------}

-- | Which category to include in single-file mode (required when mode=SingleFile)
data SingleFileCategory =
    SingleFileSafe Text
      -- ^ Safe only, with optional suffix (empty = none)
  | SingleFileUnsafe Text
      -- ^ Unsafe only, with optional suffix
  | SingleFilePointer Text
      -- ^ Pointer only, with optional suffix
  deriving (Show, Eq)

-- | Parse single-file category selections (one or more required)
parseSingleFileCategories :: Parser (NonEmpty SingleFileCategory)
parseSingleFileCategories = some1 parseSingleFileCategory
  where
    parseSingleFileCategory :: Parser SingleFileCategory
    parseSingleFileCategory =
          (SingleFileSafe <$> strOption (mconcat [
                long "safe"
              , metavar "SUFFIX"
              , help "Include safe bindings (empty suffix for no suffix)"
              ]))
      <|> (SingleFileUnsafe <$> strOption (mconcat [
                long "unsafe"
              , metavar "SUFFIX"
              , help "Include unsafe bindings (empty suffix for no suffix)"
              ]))
      <|> (SingleFilePointer <$> strOption (mconcat [
                long "pointer"
              , metavar "SUFFIX"
              , help "Include pointer bindings (empty suffix for no suffix)"
              ]))

{-------------------------------------------------------------------------------
  Output options
-------------------------------------------------------------------------------}

-- | Output options
newtype OutputOptions = OutputOptions { mode :: OutputMode }
  deriving (Show, Eq, Generic)

-- | Parse output options
parseOutputOptions :: OutputMode -> Parser OutputOptions
parseOutputOptions defMode = OutputOptions <$> parseOutputMode defMode

{-------------------------------------------------------------------------------
  Builder
-------------------------------------------------------------------------------}

-- | Build 'ByCategory' 'Choice' from output options
--
-- When multiple categories are selected, each category gets the specified suffix
-- to avoid duplicate symbols. When only one category is selected with an empty
-- suffix, no renaming occurs.
--
buildCategoryChoice :: OutputOptions -> ByCategory Choice
buildCategoryChoice opts = case opts.mode of
  FilePerModule       -> useAllCategories
  SingleFile cats     -> buildSingleFileChoice cats
  where
    -- Multi-module: all categories, no renaming
    useAllCategories :: ByCategory Choice
    useAllCategories = ByCategory {
          cType   = IncludeTypeCategory
        , cSafe   = IncludeTermCategory def
        , cUnsafe = IncludeTermCategory def
        , cFunPtr = IncludeTermCategory def
        , cGlobal = IncludeTermCategory def
        }

    -- Single-file: build based on category selections
    buildSingleFileChoice :: NonEmpty SingleFileCategory -> ByCategory Choice
    buildSingleFileChoice cats = ByCategory {
          cType   = IncludeTypeCategory
        , cSafe   = foldMap safeChoice cats
        , cUnsafe = foldMap unsafeChoice cats
        , cFunPtr = foldMap pointerChoice cats
        , cGlobal = IncludeTermCategory def
        }

    safeChoice :: SingleFileCategory -> Choice LvlTerm
    safeChoice = \case
      SingleFileSafe s    -> IncludeTermCategory (rename s)
      _                   -> ExcludeCategory

    unsafeChoice :: SingleFileCategory -> Choice LvlTerm
    unsafeChoice = \case
      SingleFileUnsafe s  -> IncludeTermCategory (rename s)
      _                   -> ExcludeCategory

    pointerChoice :: SingleFileCategory -> Choice LvlTerm
    pointerChoice = \case
      SingleFilePointer s -> IncludeTermCategory (rename s)
      _                   -> ExcludeCategory

    rename :: Text -> RenameTerm
    rename "" = def  -- Empty suffix = no renaming
    rename s  = RenameTerm (<> s)
