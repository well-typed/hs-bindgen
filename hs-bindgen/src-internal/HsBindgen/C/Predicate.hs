-- | Select definitions from the C header
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Predicate (Predicate(..))
-- > import HsBindgen.C.Predicate qualified as Predicate
module HsBindgen.C.Predicate (
    Predicate(..)
  , Regex -- opaque
    -- * Execution (this is internal API)
  , IsMainFile
  , SkipReason (..)
  , match
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Text qualified as Text
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.Imports
import HsBindgen.Util.Tracer
import Text.SimplePrettyPrint (hcat, showToCtxDoc, textToCtxDoc)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Select which definitions in the C header(s) we want to keep
data Predicate =
    -- | The filter that always matches
    --
    -- Used to define @mempty@.
    SelectAll

    -- | Logical conjunction
    --
    -- Used to define the 'Semigroup' instance.
  | SelectIfBoth Predicate Predicate

    -- | Include definitions in main files (and not in included files)
  | SelectFromMainFiles

    -- | Match filename against regex
  | SelectByFileName Regex

    -- | Match element against regex
  | SelectByElementName Regex
  deriving (Show)

instance Semigroup Predicate where
  (<>) = SelectIfBoth

instance Monoid Predicate where
  mempty = SelectAll

{-------------------------------------------------------------------------------
  Log messages (during matching)
-------------------------------------------------------------------------------}

data SkipReason =
    SkipBuiltin {
         skippedName :: Text
       }
  | SkipPredicate {
        skippedName   :: Text
      , skippedLoc    :: SingleLoc
      , skippedReason :: Text
      }
  | SkipUnexposed {
        skippedLoc :: SingleLoc
      }
  deriving stock (Show, Eq)

instance PrettyForTrace SkipReason where
  prettyForTrace = \case
      SkipBuiltin{skippedName} -> hcat [
          "Skipped built-in: "
        , showToCtxDoc skippedName
        ]
      SkipPredicate{..} -> hcat [
          "Skipped "
        , textToCtxDoc skippedName
        , " at "
        , showToCtxDoc skippedLoc
        , ": "
        , textToCtxDoc skippedReason
        ]
      SkipUnexposed{skippedLoc} -> hcat [
          "Skipped unexposed declaration at "
        , showToCtxDoc skippedLoc
        ]

instance HasDefaultLogLevel SkipReason where
  getDefaultLogLevel = \case
      SkipBuiltin{}   -> Debug
      SkipPredicate{} -> Info
      SkipUnexposed{} -> Warning

{-------------------------------------------------------------------------------
  Matching

  NOTE: This is internal API (users construct filters, but don't use them).
-------------------------------------------------------------------------------}

-- | Check if a declaration is from one of the main files
--
-- This check is somewhat subtle, and we punt on the precise implementation in
-- this module. See "HsBindgen.Frontend.ProcessIncludes" for discussion.
type IsMainFile = SingleLoc -> Bool

-- | Match filter
--
-- If the filter does not match, we report the reason why.
match :: forall m.
     MonadIO m
  => IsMainFile
  -> Predicate
  -> CXCursor -> m (Either SkipReason ())
match isMainFile predicate curr = runExceptT $ do
    loc <- HighLevel.clang_getCursorLocation' curr

    let skipBuiltIn :: ExceptT SkipReason m ()
        skipBuiltIn =
            when (nullSourcePath sourcePath) $ do
              skippedName <- clang_getCursorSpelling curr
              throwError SkipBuiltin{skippedName}
          where
            sourcePath :: SourcePath
            sourcePath = singleLocPath loc

        skip :: Text -> ExceptT SkipReason m a
        skip skippedReason = do
              skippedName <- clang_getCursorSpelling curr
              throwError SkipPredicate{
                  skippedName
                , skippedReason
                , skippedLoc = loc
                }

        go :: Predicate -> ExceptT SkipReason m ()
        go SelectAll =
            pure ()
        go (SelectIfBoth p q) = do
            go p
            go q
        go SelectFromMainFiles = do
            unless (isMainFile loc) $
              skip "Not from main files"
        go (SelectByFileName re) = do
            let filename = case singleLocPath loc of SourcePath t -> t
            unless (matchTest re filename) $
              skip $ mconcat [
                  "File name '"
                , filename
                , "' does not match "
                , Text.pack $ show re
                ]
        go (SelectByElementName re) = do
            elementName <- clang_getCursorSpelling curr
            unless (matchTest re elementName) $ do
              skip $ mconcat [
                  "Element name '"
                , elementName
                , "' does not match "
                , Text.pack $ show re
                ]


    skipBuiltIn
    go predicate

{-------------------------------------------------------------------------------
  Internal auxiliary: regexs
-------------------------------------------------------------------------------}

-- | Perl-compatible regular expression
data Regex = Regex {
      regexString   :: String
    , regexCompiled :: PCRE.Regex
    }

-- | Validatity of the 'Show' instance depends on the 'IsString' instance
instance Show Regex where
  show = show . regexString

instance IsString Regex where
  fromString regexString = Regex{regexString, regexCompiled}
    where
      regexCompiled :: PCRE.Regex
      regexCompiled = PCRE.makeRegex regexString

matchTest :: Regex -> Text -> Bool
matchTest = PCRE.matchTest . regexCompiled
