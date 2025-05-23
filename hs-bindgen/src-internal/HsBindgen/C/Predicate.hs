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
  , SkipReason (..)
  , match
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Text qualified as Text
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import HsBindgen.Imports

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
  Matching

  NOTE: This is internal API (users construct filters, but don't use them).
-------------------------------------------------------------------------------}

data SkipReason = SkipBuiltIn | SkipPredicate { reason :: Text }

-- | Match filter
--
-- If the filter does not match, we report the reason why.
match :: forall m.
     MonadIO m
  => Set SourcePath -- ^ Paths of main files
  -> CXCursor
  -> SingleLoc
  -> Predicate
  -> m (Either SkipReason ())
match mainFilePaths current sloc predicate = runExceptT (go predicate <* skipBuiltIn)
  where
    skipBuiltIn :: ExceptT SkipReason m ()
    skipBuiltIn = let sourcePath = singleLocPath sloc in
      when (nullSourcePath sourcePath) $ throwError $ SkipBuiltIn
    go :: Predicate -> ExceptT SkipReason m ()
    go SelectAll      = pure ()
    go (SelectIfBoth p q) = go p >> go q
    go SelectFromMainFiles = do
        unless (any (equalSourcePath (singleLocPath sloc)) mainFilePaths) $
          throwError $ SkipPredicate "Not from main files"
    go (SelectByFileName re) = do
        let filename = case singleLocPath sloc of
              SourcePath t -> t
        unless (matchTest re filename) $
          throwError $ SkipPredicate $ mconcat [
              "File name '"
            , filename
            , "' does not match "
            , Text.pack $ show re
            ]
    go (SelectByElementName re) = do
        elementName <- clang_getCursorSpelling current
        unless (matchTest re elementName) $ do
          throwError $ SkipPredicate $ mconcat [
              "Element name '"
            , elementName
            , "' does not match "
            , Text.pack $ show re
            ]

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
