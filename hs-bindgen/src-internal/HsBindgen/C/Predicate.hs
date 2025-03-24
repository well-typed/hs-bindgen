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
  , match
  ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text () -- instances only

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

    -- | Check if the definition is from the \"main\" file
    --
    -- Corresponds directly to @clang_Location_isFromMainFile@ in @libclang@;
    -- <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gacb4ca7b858d66f0205797ae84cc4e8f2>.
  | SelectFromMainFile

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

-- | Match filter
--
-- If the filter does not match, we report the reason why.
match ::
     SourcePath -- ^ Path of current main header file
  -> CXCursor
  -> SingleLoc
  -> Predicate
  -> IO (Either String ())
match mainSourcePath current sloc = runExceptT . go
  where
    go :: Predicate -> ExceptT String IO ()
    go SelectAll      = return ()
    go (SelectIfBoth p q) = go p >> go q

    go SelectFromMainFile =
        unless (singleLocPath sloc == mainSourcePath) $
          throwError $ "Not from the main file"

    go (SelectByFileName re) = do
        let filename = case singleLocPath sloc of
              SourcePath t -> t
        unless (matchTest re filename) $
          throwError $ mconcat [
              "File name "
            , show filename
            , " does not match "
            , show re
            ]

    go (SelectByElementName re) = do
        elementName <- liftIO $ clang_getCursorSpelling current
        unless (matchTest re elementName) $ do
          throwError $ mconcat [
              "Element name "
            , show elementName
            , " does not match "
            , show re
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
