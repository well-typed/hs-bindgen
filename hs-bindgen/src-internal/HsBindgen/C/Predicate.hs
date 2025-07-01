-- | Select definitions from the C header
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Predicate (Predicate(..))
-- > import HsBindgen.C.Predicate qualified as Predicate
module HsBindgen.C.Predicate (
    Predicate (..)
  , Regex -- opaque
    -- * Trace messages
  , SkipReason (..)
  , Match (..)
    -- * Execution (this is internal API)
  , IsMainFile
  , match
  , matchPredicate
  ) where

import Data.Text qualified as Text
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.Imports
import HsBindgen.Language.C
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

    -- | Logical negation
    --
    -- Used to negate selection predicates
  | SelectNegate Predicate

    -- | Include definitions in main files (and not in included files)
  | SelectFromMainFiles

    -- | Match filename against regex
  | SelectByFileName Regex

    -- | Match element against regex
  | SelectByElementName Regex
  deriving (Show, Eq)

instance Semigroup Predicate where
  (<>) = SelectIfBoth

instance Monoid Predicate where
  mempty = SelectAll

{-------------------------------------------------------------------------------
  Trace messages (during matching)
-------------------------------------------------------------------------------}

data SkipReason =
    SkipBuiltin {
        skippedName :: Text
      }
  | SkipPredicate Match
  | SkipUnexposed {
        skippedLoc :: SingleLoc
      }
  deriving stock (Show, Eq)

data Match = Match {
          matchName   :: Text
        , matchLoc    :: SingleLoc
        , matchReason :: Text
      }
  deriving stock (Show, Eq)

instance PrettyForTrace SkipReason where
  prettyForTrace = \case
      SkipBuiltin{skippedName} -> hcat [
          "Skipped built-in: '"
        , textToCtxDoc skippedName
        , "'"
        ]
      (SkipPredicate Match{..}) -> hcat [
          "Skipped '"
        , textToCtxDoc matchName
        , "' at "
        , showToCtxDoc matchLoc
        , ": "
        , textToCtxDoc matchReason
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

type Skip = Match

type Include = Match

-- | Match predicate and skip built-ins
--
-- If the predicate does not match, we report the reason why.
match
  :: IsMainFile -> SingleLoc -> Maybe QualName
  -> Predicate -> Either SkipReason ()
match isMainFile loc mQualName predicate = do
    let skipBuiltIn :: Either SkipReason ()
        skipBuiltIn =
            when (nullSourcePath sourcePath) $
              Left SkipBuiltin{skippedName = matchName}
          where
            sourcePath :: SourcePath
            sourcePath = singleLocPath loc

        matchName :: Text
        matchName = maybe "anonymous declaration" qualNameText mQualName

    skipBuiltIn
    bimap SkipPredicate (const ()) (matchPredicate isMainFile loc mQualName predicate)

-- | Match predicate
matchPredicate
  :: IsMainFile -> SingleLoc -> Maybe QualName
  -> Predicate -> Either Skip Include
matchPredicate isMainFile loc mQualName = go
  where skip :: Text -> Either Skip Include
        skip = Left . getMatch

        include :: Text -> Either Skip Include
        include = Right . getMatch

        getMatch :: Text -> Match
        getMatch matchReason =
            Match{
                matchName
              , matchLoc = loc
              , matchReason
              }

        matchName :: Text
        matchName = maybe "anonymous declaration" qualNameText mQualName

        go :: Predicate -> Either Skip Include
        go = \case
            SelectAll -> include "Select all declarations"
            (SelectIfBoth p1 p2) -> go p1 >> go p2
            (SelectNegate p1) -> case go p1 of
              Left  skp -> include $ "Negation of: " <> matchReason skp
              Right inc -> skip $ "Negation of: " <> matchReason inc
            SelectFromMainFiles
              | isMainFile loc -> include "From main files"
              | otherwise      -> skip "Not from main files"
            (SelectByFileName re)
              | matchTest re filename -> include (msg "matches")
              | otherwise             -> skip (msg "does not match")
                where
                  (SourcePath filename) = singleLocPath loc
                  msg verb = mconcat [ "File name '" <> filename <> "' "
                                     , verb <> " " <> Text.pack (show re)
                                     ]
            (SelectByElementName re) -> case qualNameText <$> mQualName of
              Just qualElementName
                | matchTest re qualElementName -> include (msg "matches")
              _noMatchOrAnon -> skip (msg "does not match")
              where
                msg verb = mconcat [ "Element name '" <> matchName <> "' "
                                   , verb <> " " <> Text.pack (show re)
                                   ]

{-------------------------------------------------------------------------------
  Internal auxiliary: regexs
-------------------------------------------------------------------------------}

-- | Perl-compatible regular expression
data Regex = Regex {
      regexString   :: String
    , regexCompiled :: PCRE.Regex
    }

instance Eq Regex where
  x == y = regexString x == regexString y

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
