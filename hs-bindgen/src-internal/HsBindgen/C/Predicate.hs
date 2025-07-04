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
    -- * Execution (internal API)
  , IsMainFile
  , match
  , matchPredicate
    -- * Merging
  , mergePredicates
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
    -- Base case when combining predicates with AND.
    SelectAll

    -- | The filter that never matches
    --
    -- Base case when combining predicates with OR.
  | SelectNone

    -- | Logical conjunction
    --
    -- Used to combine negated predicates.
  | SelectIfBoth Predicate Predicate

    -- | Logical disjunction
    --
    -- Used to combine positive predicates.
  | SelectIfEither Predicate Predicate

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
  Match

  NOTE: This is internal API (users construct filters, but don't use them).
-------------------------------------------------------------------------------}

-- | Check if a declaration is from one of the main files
--
-- This check is somewhat subtle, and we punt on the precise implementation in
-- this module. See "HsBindgen.Frontend.ProcessIncludes" for discussion.
type IsMainFile = SingleLoc -> Bool


type SkipMsg = Text

type SelectMsg = Text

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

        getMatch :: Text -> Match
        getMatch matchReason =
            Match{
                matchName
              , matchLoc = loc
              , matchReason
              }

        matchName :: Text
        matchName = maybe "anonymous declaration" qualNameText mQualName

    skipBuiltIn
    bimap (SkipPredicate . getMatch) (const ()) $
      matchPredicate isMainFile loc mQualName predicate

-- | Match predicate
matchPredicate
  :: IsMainFile -> SingleLoc -> Maybe QualName
  -> Predicate -> Either SkipMsg SelectMsg
matchPredicate isMainFile loc mQualName = go
  where skip, select :: Text -> Either SkipMsg SelectMsg
        skip = Left
        select = Right

        matchName :: Text
        matchName = maybe "anonymous declaration" qualNameText mQualName

        go :: Predicate -> Either SkipMsg SelectMsg
        go p = case reduce p of
            SelectAll -> select "Select all declarations"
            SelectNone -> skip "Select no declaration"
            (SelectIfBoth p1 p2) -> go p1 >> go p2
            (SelectIfEither p1 p2) -> case go p1 of
              Right sel1 -> select sel1
              Left  skp1 -> case go p2 of
                Right sel2 -> select sel2
                Left _ -> skip skp1
            (SelectNegate p1) -> case go p1 of
              Left  skp -> select $ "Negation of: " <> skp
              Right sel -> skip $ "Negation of: " <> sel
            SelectFromMainFiles
              | isMainFile loc -> select "From main files"
              | otherwise      -> skip "Not from main files"
            (SelectByFileName re)
              | matchTest re filename -> select (msg "matches")
              | otherwise             -> skip (msg "does not match")
                where
                  (SourcePath filename) = singleLocPath loc
                  msg verb = mconcat [ "File name '" <> filename <> "' "
                                     , verb <> " " <> Text.pack (show re)
                                     ]
            (SelectByElementName re) -> case qualNameText <$> mQualName of
              Just qualElementName
                | matchTest re qualElementName -> select (msg "matches")
              _noMatchOrAnon -> skip (msg "does not match")
              where
                msg verb = mconcat [ "Element name '" <> matchName <> "' "
                                   , verb <> " " <> Text.pack (show re)
                                   ]

{-------------------------------------------------------------------------------
  Reduce and merge
-------------------------------------------------------------------------------}

-- | Merge lists of negative and positive predicates
--
-- Combine the negative predicates using AND, and the positive predicates using
-- OR.
mergePredicates :: [Predicate] -> [Predicate] -> Predicate
mergePredicates negatives positives =
    let mergeNeg p q = reduce $ SelectIfBoth (reduce $ SelectNegate $ reduce p) q
        neg = foldr mergeNeg SelectAll negatives
        mergePos p q = reduce $ SelectIfEither (reduce p) q
        pos = foldr mergePos SelectNone positives
     in reduce $ SelectIfBoth neg pos

-- Boolean logic reduction (internal API)
reduce :: Predicate -> Predicate
reduce = \case
  (SelectNegate (SelectNegate p)) -> p
  (SelectNegate SelectAll)        -> SelectNone
  (SelectNegate SelectNone)       -> SelectAll
  --
  (SelectIfBoth SelectAll q) -> q
  (SelectIfBoth p SelectAll) -> p
  (SelectIfBoth p q)
    | p == SelectNone || q == SelectNone -> SelectNone
  --
  (SelectIfEither SelectNone q) -> q
  (SelectIfEither p SelectNone) -> p
  (SelectIfEither p q)
    | p == SelectAll  || q == SelectAll -> SelectAll
  --
  p -> p

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
