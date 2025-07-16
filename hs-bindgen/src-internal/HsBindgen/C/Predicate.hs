-- | Select definitions from the C header
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Predicate (Predicate(..))
-- > import HsBindgen.C.Predicate qualified as Predicate
module HsBindgen.C.Predicate (
    Predicate (..)
  , Regex -- opaque
    -- * Execution (internal API)
  , IsMainFile
  , match
    -- * Merging
  , mergePredicates
  ) where

import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Clang.HighLevel.Types
import Clang.Paths
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

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

instance Default Predicate where
  def :: Predicate
  def = SelectFromMainFiles

{-------------------------------------------------------------------------------
  Match

  NOTE: This is internal API (users construct filters, but don't use them).
-------------------------------------------------------------------------------}

-- | Check if a declaration is from one of the main files
--
-- This check is somewhat subtle, and we punt on the precise implementation in
-- this module. See "HsBindgen.Frontend.ProcessIncludes" for discussion.
type IsMainFile = SingleLoc -> Bool

-- | Match predicate
--
-- NOTE: We never select builtins:
--
-- * Builtins don't have declarations, therefore "selecting" them makes no
--   sense. We will never have to deal with builtins when processing def sites
--   (`Parse.Decl`)
-- * We /may/ have to deal with builtins at _use sites_ (`Parse.Type`), where
--   we'd have to special case them and map them to types we define, or indeed
--   deal with them in external bindings (they _do_ get added to
--   `nonSelectedDecls`, for example).
match ::
     IsMainFile
  -> SingleLoc
  -> QualPrelimDeclId
  -> Predicate
  -> Bool
match isMainFile loc qid = \p ->
       not (isBuiltin qid)
    && go p
  where
    go :: Predicate -> Bool
    go p =
        -- NOTE: The semantics we implement here /MUST/ line up with @reduce@.
        --
        -- As long the interpretation of
        --
        -- o 'SelectAll'
        -- o 'SelectNone'
        -- o 'SelectIfBoth'
        -- o 'SelectIfEither'
        -- o 'SelectNegate'
        --
        -- is just the "obvious" boolean interpretation, this will be ok.
        case reduce p of
          -- Boolean logic
          SelectAll            -> True
          SelectNone           -> False
          SelectIfBoth   p1 p2 -> go p1 && go p2
          SelectIfEither p1 p2 -> go p1 || go p2
          SelectNegate   p1    -> not (go p1)
          -- Special cases
          SelectFromMainFiles    -> isMainFile loc
          SelectByFileName re    -> matchFilename    re $ singleLocPath loc
          SelectByElementName re -> matchElementName re $ qid

    matchFilename :: Regex -> SourcePath -> Bool
    matchFilename re (SourcePath path) = matchTest re path

    matchElementName :: Regex -> QualPrelimDeclId -> Bool
    matchElementName re = \case
      QualPrelimDeclIdNamed name kind ->
        matchTest re (C.qualNameText $ C.QualName name kind)
      _otherwise -> False

isBuiltin :: QualPrelimDeclId -> Bool
isBuiltin = \case
    QualPrelimDeclIdBuiltin{} -> True
    _otherwise                -> False

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

-- | Boolean logic reduction (internal API)
--
-- NOTE:
--
-- * This is /not/ recursive: we call this at every step in 'match'
-- * This needs to match the semantics of 'match' precisely.
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
