-- | Select definitions from the C header
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Predicate (Predicate(..))
-- > import HsBindgen.Frontend.Predicate qualified as Predicate
module HsBindgen.Frontend.Predicate (
    -- * Definition
    Predicate (..)
  , HeaderPathPredicate (..)
  , DeclPredicate (..)
  , ParsePredicate
  , SelectPredicate
  , Regex -- opaque
    -- * Execution (internal API)
  , IsMainHeader
  , mkIsMainHeader
  , IsInMainHeaderDir
  , mkIsInMainHeaderDir
  , matchParse
  , matchSelect
    -- * Merging
  , mergePredicates
  ) where

import Data.List qualified as List
import Data.Set qualified as Set
import System.FilePath qualified as FilePath
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Predicate that determines which declarations should be kept
data Predicate a =
    -- | Match any declaration
    PTrue

    -- | Match no declaration
  | PFalse

    -- | Logical conjunction
  | PAnd (Predicate a) (Predicate a)

    -- | Logical disjunction
  | POr (Predicate a) (Predicate a)

    -- | Logical negation
  | PNot (Predicate a)

    -- | Concrete predicates
  | PIf a
  deriving (Show, Eq)

-- | Predicate that determines which declarations should be kept, based on
-- header paths
data HeaderPathPredicate =
    -- | Only include declarations in main headers (not included headers)
    FromMainHeaders

    -- | Only include declarations in headers in main header directories,
    -- including subdirectories
  | FromMainHeaderDirs

    -- | Match header path against regex
  | HeaderPathMatches Regex
  deriving (Show, Eq)

-- | Predicate that determines which declarations should be kept, based on the
-- declarations themselves
newtype DeclPredicate =
    -- | Match declaration name against regex
    DeclNameMatches Regex
  deriving (Show, Eq)

-- | Predicates for the @Parse@ pass select based on header file paths
type ParsePredicate = Predicate HeaderPathPredicate

instance Default ParsePredicate where
  def = PIf FromMainHeaderDirs

-- | Predicates for the @Select@ pass select based on header file paths or the
-- declarations themselves
--
-- NOTE: The parse predicate and the select predicate both allow matching
-- against header paths but serve different purposes. The parse predicate
-- dictates which declarations `hs-bindgen` reifies into `hs-bindgen`-specific
-- data structures, the selection predicate dictates which declarations
-- `hs-bindgen` generates bindings for. For details, please see the @hs-bindgen@
-- manual section on predicates and program slicing.
type SelectPredicate = Predicate (Either HeaderPathPredicate DeclPredicate)

instance Default SelectPredicate where
  def = PIf (Left FromMainHeaders)

{-------------------------------------------------------------------------------
  Execution

  NOTE: This is internal API (users construct filters, but don't use them).
-------------------------------------------------------------------------------}

-- | Check if a declaration is from one of the main headers
--
-- Dealing with main headers is somewhat subtle.  See
-- "HsBindgen.Frontend.ProcessIncludes" for discussion.
type IsMainHeader = SingleLoc -> Bool

-- | Construct an 'IsMainHeader' function for the given main header paths
mkIsMainHeader ::
     Set SourcePath -- ^ Main header paths
  -> IsMainHeader
mkIsMainHeader paths loc = singleLocPath loc `Set.member` paths

-- | Check if a declaration is in a main header directory, including
-- subdirectories
type IsInMainHeaderDir = SingleLoc -> Bool

-- | Construct an 'IsInMainHeaderDir' function for the given main header paths
mkIsInMainHeaderDir ::
     Set SourcePath -- ^ Main header paths
  -> IsInMainHeaderDir
mkIsInMainHeaderDir paths loc =
    let dir = FilePath.splitDirectories . FilePath.takeDirectory $
          getSourcePath (singleLocPath loc)
    in  any (`List.isPrefixOf` dir) mainDirs
  where
    mainDirs :: [[FilePath]]
    mainDirs = map FilePath.splitDirectories . Set.toList $
      Set.map (FilePath.takeDirectory . getSourcePath) paths

-- | Match 'ParsePredicate' predicates
--
-- * Built-ins don't have declarations, therefore /selecting/ them makes no
--   sense.   We never have to deal with built-ins when processing def sites
--   (@Parse.Decl@).
-- * We /may/ have to deal with built-ins at __use sites__ (@Parse.Type@), where
--   we'd have to special case them and map them to types we define, or indeed
--   deal with them in external bindings (they /do/ get added to
--   `nonSelectedDecls`, for example).
matchParse ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> SingleLoc
  -> PrelimDeclId
  -> ParsePredicate
  -> Bool
matchParse isMainHeader isInMainHeaderDir loc prelimDeclId
    | isBuiltin = const False
    | otherwise = eval (matchHeaderPath isMainHeader isInMainHeaderDir loc)
  where
    isBuiltin :: Bool
    isBuiltin = case prelimDeclId of
      PrelimDeclIdBuiltin{} -> True
      _otherwise            -> False

-- | Match 'SelectPredicate' predicates
matchSelect ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> SingleLoc
  -> C.QualDeclId
  -> SelectPredicate
  -> Bool
matchSelect isMainHeader isInMainHeaderDir loc qid = eval $
    either (matchHeaderPath isMainHeader isInMainHeaderDir loc) (matchDecl qid)

{-------------------------------------------------------------------------------
  Merging
-------------------------------------------------------------------------------}

-- | Merge lists of negative and positive predicates
--
-- Combine the negative predicates using AND, and the positive predicates using
-- OR.
mergePredicates :: Eq a => [Predicate a] -> [Predicate a] -> Predicate a
mergePredicates negatives positives =
    let mergeNeg p q = reduce $ PAnd (reduce $ PNot $ reduce p) q
        neg = foldr mergeNeg PTrue negatives
        mergePos p q = reduce $ POr (reduce p) q
        pos = foldr mergePos PFalse positives
     in reduce $ PAnd neg pos

{-------------------------------------------------------------------------------
  Internal auxiliary: execution
-------------------------------------------------------------------------------}

-- | Boolean logic reduction
--
-- * This is /not/ recursive: we call this at every step in 'eval'
-- * This needs to match the semantics of 'eval' precisely.
reduce :: Eq a => Predicate a -> Predicate a
reduce = \case
  PNot (PNot p) -> p
  PNot PTrue    -> PFalse
  PNot PFalse   -> PTrue
  --
  PAnd PTrue q -> q
  PAnd p PTrue -> p
  PAnd p q | p == PFalse || q == PFalse -> PFalse
  --
  POr PFalse q -> q
  POr p PFalse -> p
  POr p q | p == PTrue || q == PTrue -> PTrue
  --
  p -> p

-- | Evaluate a predicate
--
-- * This needs to match the semantics of 'reduce' precisely.  It should be OK
--   as long as /obvious/ boolean interpretations are used.
eval :: forall a.
     Eq a
  => (a -> Bool)  -- ^ Evaluation function for concrete predicates
  -> Predicate a
  -> Bool
eval f = go
  where
    go :: Predicate a -> Bool
    go p = case reduce p of
      PTrue        -> True
      PFalse       -> False
      PAnd   p1 p2 -> go p1 && go p2
      POr    p1 p2 -> go p1 || go p2
      PNot   p1    -> not (go p1)
      PIf    p1    -> f p1

-- | Match 'HeaderPathPredicate' predicates
matchHeaderPath ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> SingleLoc
  -> HeaderPathPredicate
  -> Bool
matchHeaderPath isMainHeader isInMainHeaderDir loc = \case
    FromMainHeaders      -> isMainHeader loc
    FromMainHeaderDirs   -> isInMainHeaderDir loc
    HeaderPathMatches re ->
      let (SourcePath path) = singleLocPath loc
       in matchTest re path

-- | Match 'DeclPredicate' predicates
matchDecl :: C.QualDeclId -> DeclPredicate -> Bool
matchDecl qid = \case
    DeclNameMatches re -> matchTest re $ C.qualDeclIdText qid

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

-- | Validity of the 'Show' instance depends on the 'IsString' instance
instance Show Regex where
  show = show . regexString

instance IsString Regex where
  fromString regexString = Regex{regexString, regexCompiled}
    where
      regexCompiled :: PCRE.Regex
      regexCompiled = PCRE.makeRegex regexString

matchTest :: Regex -> Text -> Bool
matchTest = PCRE.matchTest . regexCompiled
