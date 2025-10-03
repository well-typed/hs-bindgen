-- | Select definitions from the C header
module HsBindgen.Frontend.Predicate (
    -- * Booleans
    Boolean (..)
  , mergeBooleans
    -- * Predicates
  , HeaderPathPredicate (..)
  , DeclPredicate (..)
  , ParsePredicate (..)
  , SelectPredicate (..)
  , Regex -- opaque
    -- * Execution (internal API)
  , IsMainHeader
  , mkIsMainHeader
  , IsInMainHeaderDir
  , mkIsInMainHeaderDir
  , matchParse
  , matchSelect
  ) where

import Data.List qualified as List
import Data.Set qualified as Set
import System.FilePath qualified as FilePath
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Clang.Paths

import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Boolean logic combining predicates that determine which declarations should
-- be kept
data Boolean a =
    -- | Match any declaration
    PTrue

    -- | Match no declaration
  | PFalse

    -- | Logical conjunction
  | PAnd (Boolean a) (Boolean a)

    -- | Logical disjunction
  | POr (Boolean a) (Boolean a)

    -- | Logical negation
  | PNot (Boolean a)

    -- | Concrete predicates
  | PIf a
  deriving stock (Show, Eq, Generic)

instance Default a => Default (Boolean a) where
  def = PIf def

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
  deriving stock (Show, Eq, Generic)

-- | Predicate that determines which declarations should be kept, based on the
-- declarations themselves
data DeclPredicate =
    -- | Match declaration name against regex
    DeclNameMatches Regex
    -- | Match deprecated declarations taking current target platform into
    -- account; see 'Availability'
  | DeclDeprecated
  deriving stock (Show, Eq, Generic)

-- | Predicates for the @Parse@ pass select based on header file paths
data ParsePredicate = ParseHeader HeaderPathPredicate
  deriving stock (Show, Eq, Generic)

instance Default ParsePredicate where
  def = ParseHeader FromMainHeaderDirs

-- | Predicates for the @Select@ pass select based on header file paths or the
-- declarations themselves
--
-- NOTE: The parse predicate and the select predicate both allow matching
-- against header paths but serve different purposes. The parse predicate
-- dictates which declarations `hs-bindgen` reifies into `hs-bindgen`-specific
-- data structures, the selection predicate dictates which declarations
-- `hs-bindgen` generates bindings for. For details, please see the @hs-bindgen@
-- manual section on predicates and program slicing.
data SelectPredicate =
    SelectHeader HeaderPathPredicate
  | SelectDecl   DeclPredicate
  deriving stock (Show, Eq, Generic)

instance Default SelectPredicate where
  def = SelectHeader FromMainHeaders

{-------------------------------------------------------------------------------
  Execution

  NOTE: This is internal API (users construct filters, but don't use them).
-------------------------------------------------------------------------------}

-- | Check if a declaration is from one of the main headers
--
-- Dealing with main headers is somewhat subtle.  See
-- "HsBindgen.Frontend.ProcessIncludes" for discussion.
type IsMainHeader = SourcePath -> Bool

-- | Construct an 'IsMainHeader' function for the given main header paths
mkIsMainHeader ::
     Set SourcePath -- ^ Main header paths
  -> IsMainHeader
mkIsMainHeader paths path = path `Set.member` paths

-- | Check if a declaration is in a main header directory, including
-- subdirectories
type IsInMainHeaderDir = SourcePath -> Bool

-- | Construct an 'IsInMainHeaderDir' function for the given main header paths
mkIsInMainHeaderDir ::
     Set SourcePath -- ^ Main header paths
  -> IsInMainHeaderDir
mkIsInMainHeaderDir paths path =
    let dir = FilePath.splitDirectories . FilePath.takeDirectory $
          getSourcePath path
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
  -> SourcePath
  -> Boolean ParsePredicate
  -> Bool
matchParse isMainHeader isInMainHeaderDir path = eval $ \case
    ParseHeader p -> matchHeaderPath isMainHeader isInMainHeaderDir path p

-- | Match 'SelectPredicate' predicates
matchSelect ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> SourcePath
  -> C.QualDeclId
  -> C.Availability
  -> Boolean SelectPredicate
  -> Bool
matchSelect isMainHeader isInMainHeaderDir path qid availability = eval $ \case
    SelectHeader p -> matchHeaderPath isMainHeader isInMainHeaderDir path p
    SelectDecl   p -> matchDecl qid availability p

{-------------------------------------------------------------------------------
  Merging
-------------------------------------------------------------------------------}

-- | Merge lists of negative and positive Booleans
--
-- Combine the negative Booleans using AND, and the positive Booleans using
-- OR.
mergeBooleans :: Eq a => [Boolean a] -> [Boolean a] -> Boolean a
mergeBooleans negatives positives =
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
reduce :: Eq a => Boolean a -> Boolean a
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

-- | Evaluate a 'Boolean'
--
-- * This needs to match the semantics of 'reduce' precisely.  It should be OK
--   as long as /obvious/ boolean interpretations are used.
eval :: forall a.
     Eq a
  => (a -> Bool)  -- ^ Evaluation function for concrete Booleans
  -> Boolean a
  -> Bool
eval f = go
  where
    go :: Boolean a -> Bool
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
  -> SourcePath
  -> HeaderPathPredicate
  -> Bool
matchHeaderPath isMainHeader isInMainHeaderDir path@(SourcePath pathT) = \case
    FromMainHeaders      -> isMainHeader path
    FromMainHeaderDirs   -> isInMainHeaderDir path
    HeaderPathMatches re -> matchTest re pathT

-- | Match 'DeclPredicate' predicates
matchDecl :: C.QualDeclId -> C.Availability -> DeclPredicate -> Bool
matchDecl qid availability = \case
    DeclNameMatches re -> matchTest re $ C.qualDeclIdText qid
    DeclDeprecated     -> isDeprecated
  where
    isDeprecated = case availability of
      C.Deprecated -> True
      _            -> False

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
