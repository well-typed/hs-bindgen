{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoNamedFieldPuns  #-}
{-# LANGUAGE NoRecordWildCards #-}

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

import Data.Function
import Data.List qualified as List
import Data.Set qualified as Set
import System.FilePath qualified as FilePath
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Clang.Paths

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Boolean logic combining predicates that determine which declarations should
-- be kept
data Boolean a =
    -- | Match any declaration
    BTrue

    -- | Match no declaration
  | BFalse

    -- | Logical conjunction
  | BAnd (Boolean a) (Boolean a)

    -- | Logical disjunction
  | BOr (Boolean a) (Boolean a)

    -- | Logical negation
  | BNot (Boolean a)

    -- | Concrete predicates
  | BIf a
  deriving stock (Show, Eq, Generic)

instance Default a => Default (Boolean a) where
  def = BIf def

-- | Predicates matched against header paths
data HeaderPathPredicate =
    -- | Only include declarations in main headers (not included headers)
    FromMainHeaders

    -- | Only include declarations in headers in main header directories,
    -- including subdirectories
  | FromMainHeaderDirs

    -- | Match header path against regex
  | HeaderPathMatches Regex
  deriving stock (Show, Eq, Generic)

-- | Predicates matched against declarations themselves
data DeclPredicate =
    -- | Match declaration name against regex
    DeclNameMatches Regex
    -- | Match deprecated declarations taking current target platform into
    -- account; see 'Availability'
  | DeclDeprecated
  deriving stock (Show, Eq, Generic)

-- | Predicates for the @Parse@ pass
--
-- Parse predicates match against header file paths only.
--
-- The parse predicate and the select predicate both allow matching against
-- header paths but serve different purposes. The parse predicate dictates which
-- declarations `hs-bindgen` reifies into `hs-bindgen`-specific data structures,
-- the select predicate dictates which declarations `hs-bindgen` generates
-- bindings for. For details, please see the @hs-bindgen@ manual section on
-- predicates and program slicing.
data ParsePredicate =
    ParseHeader HeaderPathPredicate
  deriving stock (Show, Eq, Generic)

instance Default ParsePredicate where
  def = ParseHeader FromMainHeaderDirs

-- | Predicates for the @Select@ pass
--
-- Select predicates match against header file paths or the declarations
-- themselves.
--
-- The parse predicate and the select predicate both allow matching against
-- header paths but serve different purposes. The parse predicate dictates which
-- declarations `hs-bindgen` reifies into `hs-bindgen`-specific data structures,
-- the select predicate dictates which declarations `hs-bindgen` generates
-- bindings for. For details, please see the @hs-bindgen@ manual section on
-- predicates and program slicing.
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
  -> C.DeclName
  -> C.Availability
  -> Boolean SelectPredicate
  -> Bool
matchSelect isMainHeader isInMainHeaderDir path declName availability = eval $ \case
    SelectHeader p -> matchHeaderPath isMainHeader isInMainHeaderDir path p
    SelectDecl   p -> matchDecl declName availability p

{-------------------------------------------------------------------------------
  Merging
-------------------------------------------------------------------------------}

-- | Merge lists of negative and positive Booleans
--
-- Combine the negative Booleans using AND, and the positive Booleans using OR.
mergeBooleans :: forall a. Eq a => [Boolean a] -> [Boolean a] -> Boolean a
mergeBooleans negatives positives =
    reduce $ BAnd neg pos
  where
    mergeNeg, mergePos :: Boolean a -> Boolean a -> Boolean a
    mergeNeg p q = reduce $ BAnd (reduce $ BNot $ reduce p) q
    mergePos p q = reduce $ BOr (reduce p) q

    neg, pos :: Boolean a
    neg = foldr mergeNeg BTrue  negatives
    pos = foldr mergePos BFalse positives

{-------------------------------------------------------------------------------
  Internal auxiliary: execution
-------------------------------------------------------------------------------}

-- | Boolean logic reduction
--
-- * This is /not/ recursive: we call this at every step in 'eval'
-- * This needs to match the semantics of 'eval' precisely.
reduce :: Eq a => Boolean a -> Boolean a
reduce = \case
    BNot (BNot p) -> p
    BNot BTrue    -> BFalse
    BNot BFalse   -> BTrue
    --
    BAnd BTrue q -> q
    BAnd p BTrue -> p
    BAnd p q | p == BFalse || q == BFalse -> BFalse
    --
    BOr BFalse q -> q
    BOr p BFalse -> p
    BOr p q | p == BTrue || q == BTrue -> BTrue
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
      BTrue        -> True
      BFalse       -> False
      BAnd   p1 p2 -> go p1 && go p2
      BOr    p1 p2 -> go p1 || go p2
      BNot   p1    -> not (go p1)
      BIf    p1    -> f p1

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
matchDecl :: C.DeclName -> C.Availability -> DeclPredicate -> Bool
matchDecl declName availability = \case
    DeclNameMatches re -> matchTest re $ C.renderDeclName declName
    DeclDeprecated     -> isDeprecated
  where
    isDeprecated = case availability of
      C.Deprecated -> True
      _            -> False

{-------------------------------------------------------------------------------
  Internal auxiliary: regexs
-------------------------------------------------------------------------------}

-- | Perl-compatible regular expression
data Regex = Regex{
      string   :: String
    , compiled :: PCRE.Regex
    }

instance Eq Regex where
  (==) = (==) `on` (.string)

-- | Validity of the 'Show' instance depends on the 'IsString' instance
instance Show Regex where
  show regex = show regex.string

instance IsString Regex where
  fromString string = Regex{
        string   = string
      , compiled = PCRE.makeRegex string
      }

matchTest :: Regex -> Text -> Bool
matchTest regex = PCRE.matchTest regex.compiled
