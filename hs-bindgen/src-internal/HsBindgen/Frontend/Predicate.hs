-- | Select definitions from the C header
module HsBindgen.Frontend.Predicate (
    -- * Booleans
    Boolean (..)
  , mergeBooleans
  , eval
    -- * Predicates
  , HeaderPathPredicate (..)
  , DeclPredicate (..)
  , SelectionPredicate (..)
  , Regex -- opaque
  , matchTest
    -- * Execution (internal API)
  , IsMainHeader
  , mkIsMainHeader
  , IsInMainHeaderDir
  , mkIsInMainHeaderDir
  , matchSelect
  ) where

import Data.Function
import Data.List qualified as List
import Data.Set qualified as Set
import System.FilePath qualified as FilePath
import Text.Regex.PCRE qualified as PCRE
import Text.Regex.PCRE.Text ()

import Data.Text qualified as Text

import HsBindgen.Imports
import HsBindgen.IR.C (HeaderName)
import HsBindgen.IR.C qualified as C

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
    -- account; see 'HsBindgen.Frontend.AST.Decl.Availability'
  | DeclDeprecated
  deriving stock (Show, Eq, Generic)

-- | Predicates for the @Select@ pass
--
-- Selection predicates match against header file paths or the declarations
-- themselves.
--
-- The selection predicate dictates which declarations `hs-bindgen` generates
-- bindings for. For details, please see the @hs-bindgen@ manual section on
-- predicates and program slicing.
data SelectionPredicate =
    SelectHeader HeaderPathPredicate
  | SelectDecl   DeclPredicate
  deriving stock (Show, Eq, Generic)

instance Default SelectionPredicate where
  def = SelectHeader FromMainHeaders

{-------------------------------------------------------------------------------
  Execution

  NOTE: This is internal API (users construct filters, but don't use them).
-------------------------------------------------------------------------------}

-- | Check if a declaration is from one of the main headers
--
-- Dealing with main headers is somewhat subtle.  See
-- "HsBindgen.Frontend.ProcessIncludes" for discussion.
type IsMainHeader = HeaderName -> Bool

-- | Construct an 'IsMainHeader' function for the given main header names
mkIsMainHeader ::
     Set HeaderName -- ^ Main header names
  -> IsMainHeader
mkIsMainHeader names name = name `Set.member` names

-- | Check if a declaration is in a main header directory, including
-- subdirectories
type IsInMainHeaderDir = HeaderName -> Bool

-- | Construct an 'IsInMainHeaderDir' function for the given main header names
--
-- Directories are compared segment by segment, so @compat@ does not match
-- @compatibility@.
mkIsInMainHeaderDir ::
     Set HeaderName -- ^ Main header names
  -> IsInMainHeaderDir
mkIsInMainHeaderDir names name =
    any (`List.isPrefixOf` dirOf name) mainDirs
  where
    dirOf :: HeaderName -> [FilePath]
    dirOf =
        FilePath.splitDirectories . FilePath.takeDirectory
      . (.path) . C.headerNameArg

    mainDirs :: [[FilePath]]
    mainDirs = map dirOf (Set.toList names)

-- | Match 'SelectionPredicate' predicates
matchSelect ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> HeaderName
  -> C.DeclName
  -> C.Availability
  -> Boolean SelectionPredicate
  -> Bool
matchSelect isMainHeader isInMainHeaderDir name cDeclName availability = eval $ \case
    SelectHeader p -> matchHeaderPath isMainHeader isInMainHeaderDir name p
    SelectDecl   p -> matchDecl cDeclName availability p

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
--
-- 'HeaderPathMatches' sees the @#include@ argument alone, without the brackets
-- or quotes around it, which is the form existing patterns are written against.
matchHeaderPath ::
     IsMainHeader
  -> IsInMainHeaderDir
  -> HeaderName
  -> HeaderPathPredicate
  -> Bool
matchHeaderPath isMainHeader isInMainHeaderDir name = \case
    FromMainHeaders      -> isMainHeader name
    FromMainHeaderDirs   -> isInMainHeaderDir name
    HeaderPathMatches re ->
      matchTest re . Text.pack $ (C.headerNameArg name).path

-- | Match 'DeclPredicate' predicates
matchDecl :: C.DeclName -> C.Availability -> DeclPredicate -> Bool
matchDecl cDeclName availability = \case
    DeclNameMatches re -> matchTest re $ C.renderDeclName cDeclName
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
