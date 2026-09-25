-- |
--
-- Intended for unqualified import.
module HsBindgen.Macro.UniqueExpansion (
    -- * Parse
    ParseResult (..)
  , Error (..)
  , isFailure
  , liftDefinition
  , liftInvocation
  , parseDefinition
  , parseInvocation
    -- * Analysis
  , Analysis
  , analyseMacroDefinitions
  , redefinition
  , ambiguity
  , ambiguous
    -- * Unique expansion
  , isExpansionUnique
  ) where

import Control.Monad.Except (MonadError (throwError))
import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.Either (partitionEithers, rights)
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Text.Parsec (eof)

import Clang.HighLevel.Types (Token, TokenSpelling)

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

import HsBindgen.Macro.Error (MacroParseError)
import HsBindgen.Macro.Parse (isIdentifierOrKeyword, runParser, spelling)
import HsBindgen.Macro.Syntax (MacroDefinition (macro, name),
                               MacroInvocation (name, tokens))
import HsBindgen.Macro.UniqueExpansion.Parse qualified as P
import HsBindgen.Macro.UniqueExpansion.Types

{-------------------------------------------------------------------------------
  Parse
-------------------------------------------------------------------------------}

data ParseResult a = ParseResult {
    macroName :: Name
  , result    :: Either Error a
  }

isFailure :: ParseResult a -> Maybe Text
isFailure pres =
    either (const (Just pres.macroName.unwrap)) (const Nothing) pres.result

liftDefinition :: Definition -> ParseResult Definition
liftDefinition def = ParseResult def.name (Right def)

liftInvocation :: Invocation -> ParseResult Invocation
liftInvocation inv = ParseResult inv.name (Right inv)

data Error =
    ParseError MacroParseError
  | NameMismatch Text Text
  deriving stock (Eq, Show)

-- | Project the split macro definition onto its spelling and the names it
-- depends on
--
-- The split itself already happened during parsing; see
-- 'HsBindgen.Macro.Syntax.splitMacro'.
--
-- We keep checking the name against the one @libclang@ reported for the cursor:
-- the split does not subsume that comparison.
parseDefinition :: MacroDefinition -> ParseResult Definition
parseDefinition def =
    ParseResult (Name def.name) $
    case def.macro of
      Left e -> throwError $ ParseError e
      Right m
        | def.name == def'.name.unwrap
        -> pure def'
        | otherwise
        -> throwError $ NameMismatch def.name def'.name.unwrap
        where
          def' = definition m

-- | Reduce a macro definition to its spelling and the names it depends on
--
-- The dependencies cannot be recovered from the spelling: which tokens are
-- names depends on each token's kind.
--
-- Everything in the body that is not a name is dropped, as is every name that
-- is a parameter of this macro. A keyword that is not a local parameter is a
-- normal name: @#define B bool@ refers to @bool@ whether or not the C standard
-- in force makes @bool@ a keyword, and a reference we fail to harvest is an
-- ambiguity we fail to see.
--
-- @__VA_ARGS__@ and @__VA_OPT__@ are reserved identifiers only in variadic
-- macro definitions. We treat them as parameters there, to emphasise that their
-- expansion relies on the parameters. In the GNU named-variadic form the name
-- that stands for the trailing arguments is a parameter like any other.
definition :: Runtime.Macro.Raw (Token TokenSpelling) -> Definition
definition m = Definition{
      name    = toName m.name
    , deps    = Set.fromList
        [ n
        | t <- m.body
        , isIdentifierOrKeyword t
        , let n = toName t
        , not (isParam n)
        ]
    , spelled = fmap spelling m
    }
  where
    toName :: Token TokenSpelling -> Name
    toName = Name . spelling

    isParam :: Name -> Bool
    isParam n = case m.params of
        Runtime.Macro.NoParams              -> False
        Runtime.Macro.Params names variadic ->
          n `elem` map toName names || isVariadicParam n variadic

    isVariadicParam :: Name -> Runtime.Macro.Variadic (Token TokenSpelling) -> Bool
    isVariadicParam n = \case
        Runtime.Macro.NotVariadic                -> False
        Runtime.Macro.NamedEllipsis ellipsisName -> n == toName ellipsisName
        Runtime.Macro.Ellipsis                   ->
          n `elem` ["__VA_ARGS__", "__VA_OPT__"]

parseInvocation :: MacroInvocation -> ParseResult Invocation
parseInvocation inv =
    ParseResult (Name inv.name) $
    case runParser (P.parseInvocation <* eof) inv.tokens of
      Left e -> throwError $ ParseError e
      Right inv'
        | inv.name == inv'.name.unwrap
        -> pure inv'
        | otherwise
        -> throwError $ NameMismatch inv.name inv'.name.unwrap

{-------------------------------------------------------------------------------
  Analysis
-------------------------------------------------------------------------------}

-- | Result of the ambiguity analysis of all macro definitions
--
-- A macro is /ambiguous/ if its expansion depends on where it is invoked. The
-- analysis starts from the /seed/: the macros that are ambiguous on their own,
-- because their definitions are not all identical (see 'Redefinition'), or
-- because one of them could not be split, so that we cannot see what it
-- depends on. Every macro that (transitively) depends on a macro in the seed is
-- ambiguous too, since the tokens of a replacement list are rescanned at the
-- invocation site. For example,
--
-- @
-- #define A Foo
-- #define B A
-- #define A Bar
-- #define B A
-- @
--
-- The two definitions of @B@ are identical, but an invocation of @B@ expands to
-- @Foo@ above line 3 and to @Bar@ below it. @A@ is in the seed; @B@ is not, but
-- inherits @A@'s ambiguity. @B@ is ambiguous even without its second
-- definition.
--
-- Two notions must not be confused here. Ambiguity (the seed and its
-- dependents) decides whether an invocation may be pre-expanded; see
-- 'isExpansionUnique'. Whether a macro that is defined more than once is a
-- conflict is decided by the seed alone; see 'redefinition'. In the example,
-- @A@ conflicts and @B@ does not: its definitions are interchangeable, and
-- whether it can be bound given the conflict of @A@ is for the macro language
-- and the @Select@ pass to decide.
--
-- The exact check would expand both definitions recursively and compare the
-- results. We cannot drive the Clang preprocessor for that, so we approximate:
-- a macro is unambiguous only if none of its dependencies is ambiguous.
--
-- A replacement list consisting of literals only needs no special treatment:
-- it depends on nothing, so it is ambiguous only if it is in the seed.
data Analysis = Analysis {
      seed      :: Set Name
    , ambiguous :: Set Name
    }

analyseMacroDefinitions :: [ParseResult Definition] -> Analysis
analyseMacroDefinitions defs = Analysis{
      seed      = seed
    , ambiguous = seed <> Digraph.reaches seed graph
    }
  where
    byName :: Map Name [Either Error Definition]
    byName = Map.fromListWith (++) [ (d.macroName, [d.result]) | d <- defs ]

    seed :: Set Name
    seed = Map.keysSet $ Map.filter (not . identical) byName

    graph :: DependentsGraph
    graph = mkDependentsGraph $ rights $ map (.result) defs

-- | Are all definitions of a macro identical?
--
-- We compare token spellings. That is marginally more permissive than C, which
-- also compares white-space separation (@(1-1)@ and @(1 - 1)@ differ; C23
-- 6.10.5p1); Clang has diagnosed such a redefinition before we see it.
identical :: [Either Error Definition] -> Bool
identical results = case partitionEithers results of
    ([], d : ds) -> all (\d' -> d'.spelled == d.spelled) ds
    _otherwise   -> False

-- | Is the redefinition of a macro benign?
--
-- Only meaningful for macros that are defined more than once.
redefinition :: Analysis -> Name -> Redefinition
redefinition analysis name
    | name `Set.member` analysis.seed = NotBenign
    | otherwise                       = Benign

ambiguity :: Analysis -> Name -> Ambiguity
ambiguity analysis name
    | name `Set.member` analysis.ambiguous = Ambiguous
    | otherwise                            = Unambiguous

-- | The names of all ambiguous macros
ambiguous :: Analysis -> Set Name
ambiguous analysis = analysis.ambiguous

{-------------------------------------------------------------------------------
  Unique expansion
-------------------------------------------------------------------------------}

-- | Check whether a macro invocation has a unique expansion
--
-- A macro invocation has a /unique/ expansion if it can be moved to any
-- location further down the source file without changing the expansion result.
-- That is the case iff the invoked macro and all names in the arguments are
-- unambiguous; see 'Analysis'. Names that are not macros are unambiguous.
isExpansionUnique :: (Name -> Ambiguity) -> ParseResult Invocation -> Bool
isExpansionUnique ambiguityOf pInv =
    case pInv.result of
      Left _    -> False
      Right inv -> all ((== Unambiguous) . ambiguityOf) (inv.name : inv.args)

{-------------------------------------------------------------------------------
  Dependents graph
-------------------------------------------------------------------------------}

-- | Partial map of macro names to names of /dependent/ macros
--
-- Each macro name @A@ is mapped to a set of names of macros that depend on @A@;
-- see 'definition'.
--
-- For example, for these macro definitions:
--
-- > #define A B C D
-- > #define F(B) A B C D
--
-- The mapping would be:
--
-- > A -> [   F]
-- > B -> [A   ]
-- > C -> [A, F]
-- > D -> [A, F]
--
-- NOTE: this type is similar to the 'UseDeclGraph', but it includes *all* macro
-- definitions, not just the ones that we parsed successfully.
type DependentsGraph = Digraph () Name

mkDependentsGraph :: [Definition] -> DependentsGraph
mkDependentsGraph defs = Foldable.foldl' addDefinition Digraph.empty defs

addDefinition :: DependentsGraph -> Definition -> DependentsGraph
addDefinition g0 d = Foldable.foldl' f g0 d.deps
  where
    f :: DependentsGraph -> Name -> DependentsGraph
    f g dep = Digraph.insertEdge dep () d.name g
