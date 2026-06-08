-- |
--
-- Intended for unqualified import.
module HsBindgen.Clang.Macros.UniqueExpansion (
    isExpansionUnique
    -- * Parse
  , ParseResult
  , isFailure
  , liftDefinition
  , liftInvocation
  , parseDefinition
  , parseInvocation
    -- * Cached
  , Cache
  , precomputeIsExpansionUnique
  , cachedIsExpansionUnique
  ) where


import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (first))
import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.Either (partitionEithers)
import Data.Foldable qualified as Foldable
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Text.Parsec (eof)

import HsBindgen.Clang.Macros (MacroDefinition (name, tokens),
                               MacroInvocation (name, tokens))
import HsBindgen.Clang.Macros.UniqueExpansion.Parse qualified as P
import HsBindgen.Clang.Macros.UniqueExpansion.Parse.Infra (MacroParseError,
                                                           runParser)
import HsBindgen.Clang.Macros.UniqueExpansion.Types

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
  deriving stock Show

parseDefinition :: MacroDefinition -> ParseResult Definition
parseDefinition def =
    ParseResult (Name def.name) $
    case runParser (P.parseDefinition <* eof) def.tokens of
      Left e -> throwError $ ParseError e
      Right def'
        | def.name == def'.name.unwrap
        -> pure def'
        | otherwise
        -> throwError $ NameMismatch def.name def'.name.unwrap

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
  Unique expansion
-------------------------------------------------------------------------------}

-- | Check whether a macro invocation has a unique expansion
--
-- A macro invocation has a /unique/ expansion if it can be moved to any
-- location further down the source file without changing the expansion result.
--
-- This true as long as a macro (transitively) referenced by an invocation is
-- not captured by a new macro definition. Such capturing would cause the
-- expansion of the macro invocation to change. A macro invocation has a unique
-- expansion if all these conditions are met:
--
-- * The invoked macro only has a single definition in the translation unit
-- * The body of the invoked macro only invokes macros that have a unique
--   expansion
-- * If the invoked macro is function-like, then any parameters to the
--   invocation should also only invoke macros that have a unique expansion
--
-- These conditions give rise to a recursive algorithm: to check whether a macro
-- invocation has a unique expansion, we recursively check whether macro
-- invocations in the body have unique expansions, and we check the same for
-- parameters to the invocation.
--
isExpansionUnique ::
     [ParseResult Definition]
  -> ParseResult Invocation
  -> Bool
isExpansionUnique defs inv =
    cachedIsExpansionUnique (precomputeIsExpansionUnique defs) inv

{-------------------------------------------------------------------------------
  Cached
-------------------------------------------------------------------------------}

newtype Cache = Cache {
    -- | Names of ambiguous macros
    --
    -- Ambiguous macros are macros that are either defined more than once, or
    -- macros that (transitively) refer to ambiguous macros in their definition
    ambiguous :: Set Name
  }

-- | Precompute the majority of 'isExpansionUnique' as a cache, which can then
-- be passed to 'cachedIsExpansionUnique'.
precomputeIsExpansionUnique :: [ParseResult Definition] -> Cache
precomputeIsExpansionUnique defs = Cache {
      ambiguous = ambiguityAnalysis defs
    }

-- | Use a precomputed cache to run 'isExpansionUnique'.
cachedIsExpansionUnique :: Cache -> ParseResult Invocation -> Bool
cachedIsExpansionUnique cache pInv =
    case pInv.result of
      Left _ -> False
      Right inv ->
        not $
        or
          [ inv.name `Set.member` cache.ambiguous
          , any (`Set.member` cache.ambiguous) inv.args
          ]

{-------------------------------------------------------------------------------
  Ambiguity analysis
-------------------------------------------------------------------------------}

-- | Collect the names of all macros that are defined more than once, and all
-- macros that (transitively) depend on macros that are defined more than once.
--
ambiguityAnalysis :: [ParseResult Definition] -> Set Name
ambiguityAnalysis defs =
    go (Seen Set.empty) (Ambig $ Set.fromList parseFailures) parseSuccesses
  where
    fromParseResult :: forall a. ParseResult a -> Either Name a
    fromParseResult pres = first (const pres.macroName) pres.result

    parseFailures :: [Name]
    parseSuccesses :: [Definition]
    (parseFailures, parseSuccesses) = partitionEithers $ fmap fromParseResult defs

    graph :: DependentsGraph
    graph = mkDependentsGraph parseSuccesses

    go :: Seen -> Ambig -> [Definition] -> Set Name
    go _seen ambig [] = ambig.unwrap
    go seen ambig (d:ds)
      | d.name `Set.member` seen.unwrap
      = let current = Set.singleton d.name
            dependents = Digraph.reaches current graph
            ambig' = Ambig (current <> dependents <> ambig.unwrap)
        in  go seen' ambig' ds
      | otherwise
      = go seen' ambig ds
      where
        seen' = Seen (Set.insert d.name seen.unwrap)

newtype Seen = Seen { unwrap :: Set Name }
newtype Ambig = Ambig { unwrap :: Set Name }

{-------------------------------------------------------------------------------
  Dependents graph
-------------------------------------------------------------------------------}

-- | Partial map of macro names to names of /dependent/ macros
--
-- Each macro name @A@ is mapped to a set of names of macros that reference @A@
-- in their definition. Parameters do not count as dependencies.
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
addDefinition g0 d = Foldable.foldl' f g0 deps
  where
    deps = getDependencies d

    f :: DependentsGraph -> Name -> DependentsGraph
    f g dep = Digraph.insertEdge dep () d.name g

getDependencies :: Definition -> [Name]
getDependencies def = mapMaybe isDep def.body
  where
    isDep :: Var -> Maybe Name
    isDep = \case
        LocalParam _ -> Nothing
        FreeVar    n -> Just n
