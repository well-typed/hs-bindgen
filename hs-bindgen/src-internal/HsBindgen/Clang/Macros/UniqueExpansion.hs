-- |
--
-- Intended for unqualified import.
module HsBindgen.Clang.Macros.UniqueExpansion (
    isExpansionUnique
  , isExpansionUnique'
  ) where


import Control.Monad.Except (MonadError (throwError))
import Data.Digraph (Digraph)
import Data.Digraph qualified as Digraph
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Text.Parsec (eof)

import HsBindgen.Clang.Macros (MacroDefinition (name, tokens),
                               MacroInvocation (name, tokens))
import HsBindgen.Clang.Macros.UniqueExpansion.Parse (parseDefinition,
                                                     parseInvocation)
import HsBindgen.Clang.Macros.UniqueExpansion.Parse.Infra (MacroParseError,
                                                           runParser)
import HsBindgen.Clang.Macros.UniqueExpansion.Types
import HsBindgen.Errors (panicPure)


{-------------------------------------------------------------------------------
  Local types
-------------------------------------------------------------------------------}

data Error =
    ParseError MacroParseError
  | NameMismatch Text Text
  deriving stock Show

toDefinition :: MacroDefinition -> Either Error Definition
toDefinition def = case runParser (parseDefinition <* eof) def.tokens of
      Left e -> throwError $ ParseError e
      Right def'
        | def.name == def'.name.unwrap
        -> pure def'
        | otherwise
        -> throwError $ NameMismatch def.name def'.name.unwrap


toInvocation :: MacroInvocation -> Either Error Invocation
toInvocation inv = case runParser (parseInvocation <* eof) inv.tokens of
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
-- This true as long as the macro referenced by an invocation is not captured by
-- a new macro definition. Such capturing would cause the expansion of the macro
-- invocation to change. A macro invocation has a unique expansion if all these
-- conditions are met:
--
-- * The macro referenced only has a single definition in the translation unit
-- * The body of the referenced macro only invokes macros that have a unique
--   expansion
-- * If the referenced macro is function-like, then any parameters to the
--   invocation should also only invoke macros that have a unique expansion
--
-- These conditions give rise to a recursive algorithm: to check whether a macro
-- invocation has a unique expansion, we recursively check whether macro
-- invocations in the body have unique expansions, and we check the same for
-- parameters to the invocation.
--
isExpansionUnique ::
     [MacroDefinition]
  -> MacroInvocation
  -> Bool
isExpansionUnique = \defs inv -> isExpansionUnique' (map toDef defs) (toInv inv)
  where
    toDef :: MacroDefinition -> Definition
    toDef = either (panicPure . show) id . toDefinition

    toInv :: MacroInvocation -> Invocation
    toInv = either (panicPure . show) id . toInvocation

isExpansionUnique' ::
     [Definition]
  -> Invocation
  -> Bool
isExpansionUnique' defs inv = not $
    or
      [ inv.name `Set.member` ambig
      , any (`Set.member` ambig) inv.args
      ]
  where
    ambig :: Set Name
    ambig = ambiguityAnalysis defs

{-------------------------------------------------------------------------------
  Ambiguity analysis
-------------------------------------------------------------------------------}

-- | Collect the names of all macros that are defined more than once, and all
-- macros that (transitively) depend on macros that are defined more than once.
--
ambiguityAnalysis :: [Definition] -> Set Name
ambiguityAnalysis defs = go (Seen Set.empty) (Ambig Set.empty) defs
  where
    graph :: DependentsGraph
    graph = mkDependentsGraph defs

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
mkDependentsGraph defs = foldl' addDefinition Digraph.empty defs

addDefinition :: DependentsGraph -> Definition -> DependentsGraph
addDefinition g0 d = foldl' f g0 deps
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
