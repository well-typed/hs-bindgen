module HsBindgen.Frontend.Pass (
    Pass
  , IsPass(..)
  , NoAnn(..)
  , NoConfig(..)
  ) where

import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Kind of passes
--
-- Example:
--
-- > type Parsed :: Pass
-- > data Parsed a
--
-- This is an open kind, primarily to avoid 'IsPass' orphans.
type Pass = PassSimulatedOpenKind -> Star

-- | Internal type used only to simulate an open kind. Not exported.
data PassSimulatedOpenKind

{-------------------------------------------------------------------------------
  Associated type families
-------------------------------------------------------------------------------}

-- | Pass definition
class IsPass (p :: Pass) where
  -- | Declaration identifier
  --
  -- This takes various forms during processing:
  --
  -- 1. After parsing, this is 'PrelimDeclId': anonymous structures are assigned
  --    an ID based on source location, for everything else we use the C name.
  -- 2. After 'AssignAnonIds', this is 'DeclId': /everything/ has a name,
  --    because we have assigned names to anonymous structures.
  -- 3. After 'MangleNames', this becomes a pair of the C name and the
  --    corresponding Haskell name.
  type Id p :: Star

  -- | Scoped names
  --
  -- This is the name of struct fields, function arguments, etc.; names that
  -- live in a local scope. This is initially 'C.ScopedName', and becomes
  -- 'ScopedNamePair' after 'MangleNames'.
  type ScopedName p :: Star

  -- | Macro body
  --
  -- Parsing macros is non-trivial, and requires knowledge of other macros in
  -- scope. Therefore we don't parse macros during parsing of the clang AST, but
  -- instead simply record them a list of tokens. 'HandleMacros' then does the
  -- actual parsing, instantiating this to 'CheckedMacro'
  type MacroBody p :: Star

  -- | Representation of external bindings
  type ExtBinding p :: Star

  -- | Generic TTG-style annotation
  --
  -- For single-constructor datatypes, the index can simply be the name of the
  -- datatype; for multi-constructor datatypes, it should be @"Type.Constr"@
  -- (or simply @"Constr"@ if that is unambiguous).
  --
  -- TODO: We could consider using a closed universe for these indices; that
  -- would make certain things a bit easier (we should show that all annotations
  -- are showable, for example).
  type Ann (ix :: Symbol) p :: Star

  -- | Configuration required to run the pass
  type Config p :: Star
  type Config p = NoConfig

  -- | Trace messages possibly emitted by the pass
  type Msg p :: Star

data NoAnn = NoAnn
  deriving stock (Show, Eq, Ord)

data NoConfig = NoConfig
  deriving stock (Show, Eq, Ord)
