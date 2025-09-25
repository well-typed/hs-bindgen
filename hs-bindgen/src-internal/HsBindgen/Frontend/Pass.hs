module HsBindgen.Frontend.Pass (
    Pass
  , IsPass(..)
  , TypedefRefWrapper(..)
  , NoAnn(..)
  , NoConfig(..)
  , NoMsg(..)
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
  -- 1. After parsing, this is 'DeclId': anonymous structures are assigned an ID
  --    based on their source location, for everything else we use the C name.
  -- 2. After 'NameAnon', /everything/ has a C name: we have assigned names to
  --    anonymous structures.
  -- 3. After 'NameMangling', this becomes a pair of the C name and the
  --    corresponding Haskell name.
  type Id p :: Star

  -- | Names of fields (structs and unions)
  type FieldName p :: Star

  -- | Names of arguments (functions)
  type ArgumentName p :: Star

  -- | Reference to a typedef
  --
  -- Initially this is just the name of the typedef, but after 'HandleTypedefs'
  -- we distinguish between regular typedefs and squashed typedefs (in which
  -- case it's the type that it got replaced with).
  type TypedefRef p :: Star

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

-- | Newtype wrapper intended for class instances and constraints where
-- partially applied type synonyms are not allowed.
newtype TypedefRefWrapper p = TypedefRefWrapper { unTypedefRefWrapper :: TypedefRef p }

data NoAnn = NoAnn
  deriving stock (Show, Eq, Ord)

data NoConfig = NoConfig
  deriving stock (Show, Eq, Ord)

data NoMsg = NoMsg
  deriving stock (Show, Eq, Ord)
