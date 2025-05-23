module HsBindgen.Frontend.Pass (
    Pass
  , IsPass(..)
  , NoAnn(..)
  ) where

import HsBindgen.Imports
import GHC.TypeLits (Symbol)

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
  -- | Previous pass ('None' if this is the first pass)
  type Previous p :: Pass
  type Previous p = None

  -- | Identity of declarations
  --
  -- This takes various forms during processing:
  --
  -- 1. After parsing, this is 'DeclId': anonymous structures are assigned an ID
  --    based on their source location, for everything else we use the C name.
  -- 2. After 'RenameAnon', /everything/ has a C name: we have assigned names
  --    to anonymous structures.
  -- 3. After 'NameMangling', this becomes a pair of the C name and the
  --    corresponding Haskell name.
  type Id p :: Star
  type Id p = Id (Previous p)

  -- | Macro body
  --
  -- After parsing this is simply a list of tokens; after 'HandleMacros', this
  -- is the parsed and type-checked macro body.
  type Macro p :: Star
  type Macro p = Macro (Previous p)

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
  type Ann ix p = Ann ix (Previous p)

type None :: Pass
data None a

data NoAnn = NoAnn
  deriving stock (Show)
