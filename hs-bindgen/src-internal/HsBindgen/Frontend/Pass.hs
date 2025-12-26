{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoRecordWildCards #-}
{-# LANGUAGE OverloadedLabels  #-}

module HsBindgen.Frontend.Pass (
    Pass
  , IsPass(..)
  , NoAnn(..)
  , NoMsg
  ) where

import Clang.HighLevel.Types

import HsBindgen.Frontend.LocationInfo
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer

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
class (

        -- 'Show' constraints for debugging
        Show (ExtBinding p)
      , Show (Id         p)
      , Show (MacroBody  p)
      , Show (ScopedName p)

      , Show (Ann "CheckedMacroType" p)
      , Show (Ann "Decl"             p)
      , Show (Ann "Enum"             p)
      , Show (Ann "Function"         p)
      , Show (Ann "Struct"           p)
      , Show (Ann "StructField"      p)
      , Show (Ann "TranslationUnit"  p)
      , Show (Ann "Typedef"          p)
      , Show (Ann "Union"            p)
      , Show (Ann "UnionField"       p)

        -- 'Ord' constraints for identifiers (which we often store in maps)

      , Ord (Id         p)
      , Ord (ScopedName p)

        -- 'Ord' constraint' on 'ExtBinding' is necessary for de-dupping types.

      , Ord (ExtBinding p)

        -- 'Eq'
        --
        -- We use equality on 'DeclKind' during construction of the 'DeclIndex'
        -- (it's OK to repeat a declaration in a header, as long as they are
        -- identical). All other 'Eq' constraints we provide are in order to
        -- support this equality.

      , Eq (ExtBinding p)
      , Eq (MacroBody  p)
      , Eq (ScopedName p)

      , Eq (Ann "CheckedMacroType" p)
      , Eq (Ann "Enum"             p)
      , Eq (Ann "Function"         p)
      , Eq (Ann "Struct"           p)
      , Eq (Ann "StructField"      p)
      , Eq (Ann "Typedef"          p)
      , Eq (Ann "Union"            p)
      , Eq (Ann "UnionField"       p)

      ) => IsPass (p :: Pass) where
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
  type Id p = DeclId

  -- | Scoped names
  --
  -- This is the name of struct fields, function arguments, etc.; names that
  -- live in a local scope. This is initially 'C.ScopedName', and becomes
  -- 'ScopedNamePair' after 'MangleNames'.
  type ScopedName p :: Star
  type ScopedName p = C.ScopedName

  -- | Macro body
  --
  -- Parsing macros is non-trivial, and requires knowledge of other macros in
  -- scope. Therefore we don't parse macros during parsing of the clang AST, but
  -- instead simply record them a list of tokens. 'HandleMacros' then does the
  -- actual parsing, instantiating this to 'CheckedMacro'
  type MacroBody p :: Star

  -- | Representation of external bindings
  --
  -- This is initialized in @ResolveBindingSpecs@.
  type ExtBinding p :: Star
  type ExtBinding p = Void

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

  -- | Trace messages possibly emitted by the pass
  type Msg p :: Star

  -- | Name kind of the C name
  idNameKind :: Proxy p -> Id p -> C.NameKind
  default idNameKind :: Id p ~ DeclId => Proxy p -> Id p -> C.NameKind
  idNameKind _ = (.name.kind)

  -- | Name of the declaration as it appears in the C source, if any
  idSourceName :: Proxy p -> Id p -> Maybe C.DeclName
  default idSourceName :: Id p ~ DeclId => Proxy p -> Id p -> Maybe C.DeclName
  idSourceName _ = declIdSourceName

  -- | Location information
  idLocationInfo :: Proxy p -> Id p -> [SingleLoc] -> LocationInfo
  default idLocationInfo ::
       Id p ~ DeclId
    => Proxy p -> Id p -> [SingleLoc] -> LocationInfo
  idLocationInfo _ = declIdLocationInfo

  extBindingId :: Proxy p -> ExtBinding p -> Id p
  default extBindingId :: ExtBinding p ~ Void => Proxy p -> ExtBinding p -> Id p
  extBindingId _ = absurd

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

data NoAnn = NoAnn
  deriving stock (Show, Eq, Ord)

data NoMsg lvl
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace (NoMsg lvl) where
  prettyForTrace msg = case msg of {}

instance IsTrace lvl (NoMsg lvl) where
  getDefaultLogLevel msg = case msg of {}
  getSource          msg = case msg of {}
  getTraceId         msg = case msg of {}

