module HsBindgen.Frontend.Pass.HandleTypedefs.IsPass (
    HandleTypedefs
  , RenamedTypedefRef(..)
  , HandleTypedefsMsg(..)
  ) where

import Text.SimplePrettyPrint qualified as PP

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal (ValidPass)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.ConstructTranslationUnit.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Pass definition
-------------------------------------------------------------------------------}

type HandleTypedefs :: Pass
data HandleTypedefs a deriving anyclass ValidPass

type family AnnHandleTypedefs ix where
  AnnHandleTypedefs "TranslationUnit" = DeclMeta
  AnnHandleTypedefs "Decl"            =
    (Maybe BindingSpec.CTypeSpec, Maybe BindingSpec.HsTypeSpec)
  AnnHandleTypedefs _                 = NoAnn

instance IsPass HandleTypedefs where
  type Id           HandleTypedefs = C.DeclId HandleTypedefs
  type FieldName    HandleTypedefs = C.Name
  type ArgumentName HandleTypedefs = Maybe C.Name
  type TypedefRef   HandleTypedefs = RenamedTypedefRef HandleTypedefs
  type MacroBody    HandleTypedefs = C.CheckedMacro HandleTypedefs
  type ExtBinding   HandleTypedefs = ResolvedExtBinding
  type Ann ix       HandleTypedefs = AnnHandleTypedefs ix
  type Msg          HandleTypedefs = HandleTypedefsMsg

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

-- | Reference to a typedef type
data RenamedTypedefRef p =
    -- | Regular reference (see 'TypedefRefRegular' for more information).
    TypedefRegular
      -- | Name of the referenced typedef declaration
      (Id p)
      -- | The underlying type of the referenced typedef declaration
      --
      -- NOTE: the underlying type can arbitrarily reference other types,
      -- including typedefs that we have not parsed. Use the underlying type with
      -- care!
      (C.Type p)

    -- When we have a typedef around an anonymous declaration:
    --
    -- > typedef struct {
    -- >   int x;
    -- >   int y;
    -- > } foo;
    --
    -- we don't want to generate two types in Haskell (one for the anonymous
    -- struct and then a separate newtype around it), but just the one (for the
    -- struct, with the name from the typedef).
    --
    -- A similar situation arises when the struct tag is /already/ equal to the
    -- name of the typedef:
    --
    -- > typedef struct foo {
    -- >   int x;
    -- >   int y;
    -- > } foo;
    --
    -- We remove the declaration of these typedefs from the AST entirely (we do
    -- not need to generate code for them). We record the original C name
    -- (without a corresponding Haskell name) as well as the type that replaced
    -- the reference with.
  | TypedefSquashed C.Name (C.Type p)
  deriving stock (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

data HandleTypedefsMsg =
    HandleTypedefsSquashed (C.DeclInfo HandleTypedefs)
  | HandleTypedefsRenamedTagged (C.DeclInfo HandleTypedefs) C.Name
  deriving stock (Show)

instance PrettyForTrace HandleTypedefsMsg where
  prettyForTrace = \case
      HandleTypedefsSquashed info -> PP.hsep [
          "Squashed typedef"
        , prettyForTrace info
        ]
      HandleTypedefsRenamedTagged info newName -> PP.hsep [
          "Renamed"
        , prettyForTrace info
        , "to"
        , prettyForTrace newName
        ]

instance IsTrace Level HandleTypedefsMsg where
  getDefaultLogLevel = \case
      HandleTypedefsSquashed{}      -> Info
      HandleTypedefsRenamedTagged{} -> Info
  getSource  = const HsBindgen
  getTraceId = const "handle-typedefs"

{-------------------------------------------------------------------------------
  CoercePass
-------------------------------------------------------------------------------}

instance CoercePassId Select HandleTypedefs where
  coercePassId _ = coercePass
