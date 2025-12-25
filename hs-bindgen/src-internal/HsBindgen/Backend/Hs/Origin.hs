-- | Origin information
--
-- When we generate Haskell code, we keep a record of their \"origin\": what
-- part of the C input corresponds to these definitions? We need this
-- information for various purposes:
--
-- * Generating tests
-- * Generating external binding specification
-- * Generating documentation
--
-- Intended for qualified import.
--
-- > import HsBindgen.Backend.Hs.Origin qualified as Origin
module HsBindgen.Backend.Hs.Origin (
    -- * Declarations
    Decl(..)
  , Struct(..)
  , Newtype(..)
  , EmptyData(..)
  , ForeignImport(..)
  , PatSyn(..)
    -- * Fields
  , Field(..)
  ) where

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

data Decl a = Decl{
      declInfo :: C.DeclInfo Final
    , declKind :: a            -- ^ Kind-specific information
    , declSpec :: PrescriptiveDeclSpec
    }
  deriving stock (Generic, Show)

data Struct =
    Struct (C.Struct Final)
  deriving stock (Generic, Show)

data Newtype =
    Enum    (C.Enum           Final)
  | Typedef (C.Typedef        Final)
  | Union   (C.Union          Final)
  | Aux     (C.Typedef        Final)
  | Macro   (CheckedMacroType Final)
  deriving stock (Generic, Show)

data EmptyData =
    Opaque C.NameKind
  deriving stock (Generic, Show)

data ForeignImport =
    Function   (C.Function Final)
  | Global     (C.Type     Final)
  | ToFunPtr   (C.Type     Final)
  | FromFunPtr (C.Type     Final)
  deriving stock (Generic, Show)

newtype PatSyn =
    EnumConstant (C.EnumConstant Final)
  deriving stock (Generic, Show)

{-------------------------------------------------------------------------------
  Fields
-------------------------------------------------------------------------------}

data Field =
    GeneratedField  -- ^ Field without a direct counterpart in C
  | StructField (C.StructField Final)
  deriving stock (Generic, Show)

