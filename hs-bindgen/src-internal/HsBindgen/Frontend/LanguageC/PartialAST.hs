-- | Partial AST
--
-- The translation from the language-c AST to our AST is a bit awkward, because
-- language-c will feed us information in a piecemeal fashion. We therefore
-- introduce this \"partial AST\" as a step in the middle.
--
-- Intended for unqualified import.
module HsBindgen.Frontend.LanguageC.PartialAST (
    PartialDecl(..)
  , PartialType(..)
  , UnknownType(..)
  , KnownType(..)
    -- * Starting point: no information known
  , unknownDecl
  ) where

import HsBindgen.Frontend.AST.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PartialDecl p = PartialDecl{
      partialName :: Maybe Name
    , partialType :: PartialType p
    }
  deriving stock (Show, Generic)

data PartialType p =
    PartialUnknown UnknownType
  | PartialKnown (KnownType p)
  deriving stock (Show)

-- | The type itself is not yet known, but may have some qualifiers
data UnknownType = UnknownType{
      unknownSign  :: Maybe PrimSign
    , unknownConst :: Bool
    }
  deriving stock (Show, Generic)

data KnownType p =
    KnownType (Type p)

    -- | Special case for top-level functions, so we can record argument names
    --
    -- It's not necessary to do this recursively: we only want argument names
    -- for top-level function declarations (not for function pointers).
  | TopLevelFun [(Maybe Name, Type p)] (Type p)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Starting point: no information known
-------------------------------------------------------------------------------}

unknownDecl :: PartialDecl p
unknownDecl = PartialDecl{
      partialName = Nothing
    , partialType = unknownType
    }

unknownType :: PartialType p
unknownType = PartialUnknown UnknownType{
      unknownSign  = Nothing
    , unknownConst = False
    }
