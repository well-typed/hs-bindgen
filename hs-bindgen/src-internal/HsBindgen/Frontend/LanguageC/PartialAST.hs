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
  , CName
    -- * Starting point: no information known
  , unknownDecl
  ) where

import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data PartialDecl = PartialDecl{
      partialName :: Maybe CName
    , partialType :: PartialType
    }
  deriving stock (Show, Generic)

data PartialType =
    PartialUnknown UnknownType
  | PartialKnown KnownType
  deriving stock (Show)

-- | The type itself is not yet known, but may have some qualifiers
data UnknownType = UnknownType{
      unknownSign  :: Maybe C.PrimSign
    , unknownConst :: Bool
    }
  deriving stock (Show, Generic)

data KnownType =
    KnownType (C.Type HandleMacros)

    -- | Special case for top-level functions, so we can record argument names
    --
    -- It's not necessary to do this recursively: we only want argument names
    -- for top-level function declarations (not for function pointers).
  | TopLevelFun [(Maybe CName, C.Type HandleMacros)] (C.Type HandleMacros)
  deriving stock (Show)

-- | Name
--
-- Since @language-c@ does not really distinguish between a top-level
-- declaration or the \"declaration\" of a function argument, we use 'Text' here
-- rather than 'C.DeclName' or 'C.ScopedName'. Since we do not actually parse
-- top-level struct/enum/union declarations (i.e., deal with \"ordinary\" names
-- only anyway), this causes no trouble.
type CName = Text

{-------------------------------------------------------------------------------
  Starting point: no information known
-------------------------------------------------------------------------------}

unknownDecl :: PartialDecl
unknownDecl = PartialDecl{
      partialName = Nothing
    , partialType = unknownType
    }

unknownType :: PartialType
unknownType = PartialUnknown UnknownType{
      unknownSign  = Nothing
    , unknownConst = False
    }
