-- | Definitions for the @PrepareReparse@ pass
--
-- This module is intended to be imported unqualified.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.IsPass
--
module HsBindgen.Frontend.Pass.PrepareReparse.IsPass (
    PrepareReparse
    -- * Tokens
  , FlatTokens (..)
  ) where

import Clang.HighLevel.Types (MultiLoc)

import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (PrepareReparseMsg)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type PrepareReparse :: Pass
data PrepareReparse a

type family AnnPrepareReparse (ix :: Symbol) :: Star where
  AnnPrepareReparse "StructField"     = ReparseInfo FlatTokens
  AnnPrepareReparse "UnionField"      = ReparseInfo FlatTokens
  AnnPrepareReparse "Typedef"         = ReparseInfo FlatTokens
  AnnPrepareReparse "Function"        = ReparseInfo FlatTokens
  AnnPrepareReparse "Global"          = ReparseInfo FlatTokens
  AnnPrepareReparse _                 = NoAnn

instance IsPass PrepareReparse where
  type MacroBody   PrepareReparse = TypecheckedMacro PrepareReparse
  type Ann ix      PrepareReparse = AnnPrepareReparse ix
  type Msg         PrepareReparse = PrepareReparseMsg
  type MacroId     PrepareReparse = Id PrepareReparse
  type CommentDecl PrepareReparse = Maybe (C.Comment PrepareReparse)
  macroIdId _ = id

{-------------------------------------------------------------------------------
  Tokens
-------------------------------------------------------------------------------}

-- | @libclang@ tokens flattened into a single string
data FlatTokens = FlatTokens {
      -- | Tokens flattened into a single string
      flatten  :: String
      -- | Location of the first token before flattening
    , locStart :: MultiLoc
    }
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  CoercePass: TypecheckMacros → PrepareReparse
-------------------------------------------------------------------------------}

instance CoercePassId               TypecheckMacros PrepareReparse
instance CoercePassMacroId          TypecheckMacros PrepareReparse
instance CoercePassAnn "TypeFunArg" TypecheckMacros PrepareReparse

instance CoercePassCommentDecl      TypecheckMacros PrepareReparse where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroUnderlying  TypecheckMacros PrepareReparse
