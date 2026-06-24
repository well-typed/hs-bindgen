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

import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (PrepareReparseMsg)
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type PrepareReparse :: Pass
data PrepareReparse a

type family AnnPrepareReparse (ix :: Symbol) :: Star where
  AnnPrepareReparse "Function"    = ReparseInfo FlatTokens
  AnnPrepareReparse "Global"      = ReparseInfo FlatTokens
  AnnPrepareReparse "StructField" = ReparseInfo FlatTokens
  AnnPrepareReparse "Typedef"     = ReparseInfo FlatTokens
  AnnPrepareReparse "UnionField"  = ReparseInfo FlatTokens
  AnnPrepareReparse _             = NoAnn

instance IsPass PrepareReparse

instance PassId PrepareReparse

instance PassScopedName PrepareReparse

instance PassTypes PrepareReparse

instance PassMacro PrepareReparse where
  type MacroId   PrepareReparse = Id PrepareReparse
  type MacroBody PrepareReparse = TypecheckedMacro PrepareReparse

  macroIdId _ = id

instance PassExtBinding PrepareReparse

instance PassCommentDecl PrepareReparse where
  type CommentDecl PrepareReparse = Maybe (C.Comment PrepareReparse)

instance PassAnn PrepareReparse where
  type Ann ix PrepareReparse = AnnPrepareReparse ix

instance PassMsg PrepareReparse where
  type Msg PrepareReparse = PrepareReparseMsg

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
instance CoercePassTypes            TypecheckMacros PrepareReparse
instance CoercePassMacroId          TypecheckMacros PrepareReparse
instance CoercePassAnn "TypeFunArg" TypecheckMacros PrepareReparse

instance CoercePassCommentDecl TypecheckMacros PrepareReparse where
  coercePassCommentDecl _ = fmap coercePass

instance CoercePassMacroUnderlying TypecheckMacros PrepareReparse
