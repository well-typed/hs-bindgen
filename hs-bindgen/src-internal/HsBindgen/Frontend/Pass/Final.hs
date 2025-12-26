{-# OPTIONS_GHC -Wno-orphans #-}

-- | Working with the frontend AST after the final pass
--
-- Intended for unqualified import.
module HsBindgen.Frontend.Pass.Final (
    Final
  ) where

import GHC.Records

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass
import HsBindgen.Frontend.Pass.MangleNames.IsPass qualified as MangleNames

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Final frontend pass
--
-- Backend passes should refer to 'Final' instead of the actual name of the
-- final pass, so that if we add more passes, the backend is unaffected.
type Final = MangleNames

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

instance HasField "names" (C.Struct Final) MangleNames.RecordNames where
  getField struct = struct.structAnn

instance HasField "names" (C.Union Final) MangleNames.NewtypeNames where
  getField union = union.unionAnn

instance HasField "names" (C.Enum Final) MangleNames.NewtypeNames where
  getField enum = enum.enumAnn

instance HasField "names" (C.Typedef Final) MangleNames.NewtypeNames where
  getField typedef = typedef.typedefAnn

instance HasField "names" (CheckedMacroType Final) MangleNames.NewtypeNames where
  getField macro = macro.ann
