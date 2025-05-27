-- | Translate between the "raw" types and the "old" C AST
--
-- TODO: This module is temporary. The "raw" AST will /be/ the C AST once done.
module HsBindgen.Frontend.Adapter (
    Raw
  , FromRaw(..)
  , ToRaw(..)
  ) where

import HsBindgen.Imports

import HsBindgen.C.AST qualified as Old
import HsBindgen.Errors
import HsBindgen.Frontend.AST qualified as Raw
import HsBindgen.Frontend.Pass.HandleMacros.IsPass
import HsBindgen.Frontend.Pass.Parse.IsPass qualified as Raw
import HsBindgen.Frontend.Pass

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type family Raw a :: Star

class FromRaw a where
  fromRaw :: Raw a -> a

class ToRaw a where
  toRaw :: a -> Raw a

{-------------------------------------------------------------------------------
  Type
-------------------------------------------------------------------------------}

type instance Raw Old.Type = Raw.Type HandleMacros

instance ToRaw Old.Type where
  toRaw (Old.TypeTypedef (Old.CName name)) =
      Raw.TypeTypedef (Raw.DeclNamed name) NoAnn
  toRaw ty =
      panicPure $ "toRaw: unhandled " ++ show ty
