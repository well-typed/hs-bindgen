{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Centralized table of the @hs-bindgen-runtime@ modules that the generator
-- refers to by name.
--
-- Most paths are derived from a representative name defined in the module (via
-- 'TH' quotes), so renaming a runtime module only requires updating its one
-- @import ... qualified as@ line below: the compiler re-derives the path, and a
-- stale reference fails to compile. The exceptions are the /re-export-only/
-- modules ('Support', 'CompatHasField', 'LibC'): a re-exported name
-- resolves to its /defining/ module (e.g. @''RIP.Ptr@ resolves to "GHC.Ptr"),
-- so Template Haskell cannot recover the re-export module's own path. For those
-- we keep an explicit, centralized module name and rely on the guard test
-- (@Test.HsBindgen.Unit.Runtime@) to catch a rename that misses the table.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Backend.Runtime (RuntimeModule)
-- > import HsBindgen.Backend.Runtime qualified as Runtime
module HsBindgen.Backend.Runtime (
    RuntimeModule(..)
  , moduleName
  , qualifiedImport
  ) where

import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH

import HsBindgen.Runtime.BitfieldPtr qualified as BitfieldPtr
import HsBindgen.Runtime.Block qualified as Block
import HsBindgen.Runtime.CEnum qualified as CEnum
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FLAM qualified as FLAM
import HsBindgen.Runtime.HasCBitfield qualified as HasCBitfield
import HsBindgen.Runtime.HasCField qualified as HasCField
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.Marshal qualified as Marshal
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import HsBindgen.Runtime.Struct qualified as Struct
import HsBindgen.Runtime.Support.CAPI qualified as CAPI
import HsBindgen.Runtime.Union qualified as Union

import HsBindgen.Errors (panicPure)
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Runtime modules
-------------------------------------------------------------------------------}

-- | A module in @hs-bindgen-runtime@ that the generator refers to by name.
data RuntimeModule =
    -- Modules whose path is derived from a representative name defined in them.
    ConstantArray
  | IncompleteArray
  | IsArray
  | Marshal
  | Flam
  | HasCField
  | BitfieldPtr
  | HasCBitfield
  | PtrConst
  | CEnum
  | Block
  | Union
  | Struct
  | CAPI

    -- Re-export-only modules: their path cannot be derived via Template Haskell
    -- (see the module top-level documentation), so it is spelled out in
    -- 'moduleName'.
  | Support
  | CompatHasField
  | LibC
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | The module path emitted for a runtime module.
moduleName :: RuntimeModule -> Hs.ModuleName
moduleName = \case
    ConstantArray   -> derived ''CA.ConstantArray
    IncompleteArray -> derived ''IA.IncompleteArray
    IsArray         -> derived ''IsA.IsArray
    Marshal         -> derived ''Marshal.EquivStorable
    Flam            -> derived ''FLAM.WithFlam
    HasCField       -> derived ''HasCField.HasCField
    BitfieldPtr     -> derived ''BitfieldPtr.BitfieldPtr
    HasCBitfield    -> derived ''HasCBitfield.HasCBitfield
    PtrConst        -> derived ''PtrConst.PtrConst
    CEnum           -> derived ''CEnum.CEnum
    Block           -> derived ''Block.Block
    Union           -> derived ''Union.IsUnion
    Struct          -> derived ''Struct.IsStruct
    CAPI            -> derived 'CAPI.addCSource
    -- Re-export-only modules; no self-defined name to derive from.
    Support         -> "HsBindgen.Runtime.Support"
    CompatHasField  -> "HsBindgen.Runtime.Support.CompatHasField"
    LibC            -> "HsBindgen.Runtime.LibC"
  where
    derived :: TH.Name -> Hs.ModuleName
    derived n = case TH.nameModule n of
      Just m  -> Hs.ModuleName (Text.pack m)
      Nothing -> panicPure $ "Expected name with module: " ++ show n

-- | The qualified import emitted for a runtime module.
qualifiedImport :: RuntimeModule -> Hs.Import
qualifiedImport rm = Hs.QualifiedImport (moduleName rm) (qualifier rm)
  where
    -- The alias a qualified import of the runtime module uses (@Nothing@ =
    -- import qualified with no alias, i.e. use of the fully qualified name).
    --
    -- Only meaningful for modules imported /qualified/ by generated code. 'LibC' is
    -- imported unqualified (with an explicit import list) and does not go through
    -- 'runtimeImport'; its qualifier is unused.
    qualifier :: RuntimeModule -> Maybe String
    qualifier = \case
        ConstantArray   -> Just "CA"
        IncompleteArray -> Just "IA"
        IsArray         -> Just "IsA"
        Marshal         -> Just "Marshal"
        Flam            -> Just "FLAM"
        HasCField       -> Just "HasCField"
        BitfieldPtr     -> Just "BitfieldPtr"
        HasCBitfield    -> Just "HasCBitfield"
        PtrConst        -> Just "PtrConst"
        CEnum           -> Just "CEnum"
        Block           -> Just "Block"
        Union           -> Just "Union"
        Struct          -> Just "Struct"
        CAPI            -> Nothing
        Support         -> Just "BG"
        CompatHasField  -> Just "BG.CompatHasField"
        LibC            -> Nothing
