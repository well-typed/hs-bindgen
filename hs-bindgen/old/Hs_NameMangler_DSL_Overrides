module HsBindgen.Hs.NameMangler.DSL.Overrides (
    Overrides(..)
  , overridesNone
  , applyOverrides
    -- * Map representation
  , OverridesMap(..)
  , OverridesFor
  , overridesMap
  ) where

import Data.Map qualified as Map

import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.NameMangler.API
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Override translations of Haskell names
--
-- Since not all generated Haskell names have corresponding C names, overriding
-- is primarily done based on the generated Haskell name.  In some cases the C
-- name can be used for disambiguation, if more than one C name is being
-- translated to the same Haskell name.
data Overrides = Overrides {
      override :: forall ns.
           SingNamespace ns
        => NameSpec ns
           -- ^ Specification of the name we're trying to construct
        -> Maybe (HsName ns)
           -- ^ The Haskell name we construct by default
           --
           -- This will be 'Nothing' only if we fail to construct the Haskell
           -- name altogether. For example, this can happen if a C type is
           -- called @_123@, and we are using the 'mkHsNameDropInvalid'
           -- policy; in this case, /all/ characters are invalid, and so we'd
           -- end up with nothing.
        -> Maybe (HsName ns)
    }

-- | Do not override any translations
overridesNone :: Overrides
overridesNone = Overrides $ \_cname _name -> Nothing

{-------------------------------------------------------------------------------
  Map representation

  This is more amenable to JSON/YAML serialization.
-------------------------------------------------------------------------------}

type OverridesFor ns = Map (NameSpec ns, Maybe (HsName ns)) (HsName ns)

data OverridesMap = OverridesMap {
      overridesNsTypeConstr :: OverridesFor NsTypeConstr
    , overridesNsConstr     :: OverridesFor NsConstr
    , overridesNsVar        :: OverridesFor NsVar
    }

-- | Override translations of Haskell names using a map
overridesMap :: OverridesMap -> Overrides
overridesMap overrideMap = Overrides $
    caseNamespace singNamespace
  where
    caseNamespace ::
         SNamespace ns
      -> NameSpec ns
      -> Maybe (HsName ns)
      -> Maybe (HsName ns)
    caseNamespace SNsTypeConstr = aux overridesNsTypeConstr
    caseNamespace SNsConstr     = aux overridesNsConstr
    caseNamespace SNsVar        = aux overridesNsVar

    aux ::
         (OverridesMap -> OverridesFor ns)
      -> NameSpec ns -> Maybe (HsName ns) -> Maybe (HsName ns)
    aux f spec defName =
        case Map.lookup (spec, defName) (f overrideMap) of
          Just override -> Just override
          Nothing       -> defName


{-------------------------------------------------------------------------------
  Applying overrides
-------------------------------------------------------------------------------}

applyOverrides ::
     Overrides
  -> NameMangler' Maybe
  -> NameMangler' (Either NameManglerErr)
applyOverrides overrides nm = NameMangler $ \spec -> do
    let generated = mangle' nm spec
    aux spec generated (override overrides spec generated)
  where
    aux ::
         NameSpec ns       -- Input to name generation
      -> Maybe (HsName ns) -- Generated name  (unless failed)
      -> Maybe (HsName ns) -- Override (if any)
      -> Either NameManglerErr (HsName ns)
    aux _    _                (Just override) = Right override
    aux _    (Just generated) _               = Right generated
    aux spec Nothing          Nothing         = Left $ RequireOverride spec
