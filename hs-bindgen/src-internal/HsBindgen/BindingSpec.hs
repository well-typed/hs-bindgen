-- | Binding specification
--
-- Public interface.
module HsBindgen.BindingSpec (
    BindingSpec (..)
  , ExternalBindingSpec
  , PrescriptiveBindingSpec
  , emptyBindingSpec
  ) where

import HsBindgen.BindingSpec.Internal qualified as BindingSpec

-- | Binding specification
--
-- A binding specification serves two purposes:
--
-- * A /prescriptive binding specification/ is used to configure how bindings
--   are generated.
-- * An /external binding specification/ is used to specify existing bindings
--   that should be used, /external/ from the module being generated.
--
-- Note that a /generated binding specification/ may be used for either/both of
-- these two purposes.
data BindingSpec = BindingSpec {
      bindingSpecUnresolved :: BindingSpec.UnresolvedBindingSpec
    , bindingSpecResolved   :: BindingSpec.ResolvedBindingSpec
    }
  deriving stock (Show)

-- | External binding specification
--
-- This type alias is just used as documentation. This type name is used because
-- there is no need for a type alias for binding specifications.
--
-- See 'BindingSpec'.
type ExternalBindingSpec = BindingSpec

-- | Prescriptive binding specification
--
-- This type alias is just used as documentation. This type name is used because
-- there is no need for a type alias for binding specifications.
--
-- See 'BindingSpec'.
type PrescriptiveBindingSpec = BindingSpec

-- | Empty binding specification
emptyBindingSpec :: BindingSpec
emptyBindingSpec = BindingSpec {
      bindingSpecUnresolved = BindingSpec.empty
    , bindingSpecResolved   = BindingSpec.empty
    }
