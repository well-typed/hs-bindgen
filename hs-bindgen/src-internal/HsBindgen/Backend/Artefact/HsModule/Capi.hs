module HsBindgen.Backend.Artefact.HsModule.Capi (
    capiImport
  , renderCapiWrapper
  )
where

import Text.SimplePrettyPrint (CtxDoc, (<+>), (><))

import HsBindgen.Backend.Artefact.HsModule.Names
import HsBindgen.Imports

-- | The CAPI `addCSource` import.
--
-- We import the @hs-bindgen-runtime@ prelude when adding C sources. Foreign
-- imports of these C sources require the _data_ constructors of all involved
-- data types to be in scope. We can only ensure the data constructors to be in
-- scope by tying the CAPI import statement to an import of all C wrapper data
-- types we are using (such as 'Foreign.C.CDouble').
--
-- See also "HsBindgen.Runtime.Prelude".
capiImport :: HsImportModule
capiImport = HsImportModule hsbPrelude Nothing

-- | Render the CAPI `addCSource` code fragment.
--
-- See 'capiImport'.
renderCapiWrapper :: String -> CtxDoc
renderCapiWrapper src =
  "$(" >< fromString hsbPrelude >< ".addCSource" <+> fromString (show src) >< ")"

-- Qualified import string for @hs-bindgen-runtime@ prelude.
hsbPrelude :: String
hsbPrelude = "HsBindgen.Runtime.Prelude"
