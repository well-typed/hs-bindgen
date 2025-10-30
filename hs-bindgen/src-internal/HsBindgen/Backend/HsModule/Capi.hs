module HsBindgen.Backend.HsModule.Capi (
    capiImport
  , renderCapiWrapper
  )
where

import Text.SimplePrettyPrint (CtxDoc, nest, vlist, ($$), (><))

import HsBindgen.Backend.HsModule.Names
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
     "$(" >< fromString hsbPrelude >< ".addCSource (HsBindgen.Runtime.Prelude.unlines"
  $$ nest 2 (vlist '[' ']' linesDocs) >< "))"
  where
    linesDocs = map (fromString . show) (lines src)

-- Qualified import string for @hs-bindgen-runtime@ prelude.
hsbPrelude :: String
hsbPrelude = "HsBindgen.Runtime.Prelude"
