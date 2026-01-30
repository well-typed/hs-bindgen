-- We capitalize module names, but use camelCase/PascalCase in code:
--
-- - in types names:    CapiFoo, FooCapiBar
-- - in variable names: capiFoo, fooCapiBar
module HsBindgen.Backend.HsModule.CAPI (
    capiImport
  , renderCapiWrapper
  )
where

import Text.SimplePrettyPrint (CtxDoc)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.HsModule.Names
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

-- | The CAPI `addCSource` import.
--
-- See also "HsBindgen.Runtime.Internal.CAPI".
capiImport :: HsImportModule
capiImport = HsImportModule hsbCapiModule Nothing

-- | Render the CAPI `addCSource` code fragment.
--
-- See 'capiImport'.
renderCapiWrapper :: String -> CtxDoc
renderCapiWrapper src = PP.vcat [
     PP.hcat [
         "$("
       , withCapiModule "addCSource"
       , " ("
       , withCapiModule "unlines"
       ]
  , PP.hcat [
        PP.nest 2 (PP.vlist "[" "]" linesDocs)
      , "))"
      ]
  ]
  where
    linesDocs :: [CtxDoc]
    linesDocs = map (fromString . show) (lines src)

    withCapiModule :: String -> CtxDoc
    withCapiModule x = PP.hcat [
        PP.string (Hs.moduleNameToString hsbCapiModule)
      , "."
      , PP.string x
      ]

-- Qualified import string for @hs-bindgen-runtime@ prelude.
hsbCapiModule :: Hs.ModuleName
hsbCapiModule = "HsBindgen.Runtime.Internal.CAPI"
