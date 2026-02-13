-- We capitalize module names, but use camelCase/PascalCase in code:
--
-- - in types names:    CapiFoo, FooCapiBar
-- - in variable names: capiFoo, fooCapiBar
module HsBindgen.Backend.HsModule.CAPI (
    capiModule
  , renderCapiWrapper
  )
where

import Text.SimplePrettyPrint (CtxDoc)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

-- Qualified import string for @hs-bindgen-runtime@ prelude.
capiModule :: Hs.ModuleName
capiModule = "HsBindgen.Runtime.Internal.CAPI"

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
        PP.string (Hs.moduleNameToString capiModule)
      , "."
      , PP.string x
      ]
