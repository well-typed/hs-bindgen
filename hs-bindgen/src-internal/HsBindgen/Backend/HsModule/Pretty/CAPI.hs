-- We capitalize module names, but use camelCase/PascalCase in code:
--
-- - in types names:    CapiFoo, FooCapiBar
-- - in variable names: capiFoo, fooCapiBar
module HsBindgen.Backend.HsModule.Pretty.CAPI (
    prettyCapiWrappers
  )
where

import Text.SimplePrettyPrint (CtxDoc)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

-- | Pretty print the CAPI @addCSource@ code fragment.
prettyCapiWrappers :: [CWrapper] -> CtxDoc
prettyCapiWrappers wrappers
  | null src  = PP.empty
  | otherwise = prettyCapiWrappers' src
  where
    src :: String
    src = getCWrappersSource wrappers

prettyCapiWrappers' :: String -> CtxDoc
prettyCapiWrappers' src = PP.vcat [
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
