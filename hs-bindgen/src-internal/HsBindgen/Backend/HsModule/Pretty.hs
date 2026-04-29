{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty (
  ) where

import Data.List qualified as List
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), ($$), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.HsModule.Pretty.CAPI
import HsBindgen.Backend.HsModule.Pretty.Comment ()
import HsBindgen.Backend.HsModule.Pretty.Decl ()
import HsBindgen.Backend.HsModule.Translation
import HsBindgen.Config.Prelims
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Module pretty-printing
-------------------------------------------------------------------------------}

instance Pretty HsModule where
  pretty hsModule = PP.vsep $
      PP.vcat (map pretty hsModule.pragmas)
    : prettyModuleHeader hsModule.name hsModule.exports
    : PP.vcat (map (prettyImport hsModule.qualifiedStyle) hsModule.imports)
    : (prettyCapiWrappers hsModule.cWrappers)
    : map pretty hsModule.decls

-- | Render the module header with an explicit export list
--
-- When the export list is empty, we render @module M where@ (no export list).
-- When there are exports, we render them in the standard style with leading
-- commas. Export items are qualified with the module name to avoid ambiguity
-- with names from the implicit Prelude (e.g. @Example.reverse@ instead of
-- @reverse@).
--
prettyModuleHeader :: Hs.ModuleName -> [ExportEntry] -> CtxDoc
prettyModuleHeader name [] =
    PP.hsep ["module", PP.string (Hs.moduleNameToString name), "where"]
prettyModuleHeader name exports =
    PP.vcat [
        PP.hsep ["module", PP.string (Hs.moduleNameToString name)] $$ PP.nest 4 (
          prettyExportList qualPrefix exports
        )
      , PP.nest 2 "where"
      ]
  where
    qualPrefix :: String
    qualPrefix = Hs.moduleNameToString name ++ "."

-- | Pretty-print the export list with nested section headers
--
-- Section headers (@-- *@, @-- **@, …) are interleaved without commas, with
-- the @*@ count derived from the recursion depth in the 'ExportEntry' tree.
-- Regular export items use the standard leading-comma style.
--
-- Example output:
--
-- > ( Example.Api_version_t(..)
-- >   -- * Core Data Types
-- > , Example.Config_t(..)
-- > , Example.Color_enum(..)
-- > , pattern Example.COLOR_RED
-- >   -- * Function Definitions
-- > , Example.process_data
-- >   -- ** I/O Helpers
-- > , Example.read_data
-- > )
prettyExportList :: String -> [ExportEntry] -> CtxDoc
prettyExportList qualPrefix entries =
    PP.vcat (fst (go 1 True False entries) ++ [")"])
  where
    -- @depth@: 1 → @-- *@, 2 → @-- **@, etc.  Increments at each section.
    -- @needOpen@: whether we still need to emit the opening @(@.
    -- @needComma@: whether we've already emitted a regular export item.
    -- Returns (rendered docs, needComma after rendering).
    go :: Int -> Bool -> Bool -> [ExportEntry] -> ([CtxDoc], Bool)
    go _ _ needComma [] = ([], needComma)
    go depth needOpen needComma (entry : rest) = case entry of
      ExportSection title children ->
        let prefix    = if needOpen then "( " else "  "
            stars     = replicate depth '*'
            header    = PP.string prefix
                     >< PP.string ("-- " ++ stars ++ " ")
                     >< PP.hsep (map pretty title)
            (cd, nc)  = go (depth + 1) False needComma children
            (rd, nc') = go depth False nc rest
        in  (header : cd ++ rd, nc')
      ExportEntry item ->
        let prefix | needOpen      = "( "
                   | not needComma = "  "
                   | otherwise     = ", "
            (rd, nc) = go depth False True rest
        in  ((PP.string prefix >< prettyExportItem qualPrefix item) : rd, nc)

-- | Pretty-print a single export item, qualified with the module name
prettyExportItem :: String -> ExportItem -> CtxDoc
prettyExportItem q = \case
    ExportTypeAll s -> PP.string q >< PP.text s >< "(..)"
    ExportName s    -> PP.string q >< PP.text s
    ExportPattern s -> "pattern" <+> (PP.string q >< PP.text s)

{-------------------------------------------------------------------------------
  GhcPragma pretty-printing
-------------------------------------------------------------------------------}

instance Pretty GhcPragma where
  pretty (GhcPragma ghcPragma) = PP.hsep ["{-#", PP.string ghcPragma, "#-}"]

{-------------------------------------------------------------------------------
  Import pretty-printing
-------------------------------------------------------------------------------}

-- | Pretty-print an import statement, respecting 'QualifiedStyle'
prettyImport :: QualifiedStyle -> ImportListItem -> CtxDoc
prettyImport qualStyle = \case
    UnqualifiedImportListItem name Nothing -> PP.hsep
      [ "import"
      , PP.string (Hs.moduleNameToString name)
      ]
    UnqualifiedImportListItem name (Just ns) -> PP.hsep
      [ "import"
      , PP.string (Hs.moduleNameToString name)
      , PP.parens . PP.hcat . List.intersperse ", " $ map pretty ns
      ]
    QualifiedImportListItem name alias -> case qualStyle of
      PreQualified ->
        case alias of
          Just q -> PP.hsep
            [ "import qualified"
            , PP.string (Hs.moduleNameToString name)
            , "as"
            , PP.string q
            ]
          Nothing -> PP.hsep
            [ "import qualified"
            , PP.string (Hs.moduleNameToString name)
            ]
      PostQualified ->
        case alias of
          Just q -> PP.hsep
            [ "import"
            , PP.string (Hs.moduleNameToString name)
            , "qualified"
            , "as"
            , PP.string q
            ]
          Nothing -> PP.hsep
            [ "import"
            , PP.string (Hs.moduleNameToString name)
            , "qualified"
            ]
