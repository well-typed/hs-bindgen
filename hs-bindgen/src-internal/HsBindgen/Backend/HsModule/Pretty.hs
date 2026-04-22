{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty (
  ) where

import Data.List qualified as List
import Text.SimplePrettyPrint (CtxDoc, Pretty (..), ($$), (<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Backend.HsModule.Pretty.CAPI
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

-- | Pretty-print the export list with section headers
--
-- Section headers (@-- *@, @-- **@) are interleaved without commas.
-- Regular export items use the standard leading-comma style.
--
-- Example output:
--
-- > ( Example.Api_version_t(..)
-- >   -- * Core Data Types
-- > , Example.Config_t(..)
-- > , Example.Color_enum(..)
-- > , pattern Example.COLOR_RED
-- >   -- * Advanced Features
-- > , Example.complex_function
-- > )
prettyExportList :: String -> [ExportEntry] -> CtxDoc
prettyExportList qualPrefix = PP.vcat . go True False
  where
    -- @needOpen@: whether we still need to emit the opening @(@
    -- @needComma@: whether we've seen a regular export item (and need @,@)
    go :: Bool -> Bool -> [ExportEntry] -> [CtxDoc]
    go _ _ [] = [")"]
    go needOpen needComma (entry : rest) = case entry of
      ExportSectionHeader depth title ->
        let prefix = if needOpen then "( " else "  "
            stars = replicate (fromIntegral depth) '*'
        in  (PP.string prefix >< PP.string ("-- " ++ stars ++ " ") >< PP.text title)
            : go False needComma rest
      ExportEntry item ->
        let prefix | needOpen       = "( "
                   | not needComma  = "  "
                   | otherwise      = ", "
        in  (PP.string prefix >< prettyExportItem qualPrefix item)
            : go False True rest

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
