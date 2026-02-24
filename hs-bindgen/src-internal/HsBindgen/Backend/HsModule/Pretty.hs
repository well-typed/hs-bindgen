{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module HsBindgen.Backend.HsModule.Pretty (
  ) where

import Data.List qualified as List
import Text.SimplePrettyPrint (CtxDoc, Pretty (..))
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
    : PP.hsep ["module", PP.string (Hs.moduleNameToString hsModule.name), "where"]
    : PP.vcat (map (prettyImport hsModule.qualifiedStyle) hsModule.imports)
    : (prettyCapiWrappers hsModule.cWrappers)
    : map pretty hsModule.decls

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
