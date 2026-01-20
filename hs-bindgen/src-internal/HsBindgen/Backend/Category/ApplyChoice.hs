module HsBindgen.Backend.Category.ApplyChoice (
    applyBindingCategoryChoice
  ) where

import Optics.Core (Lens')

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Errors (panicPure)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Binding category choice
-------------------------------------------------------------------------------}

applyTypes :: Choice LvlType -> [a] -> [a]
applyTypes = \case
  ExcludeCategory     -> const []
  IncludeTypeCategory -> id

-- The list of declarations should only contain terms ('LvlTerm').
applyTerms :: Choice LvlTerm -> [Hs.Decl] -> [Hs.Decl]
applyTerms = \case
    ExcludeCategory                    -> const []
    IncludeTermCategory (RenameTerm f) -> map (renameHsDeclWith f)
  where
    renameHsDeclWith :: (Text -> Text) -> Hs.Decl -> Hs.Decl
    renameHsDeclWith f d = case d of
        Hs.DeclData{}                     -> p "Data"
        Hs.DeclEmpty{}                    -> p "Empty"
        Hs.DeclNewtype{}                  -> p "Newtype"
        Hs.DeclPatSyn{}                   -> p "PatSyn"
        Hs.DeclDefineInstance{}           -> p "DefineInstance"
        Hs.DeclDeriveInstance{}           -> p "DeriveInstance"
        fi@Hs.DeclForeignImport{}         -> fi
        fiw@Hs.DeclForeignImportWrapper{} -> fiw
        fid@Hs.DeclForeignImportDynamic{} -> fid
        Hs.DeclFunction fn                -> Hs.DeclFunction $ overN #name fn
        Hs.DeclMacroExpr{}                -> p "MacroExpr"
        Hs.DeclUnionGetter{}              -> p "UnionGetter"
        Hs.DeclUnionSetter{}              -> p "UnionSetter"
        Hs.DeclVar x                      -> Hs.DeclVar $ overN #name x
      where
        p :: String -> a
        p e = panicPure $ "applyTerms.renameHsDeclWith (" <> show d <> "): unexpected " <> e

        -- Don't rename internal names
        fN :: Hs.Name n -> Hs.Name n
        fN = \case
            Hs.ExportedName x ->
              Hs.ExportedName . Hs.UnsafeExportedName $ f x.text
            Hs.InternalName x ->
              Hs.InternalName x

        overN :: Lens' a (Hs.Name n) -> a -> a
        overN l = over l fN

-- | Choose binding categories and possibly rename declarations in term-level
-- | categories.
applyBindingCategoryChoice ::
     ByCategory Choice
  -> ByCategory_ [Hs.Decl]
  -> ByCategory_ [Hs.Decl]
applyBindingCategoryChoice choice =
    mapWithCategory_ aux
  where
    aux :: Category -> [Hs.Decl] -> [Hs.Decl]
    aux = \case
      CType     -> applyTypes choice.cType
      CTerm cat -> applyTerms (view (lensForTermCategory cat) choice)
