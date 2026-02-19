module HsBindgen.Backend.Category.ApplyChoice (
    applyBindingCategoryChoice
  ) where

import Optics.Core (Lens')

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Level
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
        Hs.DeclTypSyn{}                   -> p
        Hs.DeclData{}                     -> p
        Hs.DeclEmpty{}                    -> p
        Hs.DeclNewtype{}                  -> p
        Hs.DeclPatSyn{}                   -> p
        Hs.DeclDefineInstance{}           -> p
        Hs.DeclDeriveInstance{}           -> p
        fi@Hs.DeclForeignImport{}         -> fi
        fiw@Hs.DeclForeignImportWrapper{} -> fiw
        fid@Hs.DeclForeignImportDynamic{} -> fid
        Hs.DeclFunction fn                -> Hs.DeclFunction $ overN #name fn
        Hs.DeclMacroExpr{}                -> p
        Hs.DeclUnionGetter{}              -> p
        Hs.DeclUnionSetter{}              -> p
        Hs.DeclVar x                      -> Hs.DeclVar $ overN #name x
      where
        p :: a
        p = panicPure $ "Must not rename type-level declaration " <> show d

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
