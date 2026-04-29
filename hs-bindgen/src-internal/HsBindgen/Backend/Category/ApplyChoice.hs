module HsBindgen.Backend.Category.ApplyChoice (
    applyBindingCategoryChoice
  ) where

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Level
import HsBindgen.Errors (panicPure)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

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
        Hs.DeclFunction fn                -> Hs.DeclFunction $ over #name renameTerm $ fn
        Hs.DeclMacroExpr{}                -> p
        Hs.DeclUnionGetter{}              -> p
        Hs.DeclUnionSetter{}              -> p
        Hs.DeclVar x                      -> Hs.DeclVar $ over #name renameTerm $ x
      where
        p :: a
        p = panicPure $ "Must not rename type-level declaration " <> show d

        -- Only rename exported names
        renameTerm :: Hs.TermName -> Hs.TermName
        renameTerm = \case
            Hs.ExportedName x ->
              -- TODO <https://github.com/well-typed/hs-bindgen/issues/1928>
              --
              -- At the moment, rename of exported names is unsafe. The user
              -- must adhere to naming rules (e.g., type constructors start with
              -- capital letters).
              --
              -- However, we do have the means to parse and check the new name,
              -- and could do so here.
              Hs.ExportedName $ Hs.UnsafeName $ f x.text
            Hs.InternalName x ->
              Hs.InternalName x

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
