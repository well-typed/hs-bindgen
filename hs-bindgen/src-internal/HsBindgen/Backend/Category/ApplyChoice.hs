{-# LANGUAGE OverloadedLabels #-}

module HsBindgen.Backend.Category.ApplyChoice (
    applyBindingCategoryChoice
  ) where

import Optics.Core (Lens', over, view)

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
import HsBindgen.Errors (panicPure)
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Binding category choice
-------------------------------------------------------------------------------}

applyTypes :: Choice LvlType -> [a] -> [a]
applyTypes = \case
  ExcludeCategory     -> const []
  IncludeTypeCategory -> id

applyTerms :: Choice LvlTerm -> [Hs.Decl] -> [Hs.Decl]
applyTerms = \case
    ExcludeCategory                    -> const []
    IncludeTermCategory (RenameTerm f) -> map (renameHsDeclWith f)
  where
    renameHsDeclWith :: (Text -> Text) -> Hs.Decl -> Hs.Decl
    renameHsDeclWith f d = case d of
        Hs.DeclData{}                  -> p "Data"
        Hs.DeclEmpty{}                 -> p "Empty"
        Hs.DeclNewtype{}               -> p "Newtype"
        Hs.DeclPatSyn{}                -> p "PatSyn"
        Hs.DeclDefineInstance{}        -> p "DefineInstance"
        Hs.DeclDeriveInstance{}        -> p "DeriveInstance"
        fi@Hs.DeclForeignImport{}      -> fi
        Hs.DeclFunction fn             -> Hs.DeclFunction $ overN #functionDeclName fn
        Hs.DeclMacroExpr{}             -> p "MacroExpr"
        Hs.DeclUnionGetter{}           -> p "UnionGetter"
        Hs.DeclUnionSetter{}           -> p "UnionSetter"
        Hs.DeclVar x                   -> Hs.DeclVar $ overN #varName x
        Hs.DeclPragma (SHs.NOINLINE x) -> Hs.DeclPragma (SHs.NOINLINE $ fN x)
      where
        p :: String -> a
        p e = panicPure $ "applyTerms.renameHsDeclWith (" <> show d <> "): unexpected " <> e

        fN :: Hs.Name n -> Hs.Name n
        fN = \case
          Hs.ExposedName  x -> Hs.ExposedName $ f x
          Hs.InternalName x -> Hs.InternalName  x

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
