{-# LANGUAGE OverloadedLabels #-}

module HsBindgen.Backend.Category.ApplyChoice (
    applyBindingCategoryChoice
  ) where

import Optics.Core (Lens', over, view)

import HsBindgen.Backend.Category
import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.SHs.AST qualified as SHs
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

applyTerms :: Choice LvlTerm -> [Hs.Decl] -> [Hs.Decl]
applyTerms = \case
    ExcludeCategory                    -> const []
    IncludeTermCategory (RenameTerm f) -> map (renameHsDeclWith f)
  where
    renameHsDeclWith :: (Text -> Text) -> Hs.Decl -> Hs.Decl
    renameHsDeclWith f d = case d of
        Hs.DeclData{}           -> p "Data"
        Hs.DeclEmpty{}          -> p "Empty"
        Hs.DeclNewtype{}        -> p "Newtype"
        Hs.DeclPatSyn{}         -> p "PatSyn"
        Hs.DeclDefineInstance{} -> p "DefineInstance"
        Hs.DeclDeriveInstance{} -> p "DeriveInstance"
        -- TODO_PR: These should not be renamed, but wait for PR 1.
        Hs.DeclForeignImport x  -> Hs.DeclForeignImport $ overN #foreignImportName f x
        -- TODO_PR: Only rename non-unique symbols here (after PR 2).
        Hs.DeclFunction      x  -> Hs.DeclFunction      $ overN #functionDeclName  f x
        Hs.DeclMacroExpr{}      -> p "MacroExpr"
        Hs.DeclUnionGetter{}    -> p "UnionGetter"
        Hs.DeclUnionSetter{}    -> p "UnionSetter"
        -- TODO_PR: This recursion should not exist after PR 0.
        Hs.DeclSimple        x  -> Hs.DeclSimple        $ renameSHsDeclWith f x
      where
        p :: String -> a
        p e = panicPure $ "applyTerms: renameHsDeclWith (" <> show d <> "): " <> e

    renameSHsDeclWith :: (Text -> Text) -> SHs.SDecl -> SHs.SDecl
    renameSHsDeclWith f d = case d of
        SHs.DVar x                   -> SHs.DVar $ overN #varName f x
        SHs.DInst{}                  -> p "Instance"
        SHs.DRecord{}                -> p "Record"
        SHs.DNewtype{}               -> p "Newtype"
        SHs.DEmptyData{}             -> p "EmptyData"
        SHs.DDerivingInstance{}      -> p "DerivingInstance"
        SHs.DForeignImport{}         -> p "ForeignImport"
        SHs.DFunction{}              -> p "Function"
        SHs.DPatternSynonym{}        -> p "PatternSynonym"
        SHs.DPragma (SHs.NOINLINE n) -> SHs.DPragma (SHs.NOINLINE $ fN f n)
      where
        p :: String -> a
        p e = panicPure $ "applyTerms: renameSHsDeclWith: (" <> show d <> "): " <> e

    fN :: (Text -> Text) -> Hs.Name n -> Hs.Name n
    fN f = Hs.Name . f . Hs.getName

    overN :: Lens' a (Hs.Name n) -> (Text -> Text) -> a -> a
    overN l f = over l (fN f)


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
