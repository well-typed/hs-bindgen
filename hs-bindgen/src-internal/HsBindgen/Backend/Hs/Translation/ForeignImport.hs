-- | Generate Haskell foreign imports (using the 'HasBaseForeignType' class)
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    foreignImportDec
  , hasBaseForeignTypeDecs
  ) where

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.SHs.AST
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell
import HsBindgen.Language.Haskell qualified as Hs

foreignImportDec ::
     Hs.Name Hs.NsVar
  -> HsType
  -> [Hs.FunctionParameter]
  -> C.DeclName
  -> CallConv
  -> Origin.ForeignImport
  -> Safety
  -> Hs.Decl
foreignImportDec name result parameters origName callConv origin safety =
    Hs.DeclForeignImport foreignImportDecl
  where
    foreignImportDecl :: Hs.ForeignImportDecl
    foreignImportDecl =  Hs.ForeignImportDecl{
          name       = name
        , result     = result
        , parameters = parameters
        , origName   = origName
        , callConv   = callConv
        , origin     = origin
        , comment    = mbUniqueSymbol
        , safety     = safety
        }

    mbUniqueSymbol :: Maybe HsDoc.Comment
    mbUniqueSymbol = case name of
      Hs.ExportedName _ -> Nothing
      Hs.InternalName x -> Just $ HsDoc.uniqueSymbol x

hasBaseForeignTypeDecs ::
     Hs.Newtype
  -> [Hs.Decl]
hasBaseForeignTypeDecs nt =
    [mk | HasBaseForeignType `elem` nt.instances]
  where
    mk :: Hs.Decl
    mk = Hs.DeclDeriveInstance Hs.DeriveInstance{
          deriveInstanceStrategy = Hs.DeriveNewtype
        , deriveInstanceClass    = HasBaseForeignType
        , deriveInstanceName     = nt.name
        , deriveInstanceComment  = Nothing
        }
