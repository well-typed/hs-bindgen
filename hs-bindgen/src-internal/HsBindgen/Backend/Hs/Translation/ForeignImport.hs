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
foreignImportDec name resultType parameters origName callConv origin safety =
    Hs.DeclForeignImport foreignImportDecl
    -- TODO: prevent the "newtype constructor not in scope" bug. See issue #1282.
  where
    foreignImportDecl :: Hs.ForeignImportDecl
    foreignImportDecl =  Hs.ForeignImportDecl
        { foreignImportName         = name
        , foreignImportResultType   = resultType
        , foreignImportParameters   = parameters
        , foreignImportOrigName     = origName
        , foreignImportCallConv     = callConv
        , foreignImportOrigin       = origin
        , foreignImportComment      = mbUniqueSymbol
        , foreignImportSafety       = safety
        }

    mbUniqueSymbol :: Maybe HsDoc.Comment
    mbUniqueSymbol = case name of
      Hs.ExportedName _ -> Nothing
      Hs.InternalName x -> Just $ HsDoc.uniqueSymbol x

hasBaseForeignTypeDecs ::
     Hs.Newtype
  -> [Hs.Decl]
hasBaseForeignTypeDecs nt =
     [mk | HasBaseForeignType `elem` nt.newtypeInstances]
  where
    mk :: Hs.Decl
    mk = Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = Hs.DeriveNewtype
              , deriveInstanceClass    = HasBaseForeignType
              , deriveInstanceName     = Hs.newtypeName nt
              , deriveInstanceComment  = Nothing
              }
