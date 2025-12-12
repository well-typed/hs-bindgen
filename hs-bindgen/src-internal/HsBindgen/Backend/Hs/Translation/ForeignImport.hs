-- | Generate Haskell foreign imports
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    foreignImportDec
  ) where

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.SHs.AST
import HsBindgen.Backend.UniqueSymbol
import HsBindgen.Language.C qualified as C

foreignImportDec ::
     UniqueSymbol
  -> HsType
  -> [Hs.FunctionParameter]
  -> C.DeclName
  -> CallConv
  -> Origin.ForeignImport
  -> Maybe HsDoc.Comment
  -> Safety
  -> Hs.Decl
foreignImportDec name resultType parameters origName callConv origin comment safety =
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
        , foreignImportComment      = comment
        , foreignImportSafety       = safety
        }
