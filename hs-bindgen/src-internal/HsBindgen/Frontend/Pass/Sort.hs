module HsBindgen.Frontend.Pass.Sort (
    sortDecls
  , SortError(..)
  ) where

import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndexError)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.AST.Coerce
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Sort.IsPass
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

sortDecls ::
     C.TranslationUnit Parse
  -> (C.TranslationUnit Sort, [SortError])
sortDecls unit@C.TranslationUnit{..} =
    let (declMeta, declIndexErrors) = mkDeclMeta unit
    in ( C.TranslationUnit{
             unitAnn   = declMeta
           , unitDecls = map coercePass $
                           UseDeclGraph.toDecls
                             (declIndex declMeta)
                             (declUsage declMeta)
           , ..
           }
       , declIndexErrors
       )

mkDeclMeta :: C.TranslationUnit Parse -> (DeclMeta, [SortError])
mkDeclMeta unit =
    let (declIndex, declIndexErrors) = DeclIndex.fromDecls unitDecls
        declUsage = UseDeclGraph.fromDecls unitIncludeGraph unitDecls
    in ( DeclMeta{..}
       , map SortErrorDeclIndex declIndexErrors
       )
  where
    C.TranslationUnit{
        unitDecls
      , unitIncludeGraph
      , unitAnn = declNonSelected
      } = unit

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data SortError =
    SortErrorDeclIndex DeclIndexError
  deriving stock (Show, Eq)

instance PrettyTrace SortError where
  prettyTrace (SortErrorDeclIndex x) = prettyTrace x

instance HasDefaultLogLevel SortError where
  getDefaultLogLevel (SortErrorDeclIndex x) = getDefaultLogLevel x
