module HsBindgen.Frontend.Pass.Sort (
    sortDecls
  , SortMsg(..)
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
  -> (C.TranslationUnit Sort, [SortMsg])
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

mkDeclMeta :: C.TranslationUnit Parse -> (DeclMeta, [SortMsg])
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

data SortMsg =
    SortErrorDeclIndex DeclIndexError
  deriving stock (Show, Eq)

instance PrettyForTrace SortMsg where
  prettyTrace (SortErrorDeclIndex x) = prettyTrace x

instance HasDefaultLogLevel SortMsg where
  getDefaultLogLevel (SortErrorDeclIndex x) = getDefaultLogLevel x
