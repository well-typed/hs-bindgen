module HsBindgen.Guasi (
    Guasi (..)
  , withDecDocM
  , putFieldDocM
  ) where

import Data.Foldable (traverse_)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Text.SimplePrettyPrint (pretty)

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.HsModule.Render (CommentKind (..))

-- | An intermediate class between 'TH.Quote' and 'TH.Quasi'
-- which doesn't provide reification functionality of 'TH.Quasi',
-- but has a bit more than 'TH.Quote'.
class TH.Quote g => Guasi g where
    addDependentFile :: FilePath -> g ()
    extsEnabled :: g [TH.Extension]
    reportError :: String -> g ()
    addCSource :: String -> g ()

    -- | Attach a documentation string to a declaration
    --
    withDecDoc   :: HsDoc.Comment -> g TH.Dec -> g TH.Dec

    -- | Attach a documentation string to a 'TH.DocLoc'. This is mostly used
    -- for data structure fields.
    --
    putFieldDoc :: TH.DocLoc -> HsDoc.Comment -> g ()

instance Guasi TH.Q where
    addDependentFile = TH.addDependentFile
    extsEnabled = TH.extsEnabled
    reportError = TH.reportError

    addCSource = TH.addForeignSource TH.LangC

    withDecDoc comment =
      TH.withDecDoc (show $ pretty $ THComment comment)
    putFieldDoc docLoc comment =
      TH.addModFinalizer $
        TH.putDoc docLoc (show $ pretty $ THComment comment)

withDecDocM :: Guasi g => Maybe HsDoc.Comment -> g TH.Dec -> g TH.Dec
withDecDocM Nothing  a = a
withDecDocM (Just c) a = withDecDoc c a

putFieldDocM :: Guasi g => TH.DocLoc -> Maybe HsDoc.Comment -> g ()
putFieldDocM l = traverse_ (putFieldDoc l)
