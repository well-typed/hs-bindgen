module HsBindgen.Guasi (
    Guasi (..)
  , withDecDocM
  , putDocNameM
  ) where

import Data.Foldable (traverse_)
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Text.SimplePrettyPrint (pretty)

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
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
    withDecDoc   :: HsDoc.Comment -> g TH.Dec -> g TH.Dec

    -- | Attach a documentation to a declaration with provided name _in the
    --   current module_.
    putDocName :: Hs.Name ns -> HsDoc.Comment -> g ()

instance Guasi TH.Q where
    addDependentFile = TH.addDependentFile
    extsEnabled = TH.extsEnabled
    reportError = TH.reportError

    addCSource = TH.addForeignSource TH.LangC

    withDecDoc comment =
      TH.withDecDoc (show $ pretty $ THComment comment)

    putDocName nm comment = do
      md <- TH.loc_module <$> TH.location
      let qualifiedName = TH.mkName $ md <> "." <> (Text.unpack $ Hs.getName nm)
      TH.addModFinalizer $
        TH.putDoc (TH.DeclDoc qualifiedName) (show $ pretty $ THComment comment)

withDecDocM :: Guasi g => Maybe HsDoc.Comment -> g TH.Dec -> g TH.Dec
withDecDocM Nothing  a = a
withDecDocM (Just c) a = withDecDoc c a

putDocNameM :: Guasi g => Hs.Name ns -> Maybe HsDoc.Comment -> g ()
putDocNameM nm = traverse_ (putDocName nm)
