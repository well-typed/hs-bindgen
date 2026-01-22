module HsBindgen.Guasi (
    Guasi (..)
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

    -- | Attach a documentation to a declaration with provided name _in the
    --   current module_.
    --
    -- This abstraction is necessary to work around a TH bug, where
    -- documentation would be attached to symbols defined _in other modules_ but
    -- with the same name (e.g., `reverse`).
    putDocName :: Hs.Name ns -> HsDoc.Comment -> g ()

instance Guasi TH.Q where
    addDependentFile = TH.addDependentFile
    extsEnabled = TH.extsEnabled
    reportError = TH.reportError

    addCSource = TH.addForeignSource TH.LangC

    putDocName nm comment = do
      md <- TH.loc_module <$> TH.location
      let qualifiedName = TH.mkName $ md <> "." <> (Text.unpack $ Hs.getName nm)
      TH.addModFinalizer $
        TH.putDoc (TH.DeclDoc qualifiedName) (show $ pretty $ THComment comment)

putDocNameM :: Guasi g => Hs.Name ns -> Maybe HsDoc.Comment -> g ()
putDocNameM nm = traverse_ (putDocName nm)
