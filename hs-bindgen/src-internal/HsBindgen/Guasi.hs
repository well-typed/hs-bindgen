module HsBindgen.Guasi (
    Guasi (..),
) where

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import HsBindgen.Backend.Artefact.PP.Render (CommentKind (..))
import HsBindgen.Backend.Hs.Haddock.Documentation (Comment)

import Text.SimplePrettyPrint (pretty)

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
    withDecDoc   :: Maybe Comment -> g TH.Dec -> g TH.Dec

    -- | Attach a documentation string to a 'TH.DocLoc'. This is mostly used
    -- for data structure fields.
    --
    putFieldDoc :: TH.DocLoc -> Maybe Comment -> g ()

-- |
--
-- >>> :seti -XTemplateHaskell
-- >>> import Language.Haskell.TH.Syntax
-- >>> $(getModuleUnique >>= lift)
-- "interactive_Ghci"
--
instance Guasi TH.Q where
    addDependentFile = TH.addDependentFile
    extsEnabled = TH.extsEnabled
    reportError = TH.reportError

    addCSource = TH.addForeignSource TH.LangC

    withDecDoc mbComment =
      TH.withDecDoc (maybe "" (show . pretty . THComment) mbComment)
    putFieldDoc docLoc mbComment =
      TH.addModFinalizer $
        TH.putDoc docLoc (maybe "" (show . pretty . THComment) mbComment)
