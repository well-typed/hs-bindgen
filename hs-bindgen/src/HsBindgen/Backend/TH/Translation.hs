module HsBindgen.Backend.TH.Translation (
    translateC
  ) where

import Language.Haskell.TH qualified as TH

import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.Translation

{-------------------------------------------------------------------------------
  Generate list of declarations for splicing
-------------------------------------------------------------------------------}

translateC :: TH.Quote q => C.Header -> q [TH.Dec]
translateC =
      fmap concat
    . traverse mkDecl
    . map SHs.translateDecl
    . generateDeclarations topts
  where
    topts :: TranslationOpts
    topts = defaultTranslationOpts
