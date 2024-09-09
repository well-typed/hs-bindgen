module HsBindgen.Backend.TH.Translation (
    translateC
  , translateHs
  ) where

import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Common
import HsBindgen.Backend.Common.Translation
import HsBindgen.Backend.TH
import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Translation.LowLevel

{-------------------------------------------------------------------------------
  Generate list of declarations for splicing
-------------------------------------------------------------------------------}

translateC :: TH.Quote q => C.Header -> q [TH.Dec]
translateC = translateHs . generateDeclarations

translateHs :: forall q. TH.Quote q => [Hs.Decl (Fresh (BE q))] -> q [TH.Dec]
translateHs =
    aux . runM . mapM (toBE BE)
  where
    aux :: q ([q TH.Dec]) -> q [TH.Dec]
    aux = (>>= sequence)

