module HsBindgen.Backend.TH.Translation (
    translateC
  , translateHs
  ) where

import Language.Haskell.TH (Q)
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

translateC :: C.Header -> TH.DecsQ
translateC = translateHs . generateDeclarations

translateHs :: [Hs.Decl (Fresh BE)] -> TH.DecsQ
translateHs =
    aux . runM . mapM (toBE BE)
  where
    aux :: Q [TH.DecQ] -> TH.DecsQ
    aux = (>>= sequence)

