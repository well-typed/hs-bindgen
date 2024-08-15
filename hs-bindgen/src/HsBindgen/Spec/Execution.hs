-- | Execution of the spec
module HsBindgen.Spec.Execution (execSpec) where

import Control.Monad.IO.Class
import Language.Haskell.Meta qualified as Meta
import Text.Show.Pretty qualified as Pretty

import HsBindgen.C.AST qualified as C
import HsBindgen.C.Parser qualified as C
import HsBindgen.Hs.Render qualified as Hs
import HsBindgen.Spec
import HsBindgen.Translation.LowLevel
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Execute the spec
-------------------------------------------------------------------------------}

execSpec :: Spec result -> result
execSpec (Preprocess a b c) =
    processOutput c =<< (translate b) <$> prepareInput a
execSpec (GenSplice a b) = liftIO $
    map Meta.toDec <$> translate b <$> prepareInput a

{-------------------------------------------------------------------------------
  Prepare input
-------------------------------------------------------------------------------}

prepareInput :: PrepareInput inp -> IO inp
prepareInput (ParseCHeader tracer clangArgs fp) =
    C.parseHeader (contramap prettyLogMsg tracer) clangArgs fp
prepareInput (DumpClangAST clangArgs fp) =
    C.dumpClangAST clangArgs fp

{-------------------------------------------------------------------------------
  Main mode
-------------------------------------------------------------------------------}

translate :: Translation inp out -> inp -> out
translate NoTranslation    = id
translate GenDecls         = generateDeclarations . C.headerDecls
translate (GenModule opts) = generateModule opts

{-------------------------------------------------------------------------------
  Process output
-------------------------------------------------------------------------------}

processOutput :: ProcessOutput out -> out -> IO ()
processOutput PrettyC            = Pretty.dumpIO
processOutput (PrettyHs opts fp) = Hs.renderIO opts fp
processOutput NoOutput           = return