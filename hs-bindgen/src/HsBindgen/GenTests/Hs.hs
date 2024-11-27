module HsBindgen.GenTests.Hs (
    genTestsHs
  ) where

import HsBindgen.SHs.AST (SDecl)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

genTestsHs ::
     FilePath  -- ^ Test module path
  -> [SDecl]
  -> IO ()
genTestsHs hsTestPath _decls =
    writeFile hsTestPath "TODO"
