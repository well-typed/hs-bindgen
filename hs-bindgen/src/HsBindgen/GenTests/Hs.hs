module HsBindgen.GenTests.Hs (
    genTestsHs
  ) where

import HsBindgen.Hs.AST (Decl)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

genTestsHs ::
     FilePath  -- ^ Test module path
  -> String    -- ^ Generated Haskell module name
  -> FilePath  -- ^ C test header file path
  -> Int       -- ^ Maximum line length
  -> [Decl]    -- ^ Declarations
  -> IO ()
genTestsHs hsTestPath _moduleName _cTestHeaderPath _lineLength _decls =
    writeFile hsTestPath "TODO"
