module HsBindgen.GenTests (
    genTests
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import System.Directory qualified as Dir
import System.FilePath qualified as FilePath

import HsBindgen.Clang.Paths
import HsBindgen.GenTests.C (genTestsC)
import HsBindgen.GenTests.Hs (genTestsHs)
import HsBindgen.GenTests.Readme (genTestsReadme)
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Generate test suite
genTests ::
     CHeaderIncludePath
  -> [Hs.Decl]
  -> String   -- ^ Generated Haskell module name
  -> Int      -- ^ Maximum line length
  -> FilePath -- ^ Test suite directory path
  -> IO ()
genTests headerIncludePath decls moduleName lineLength testSuitePath = do
    -- fails when testSuitePath already exists
    mapM_ Dir.createDirectory $
      testSuitePath : cbitsPath : srcPath : modulePaths
    genTestsReadme
      readmePath
      moduleName
      testSuitePath
      cTestHeaderPath
      cTestSourcePath
    genTestsC
      cTestHeaderPath
      cTestSourcePath
      lineLength
      headerIncludePath
      decls
    genTestsHs
      hsTestPath
      hsSpecPath
      hsMainPath
      moduleName
      cTestHeaderPath
      lineLength
      decls
  where
    readmePath, cbitsPath, srcPath :: FilePath
    readmePath = FilePath.combine testSuitePath "README.md"
    cbitsPath  = FilePath.combine testSuitePath "cbits"
    srcPath    = FilePath.combine testSuitePath "src"

    cTestHeaderPath, cTestSourcePath :: FilePath
    (cTestHeaderPath, cTestSourcePath) =
      bimap (FilePath.combine cbitsPath) (FilePath.combine cbitsPath) $
        getModuleCFilenames moduleName

    modulePath, hsTestPath, hsSpecPath, hsMainPath :: FilePath
    modulePaths :: [FilePath]
    (modulePath, modulePaths) = getModuleDirectories srcPath moduleName
    hsTestPath                = FilePath.combine modulePath "Test.hs"
    hsSpecPath                = FilePath.combine srcPath "Spec.hs"
    hsMainPath                = FilePath.combine srcPath "Main.hs"

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

getModuleCFilenames ::
     String               -- ^ Module name (example: @Acme.Foo@)
  -> (FilePath, FilePath) -- ^ Header and source filenames
getModuleCFilenames moduleName =
    let basename = "test_" ++ List.map aux moduleName
    in  (FilePath.addExtension basename "h", FilePath.addExtension basename "c")
  where
    aux :: Char -> Char
    aux c
      | Char.isAlphaNum c = Char.toLower c
      | otherwise         = '_'

getModuleDirectories ::
     FilePath                -- ^ Parent directory
  -> String                  -- ^ Module name (example: @Acme.Foo@)
  -> (FilePath, [FilePath])  -- ^ Module directory and directories to create
getModuleDirectories parentDir = aux [] . List.break (== '.')
  where
    aux :: [FilePath] -> (String, String) -> (FilePath, [FilePath])
    aux [] = \case
      (part, []) ->
        let path' = FilePath.combine parentDir part
        in  (path', [path'])
      (part, _dot : s) ->
        aux [FilePath.combine parentDir part] $ List.break (== '.') s
    aux acc@(path:_paths) = \case
      (part, []) ->
        let path' = FilePath.combine path part
        in  (path', reverse (path' : acc))
      (part, _dot : s) ->
        aux (FilePath.combine path part : acc) $ List.break (== '.') s
