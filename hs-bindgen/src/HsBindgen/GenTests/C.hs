module HsBindgen.GenTests.C (
    genTestsC
  ) where

import Data.Char qualified as Char
import Data.List qualified as List
import System.FilePath qualified as FilePath

import Text.SimplePrettyPrint

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

genTestsC ::
     FilePath  -- ^ C test header file path
  -> FilePath  -- ^ C test source file path
  -> Int       -- ^ Maximum line length
  -> FilePath  -- ^ C header path
  -> IO ()
genTestsC cTestHeaderPath cTestSourcePath lineLength cHeaderPath = do
    writeFile cTestHeaderPath . renderPretty (mkContext lineLength) $
      CTestHeader {
          cTestHeaderIncludeGuard = getIncludeGuard cTestHeaderFilename
        , cTestHeaderUsrIncludes  = [cHeaderFilename]
        , cTestHeaderSysIncludes  = ["stdbool.h", "stddef.h"]
        }
    writeFile cTestSourcePath . renderPretty (mkContext lineLength) $
      CTestSource {
          cTestSourceUsrIncludes = List.sort
            ["hs_bindgen_testlib.h" , cHeaderFilename , cTestHeaderFilename]
        , cTestSourceSysIncludes = ["stdalign.h", "stdbool.h", "stddef.h"]
        }
  where
    cTestHeaderFilename, cHeaderFilename :: FilePath
    cTestHeaderFilename = FilePath.takeFileName cTestHeaderPath
    cHeaderFilename     = FilePath.takeFileName cHeaderPath

{-------------------------------------------------------------------------------
  AST
-------------------------------------------------------------------------------}

data CTestHeader = CTestHeader {
      cTestHeaderIncludeGuard :: String
    , cTestHeaderUsrIncludes  :: [String]
    , cTestHeaderSysIncludes  :: [String]
    -- TODO
    }

data CTestSource = CTestSource {
      cTestSourceUsrIncludes :: [String]
    , cTestSourceSysIncludes :: [String]
    -- TODO
    }

{-------------------------------------------------------------------------------
  Pretty printing
-------------------------------------------------------------------------------}

instance Pretty CTestHeader where
  pretty CTestHeader{..} = vsep $
      vcat
        [ "#ifndef" <+> string cTestHeaderIncludeGuard
        , "#define" <+> string cTestHeaderIncludeGuard
        ]
    : vcat ["#include \"" >< string inc >< "\"" | inc <- cTestHeaderUsrIncludes]
    : vcat ["#include <" >< string inc >< ">" | inc <- cTestHeaderSysIncludes]
    : "// TODO"
    : ["#endif //" <+> string cTestHeaderIncludeGuard]

instance Pretty CTestSource where
  pretty CTestSource{..} = vsep $
      vcat ["#include \"" >< string inc >< "\"" | inc <- cTestSourceUsrIncludes]
    : vcat ["#include <" >< string inc >< ">" | inc <- cTestSourceSysIncludes]
    : ["// TODO"]

{-------------------------------------------------------------------------------
  Auxillary functions
-------------------------------------------------------------------------------}

getIncludeGuard ::
     FilePath  -- ^ C test header filename
  -> String
getIncludeGuard = List.map aux
  where
    aux :: Char -> Char
    aux c
      | Char.isAlphaNum c = Char.toUpper c
      | otherwise         = '_'
