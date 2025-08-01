-- | Test resources (for integration with tasty)
--
-- Intended for unqualified import.
module Test.HsBindgen.Resources (
    TestResources -- opaque
  , withTestResources
    -- * Use the resources
  , getTestPackageRoot
  , getTestDefaultClangArgs
  , getTestDefaultConfig
  , getTestDefaultExtSpec
    -- ** rust-bindgen
  , RustBindgenResult(..)
  , callRustBindgen
    -- ** hs-bindgen
  , normaliseCommentOriginDecl
  , normaliseCommentOriginSDecl
  , normaliseCommentOrigin
  ) where

import Data.Text qualified as Text

import System.Exit (ExitCode (..))
import Test.Tasty

import Clang.Args
import HsBindgen.Lib

import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Haddock.Documentation (Comment(..))
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.Util.Parsec (parseUnnamed)

import Test.Common.HsBindgen.TracePredicate
import Test.Common.Util.Cabal
import Test.HsBindgen.Resources.Rust
import Text.Parsec qualified as P

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data TestResources = TestResources {
      -- | Package root
      testPackageRoot :: FilePath

      -- | Clang arguments we use when running the tests
      --
      -- NOTE: Individual tests will need to add their required include dirs.
    , testClangArgs :: ClangArgs

      -- | Default external binding specification
    , testExtSpec :: BindingSpec

      -- | Path to @rust-bindgen@, if available
    , testRustBindgen :: RustBindgen
    }

{-------------------------------------------------------------------------------
  Acquisition and release
-------------------------------------------------------------------------------}

withTestResources :: (IO TestResources -> TestTree) -> TestTree
withTestResources = withResource initTestResources freeTestResources

initTestResources :: IO TestResources
initTestResources = do
    testPackageRoot <- findPackageDirectory "hs-bindgen"
    let testClangArgs = mkTestClangArgs testPackageRoot
    testExtSpec     <- initExtBindingSpec testClangArgs
    testRustBindgen <- initRustBindgen
    return TestResources{..}

freeTestResources :: TestResources -> IO ()
freeTestResources TestResources{..} =
    freeRustBindgen testRustBindgen

{-------------------------------------------------------------------------------
  Package root
-------------------------------------------------------------------------------}

getTestPackageRoot :: IO TestResources -> IO FilePath
getTestPackageRoot = fmap testPackageRoot

{-------------------------------------------------------------------------------
  Clang arguments
-------------------------------------------------------------------------------}

mkTestClangArgs :: FilePath -> ClangArgs
mkTestClangArgs packageRoot = def {
      clangTarget = Just $
        (Target_Linux_X86_64, TargetEnvOverride "gnu")
    , clangCStandard = Just $
        C23
    , clangExtraIncludeDirs = [
          CIncludeDir (packageRoot </> "musl-include/x86_64")
        ]
    }

getTestDefaultClangArgs :: IO TestResources -> [FilePath] -> IO ClangArgs
getTestDefaultClangArgs testResources extraIncludeDirs =
    aux <$> testResources
  where
    aux :: TestResources -> ClangArgs
    aux TestResources{testPackageRoot, testClangArgs} = testClangArgs{
          clangExtraIncludeDirs =
               -- NOTE: The include search path is traversed from left to right.
               -- That is, earlier flags overrule later flags, and so, the
               -- test-specific include directories must come before the default
               -- include directories.
               map (CIncludeDir . (</>) testPackageRoot) extraIncludeDirs
            <> clangExtraIncludeDirs testClangArgs
        }

{-------------------------------------------------------------------------------
  Test configuration
-------------------------------------------------------------------------------}

getTestDefaultConfig :: IO TestResources -> [FilePath] -> IO Config
getTestDefaultConfig testResources extraIncludeDirs = do
    aux <$> getTestDefaultClangArgs testResources extraIncludeDirs
  where
    aux :: ClangArgs -> Config
    aux clangArgs = def{
          configClangArgs    = clangArgs
        , configHsModuleOpts = HsModuleOpts{hsModuleOptsName = "Example"}
        }

{-------------------------------------------------------------------------------
  Binding specifications
-------------------------------------------------------------------------------}

initExtBindingSpec :: ClangArgs -> IO BindingSpec
initExtBindingSpec clangArgs =
    -- This trace predicate is used only during resolution of the default
    -- binding specifications.
    withTracePredicate defaultTracePredicate $ \tracer ->
      loadExtBindingSpecs tracer clangArgs EnableStdlibBindingSpec []

getTestDefaultExtSpec :: IO TestResources -> IO BindingSpec
getTestDefaultExtSpec = fmap testExtSpec

{-------------------------------------------------------------------------------
  rust-bindgen
-------------------------------------------------------------------------------}

data RustBindgenResult =
    RustBindgenSuccess String          -- ^ stdout on success
  | RustBindgenFailed ExitCode String  -- ^ stderr on failure
  | RustBindgenNotCalled

callRustBindgen ::
     IO TestResources
  -> ClangArgs
     -- ^ Clang arguments
     --
     -- We take this as an explicit argument rather than calling
     -- 'getTestDefaultClangArgs' here, because individual tests may override
     -- those default arguments.
  -> FilePath
  -> IO RustBindgenResult
callRustBindgen testResources clangArgs input = do
    TestResources{..} <- testResources
    case testRustBindgen of
      RustBindgenInPath     path -> go path
      RustBindgenDownloaded path -> go path
      RustBindgenUnavailable     -> return RustBindgenNotCalled
  where
    go :: FilePath -> IO RustBindgenResult
    go path = do
        (exitCode, stdout, stderr) <- runRustBindgen clangArgs path input
        case exitCode of
          ExitSuccess -> return $ RustBindgenSuccess stdout
          _otherwise  -> return $ RustBindgenFailed exitCode stderr


{-------------------------------------------------------------------------------
  hs-bindgen
-------------------------------------------------------------------------------}

-- | Normalises all Comments in 'Hs.Decl'
--
normaliseCommentOriginDecl :: Hs.Decl -> Hs.Decl
normaliseCommentOriginDecl = \case
  Hs.DeclData x@Hs.Struct{..} ->
    Hs.DeclData x { Hs.structComment = normaliseCommentOrigin structComment
                  , Hs.structFields  = fmap normaliseCommentOriginHsField structFields
                  }
  Hs.DeclEmpty x@Hs.EmptyData{..} ->
    Hs.DeclEmpty x { Hs.emptyDataComment = normaliseCommentOrigin emptyDataComment }
  Hs.DeclNewtype x@Hs.Newtype{..} ->
    Hs.DeclNewtype x { Hs.newtypeComment = normaliseCommentOrigin newtypeComment
                     , Hs.newtypeField   = normaliseCommentOriginHsField newtypeField
                     }
  Hs.DeclPatSyn x@Hs.PatSyn{..}         ->
    Hs.DeclPatSyn x { Hs.patSynComment = normaliseCommentOrigin patSynComment }
  Hs.DeclDefineInstance x@Hs.DefineInstance{..} ->
    Hs.DeclDefineInstance x { Hs.defineInstanceComment = normaliseCommentOrigin defineInstanceComment }
  Hs.DeclDeriveInstance x@Hs.DeriveInstance{..} ->
    Hs.DeclDeriveInstance x { Hs.deriveInstanceComment = normaliseCommentOrigin deriveInstanceComment }
  Hs.DeclForeignImport x@Hs.ForeignImportDecl{..} ->
    Hs.DeclForeignImport x { Hs.foreignImportComment = normaliseCommentOrigin foreignImportComment }
  Hs.DeclVar x@Hs.VarDecl{..} ->
    Hs.DeclVar x { Hs.varDeclComment = normaliseCommentOrigin varDeclComment }
  Hs.DeclUnionGetter x@Hs.UnionGetter{..} ->
    Hs.DeclUnionGetter x { Hs.unionGetterComment = normaliseCommentOrigin unionGetterComment }
  Hs.DeclUnionSetter x@Hs.UnionSetter{..} ->
    Hs.DeclUnionSetter x { Hs.unionSetterComment = normaliseCommentOrigin unionSetterComment }
  Hs.DeclSimple sdecl ->
    Hs.DeclSimple (normaliseCommentOriginSDecl sdecl)
  x -> x

-- | Normalises all Comments in 'SHs.SDecl'
--
normaliseCommentOriginSDecl :: SHs.SDecl -> SHs.SDecl
normaliseCommentOriginSDecl = \case
    SHs.DVar x@SHs.Var {..} ->
      SHs.DVar x { SHs.varComment = normaliseCommentOrigin varComment }
    SHs.DInst x@SHs.Instance{..} ->
      SHs.DInst x { SHs.instanceComment = normaliseCommentOrigin instanceComment }
    SHs.DRecord x@SHs.Record{..} ->
      SHs.DRecord x { SHs.dataComment = normaliseCommentOrigin dataComment
                    , SHs.dataFields  = map normaliseCommentOriginSHsField dataFields
                    }
    SHs.DEmptyData x@SHs.EmptyData{..} ->
      SHs.DEmptyData x { SHs.emptyDataComment = normaliseCommentOrigin emptyDataComment }
    SHs.DNewtype x@SHs.Newtype{..} ->
      SHs.DNewtype x { SHs.newtypeComment = normaliseCommentOrigin newtypeComment
                     , SHs.newtypeField   = normaliseCommentOriginSHsField newtypeField
                     }
    SHs.DForeignImport x@SHs.ForeignImport{..} ->
      SHs.DForeignImport x { SHs.foreignImportComment = normaliseCommentOrigin foreignImportComment }
    SHs.DDerivingInstance x@SHs.DerivingInstance {..} ->
      SHs.DDerivingInstance x { SHs.derivingInstanceComment = normaliseCommentOrigin derivingInstanceComment }
    SHs.DPatternSynonym x@SHs.PatternSynonym {..} ->
      SHs.DPatternSynonym x { SHs.patSynComment = normaliseCommentOrigin patSynComment }
    SHs.DCSource src -> SHs.DCSource src

-- | Normalises all Comments in 'SHs.Field'
--
normaliseCommentOriginSHsField :: SHs.Field -> SHs.Field
normaliseCommentOriginSHsField x@SHs.Field{..} =
  x { SHs.fieldComment = normaliseCommentOrigin fieldComment }

-- | Normalises all Comments in 'Hs.Field'
--
normaliseCommentOriginHsField :: Hs.Field -> Hs.Field
normaliseCommentOriginHsField x@Hs.Field{..} =
  x { Hs.fieldComment = normaliseCommentOrigin fieldComment }

-- | Parses "unnamed at" paths in C names.
--
-- E.g., "struct (unnamed at /path/to/file.h:123:4)" -> "struct (unnamed at file.h)"
--
normaliseCommentOrigin :: Maybe Comment -> Maybe Comment
normaliseCommentOrigin mbComment = do
  c@Comment{..} <- mbComment
  originalCName <- commentOrigin
  pure $
    case P.parse parseUnnamed "" (Text.unpack originalCName) of
      Left _        -> c { commentOrigin = commentOrigin }
      Right unnamed -> c { commentOrigin = Just unnamed }

