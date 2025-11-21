module Test.HsBindgen.Unit.ClangArgs (tests) where

import System.Environment (setEnv, unsetEnv)
import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Resources
import Test.Tasty
import Test.Tasty.HUnit

import Clang.LowLevel.Core

import HsBindgen.Clang
import HsBindgen.Clang.ExtraClangArgs (getExtraClangArgs, splitArguments)
import HsBindgen.Config.ClangArgs
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources = testGroup "Test.HsBindgen.Unit.ClangArgs" [
      testCase "getTargetTriple" $ testGetTargetTriple testResources
    , parseTargetTripleLenientTests
    , getExtraClangArgsTests
    , splitArgumentsTests
    ]

{-------------------------------------------------------------------------------
  Target triple
-------------------------------------------------------------------------------}

testGetTargetTriple :: IO TestResources -> Assertion
testGetTargetTriple testResources = do
    clangArgsConfig <- getTestDefaultClangArgsConfig testResources []
    clangArgs <- either (panicIO . show) return $ getClangArgs clangArgsConfig

    let setup :: ClangSetup
        setup = defaultClangSetup clangArgs $
                  ClangInputMemory "hs-bindgen-triple.h" ""

    triple <- withTracePredicate noReport defaultTracePredicate $ \tracer ->
      getTargetTriple tracer setup

    -- macos-latest (macos-14) returns "arm64-apple-macosx14.0.0"
    -- windows-latest (???) returns "x86_64-pc-windows-msvc19.41.34120"
    triple @?= "x86_64-pc-linux-gnu"
  where
    getTargetTriple :: Tracer ClangMsg -> ClangSetup -> IO Text
    getTargetTriple tracer setup =
        fmap (fromMaybe (panicPure "getTargetTriple failed")) $
        withClang tracer setup $ \unit -> Just <$>
          bracket
            (clang_getTranslationUnitTargetInfo unit)
            clang_TargetInfo_dispose
            clang_TargetInfo_getTriple

    noReport :: a -> IO ()
    noReport = const $ pure ()

parseTargetTripleLenientTests :: TestTree
parseTargetTripleLenientTests = testGroup "parseTargetTripleLenient" [
      -- Canonical Linux target triples
      aux "x86_64-pc-linux-gnu"   (Just Target_Linux_GNU_X86_64)
    , aux "x86_64-pc-linux-musl"  (Just Target_Linux_Musl_X86_64)
    , aux "i386-pc-linux-gnu"     (Just Target_Linux_GNU_X86)
    , aux "aarch64-pc-linux-gnu"  (Just Target_Linux_GNU_AArch64)
    , aux "aarch64-pc-linux-musl" (Just Target_Linux_Musl_AArch64)
      -- Supported alternate Linux machine architectures
    , aux "amd64-pc-linux-gnu"  (Just Target_Linux_GNU_X86_64)
    , aux "i486-pc-linux-gnu"   (Just Target_Linux_GNU_X86)
    , aux "i586-pc-linux-gnu"   (Just Target_Linux_GNU_X86)
    , aux "i686-pc-linux-gnu"   (Just Target_Linux_GNU_X86)
    , aux "arm64-pc-linux-gnu"  (Just Target_Linux_GNU_AArch64)
      -- Supported alternate Linux vendors
    , aux "x86_64-linux-gnu"         (Just Target_Linux_GNU_X86_64)
    , aux "x86_64-unknown-linux-gnu" (Just Target_Linux_GNU_X86_64)
      -- Invalid Linux target triples
    , aux "i386-pc-linux-musl" Nothing -- Musl 32-bit not supported
    , aux "x86_64-pc-linux"    Nothing -- Must specify GNU or Musl
      -- Canonical Windows target triples
    , aux "x86_64-pc-windows-msvc" (Just Target_Windows_MSVC_X86_64)
    , aux "x86_64-pc-windows-gnu"  (Just Target_Windows_GNU_X86_64)
      -- Supported alternate Windows machine architectures
    , aux "amd64-pc-windows-msvc" (Just Target_Windows_MSVC_X86_64)
      -- Supported alternate Windows vendors
    , aux "x86_64-windows-msvc"         (Just Target_Windows_MSVC_X86_64)
    , aux "x86_64-w64-windows-msvc"     (Just Target_Windows_MSVC_X86_64)
    , aux "x86_64-unknown-windows-msvc" (Just Target_Windows_MSVC_X86_64)
      -- Supported alternate Windows operating system enironments
    , aux "x86_64-w64-windows-mingw32" (Just Target_Windows_GNU_X86_64)
      -- Invalid Windows target triples
    , aux "i686-pc-windows-msvc" Nothing -- Windows 32-bit not supported
      -- Canonical Darwin target triples
    , aux "x86_64-apple-darwin"  (Just Target_Darwin_X86_64)
    , aux "aarch64-apple-darwin" (Just Target_Darwin_AArch64)
      -- Supported alternate Darwin machine architectures
    , aux "amd64-apple-darwin" (Just Target_Darwin_X86_64)
    , aux "arm64-apple-darwin" (Just Target_Darwin_AArch64)
      -- Supported Darwin target triple with appended version number
    , aux "arm64-apple-darwin24.6.0" (Just Target_Darwin_AArch64)
      -- Invalid Darwin target triples
    , aux "x86_64-pc-darwin" Nothing -- only support apple vendor
    ]
  where
    aux :: String -> Maybe Target -> TestTree
    aux tt mTarget =
      let label = case mTarget of
            Just{}  -> tt
            Nothing -> tt ++ " (invalid)"
      in  testCase label $ mTarget @=? parseTargetTripleLenient tt

{-------------------------------------------------------------------------------
  Get extra clang args
-------------------------------------------------------------------------------}

getExtraClangArgsTests :: TestTree
getExtraClangArgsTests = testGroup "getExtraClangArgs" [
      testCase "!target" $
        assertExtraClangArgs [(eDef, "native")] Nothing ["native"]
    , -- Without target, we ignore target-specific `clang` arguments.
      testCase "!target+other" $
        assertExtraClangArgs [(eDef, "native"), (eLnx, "cross")] Nothing ["native"]
    , testCase "target" $
        assertExtraClangArgs [(eLnx, "cross")] (Just Target_Linux_GNU_X86_64) ["cross"]
    , -- With target, we exclusively use target-specific `clang` arguments,
      -- if present.
      testCase "target+other" $
        assertExtraClangArgs [(eDef, "native"), (eLnx, "cross")] (Just Target_Linux_GNU_X86_64) ["cross"]
    , -- With target, we fall back to the default `clang` arguments.
      testCase "target+!other" $
        assertExtraClangArgs [(eDef, "native")] (Just Target_Linux_GNU_X86_64) ["native"]
    , testCase "target+otherEmpty" $
        assertExtraClangArgs [(eDef, "native"), (eLnx, "")] (Just Target_Linux_GNU_X86_64) ["native"]
    ]
  where
    assertExtraClangArgs :: [(EnvVar, EnvVal)] -> Maybe Target -> [String] -> IO ()
    assertExtraClangArgs xs mtarget x = do
        withTracePredicate noReport defaultTracePredicate $ \tracer ->
          assertWithEnv xs (getExtraClangArgs tracer mtarget) x

    noReport :: a -> IO ()
    noReport = const $ pure ()
{-------------------------------------------------------------------------------
  Split arguments
-------------------------------------------------------------------------------}

splitArgumentsTests :: TestTree
splitArgumentsTests = testGroup "splitStringArguments"
    [ testCase "simple"       $ splitArguments "a b"                   @?= ["a", "b"]
    , testCase "spacesPre"    $ splitArguments " \targ"                @?= ["arg"]
    , testCase "spacesSuf1"   $ splitArguments "arg\t "                @?= ["arg"]
    , testCase "spacesSuf2"   $ splitArguments "arg "                  @?= ["arg"]
    , testCase "spaces1"      $ splitArguments "  \t  "                @?= []
    , testCase "spaces2"      $ splitArguments " \t a b \t\n "         @?= ["a", "b"]
    , testCase "escape1"      $ splitArguments "a\\ b"                 @?= ["a b"]
    , testCase "escape2"      $ splitArguments "a1 arg\\ two a3"       @?= ["a1", "arg two", "a3"]
    , testCase "escapedEsape" $ splitArguments "c\\\\d"                @?= ["c\\d"]
    , testCase "escapedQuote" $ splitArguments "e\\\"f"                @?= ["e\"f"]
    , testCase "escaped^2"    $ splitArguments "c\\\\d e\\\"f"         @?= ["c\\d", "e\"f"]
    , testCase "escaped^3"    $ splitArguments " a\\ b c\\\\d e\\\"f " @?= ["a b", "c\\d", "e\"f"]
    , testCase "quote1"       $ splitArguments "\"a b\""               @?= ["a b"]
    , testCase "quote2"       $ splitArguments "a1 \"arg two\" a3"     @?= ["a1", "arg two", "a3"]
    , testCase "escape&quote" $ splitArguments "a\\ \"b c\"\\ d"       @?= ["a b c d"]
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary: setup environment
-------------------------------------------------------------------------------}

type EnvVar = String
type EnvVal = String

eDef :: EnvVar
eDef = "BINDGEN_EXTRA_CLANG_ARGS"

eLnx :: EnvVal
eLnx = eDef <> "_x86_64-pc-linux-gnu"

withEnv :: [(EnvVar, EnvVal)] -> IO a -> IO a
withEnv xs k = do
    mapM_ unsetEnv [eDef, eLnx]
    mapM_ (uncurry setEnv) xs
    r <- k
    mapM_ (unsetEnv . fst) xs
    pure r

assertWithEnv :: (Eq a, Show a) => [(EnvVar, EnvVal)] -> IO a -> a -> Assertion
assertWithEnv xs k x = withEnv xs k >>= \r -> r @?= x
