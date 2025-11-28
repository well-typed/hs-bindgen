module Test.HsBindgen.Unit.ClangArgs (tests) where

import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Resources
import Test.Tasty
import Test.Tasty.HUnit

import Clang.LowLevel.Core

import HsBindgen.Clang
import HsBindgen.Clang.ExtraClangArgs (splitArguments)
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

    triple @?= "x86_64-pc-linux-musl"
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
      -- Supported Windows target triple with appended MSVC version number
    , aux "x86_64-pc-windows-msvc19.50.35717" (Just Target_Windows_MSVC_X86_64)
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
