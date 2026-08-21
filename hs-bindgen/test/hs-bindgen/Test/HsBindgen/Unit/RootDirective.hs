module Test.HsBindgen.Unit.RootDirective (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Unit.RootDirective" [
      hashDefineToDirectiveTests
    , renderRootDirectivesTests
    ]

{-------------------------------------------------------------------------------
  HashDefine
-------------------------------------------------------------------------------}

-- | The rows of the correspondence table in t'C.HashDefine's haddock
hashDefineToDirectiveTests :: TestTree
hashDefineToDirectiveTests = testGroup "hashDefineToDirective" [
      testCase "-D FOO"        $ render "FOO"    "1"   @?= "#define FOO 1"
    , testCase "-D FOO=BAR"    $ render "FOO"    "BAR" @?= "#define FOO BAR"
    , testCase "-D FOO="       $ render "FOO"    ""    @?= "#define FOO"
    , testCase "-D 'FOO(x)=x'" $ render "FOO(x)" "x"   @?= "#define FOO(x) x"
    ]
  where
    render :: String -> String -> String
    render name value = C.hashDefineToDirective (C.HashDefine name value)

{-------------------------------------------------------------------------------
  RootDirective
-------------------------------------------------------------------------------}

renderRootDirectivesTests :: TestTree
renderRootDirectivesTests = testGroup "renderRootDirectives" [
      testCase "empty" $ C.renderRootDirectives [] @?= ""
    , testCase "order preserved" $
        C.renderRootDirectives [
            C.DirectiveHashDefine  (C.HashDefine "A" "1")
          , C.DirectiveHashInclude "a.h"
          , C.DirectiveHashDefine  (C.HashDefine "B" "")
          , C.DirectiveHashInclude "b.h"
          ]
          @?= unlines [
              "#define A 1"
            , "#include <a.h>"
            , "#define B"
            , "#include <b.h>"
            ]
    ]
