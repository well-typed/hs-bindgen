module Test.CExpr.Typecheck.Classify (
    tests
  ) where

import Data.Vec.Lazy (Vec (..))
import DeBruijn (Idx (..), pattern I1)
import Test.Tasty
import Test.Tasty.HUnit

import C.Expr.Syntax

import Test.CExpr.Typecheck.Infra

tests :: TestTree
tests = testGroup "classify" [
      tests_keywordTypes
    , tests_typeApp
    , tests_intLiterals
    , tests_arithmetic
    , tests_functionLike
    , tests_typeEnvChain
    , tests_errors
    ]

{-------------------------------------------------------------------------------
  Group 1: keyword type bodies
-------------------------------------------------------------------------------}

tests_keywordTypes :: TestTree
tests_keywordTypes = testGroup "keyword type bodies" [
      testCase "void"       $ assertTypeMacro $ classifyOne "M" VNil (tyLit TypeVoid)
    , testCase "int"        $ assertTypeMacro $ classifyOne "M" VNil (tyLit (TypeInt Nothing (Just SizeInt)))
    , testCase "unsigned"   $ assertTypeMacro $ classifyOne "M" VNil (tyLit (TypeInt (Just Unsigned) Nothing))
    , testCase "float"      $ assertTypeMacro $ classifyOne "M" VNil (tyLit (TypeFloat SizeFloat))
    , testCase "double"     $ assertTypeMacro $ classifyOne "M" VNil (tyLit (TypeFloat SizeDouble))
    , testCase "_Bool"      $ assertTypeMacro $ classifyOne "M" VNil (tyLit TypeBool)
    , testCase "char"       $ assertTypeMacro $ classifyOne "M" VNil (tyLit (TypeChar Nothing))
    , testCase "struct Foo" $ assertTypeMacro $ classifyOne "M" VNil (Term (Literal (TypeTagged TagStruct "Foo")))
    , testCase "union Bar"  $ assertTypeMacro $ classifyOne "M" VNil (Term (Literal (TypeTagged TagUnion  "Bar")))
    , testCase "enum Baz"   $ assertTypeMacro $ classifyOne "M" VNil (Term (Literal (TypeTagged TagEnum   "Baz")))
    ]

{-------------------------------------------------------------------------------
  Group 2: type application bodies
-------------------------------------------------------------------------------}

tests_typeApp :: TestTree
tests_typeApp = testGroup "type application bodies" [
      testCase "int *"        $ assertTypeMacro $ classifyOne "M" VNil (ptrOf (tyLit intTy))
    , testCase "const int"    $ assertTypeMacro $ classifyOne "M" VNil (constOf (tyLit intTy))
    , testCase "const int *"  $ assertTypeMacro $ classifyOne "M" VNil (ptrOf (constOf (tyLit intTy)))
    , testCase "int * const"  $ assertTypeMacro $ classifyOne "M" VNil (constOf (ptrOf (tyLit intTy)))
    , testCase "void *"       $ assertTypeMacro $ classifyOne "M" VNil (ptrOf (tyLit TypeVoid))
    , testCase "struct Foo *" $ assertTypeMacro $ classifyOne "M" VNil (ptrOf (Term (Literal (TypeTagged TagStruct "Foo"))))
    ]

{-------------------------------------------------------------------------------
  Group 3: integer literal bodies
-------------------------------------------------------------------------------}

tests_intLiterals :: TestTree
tests_intLiterals = testGroup "integer literal bodies" [
      testCase "0"   $ assertValueMacro $ classifyOne "M" VNil (intLit 0)
    , testCase "1"   $ assertValueMacro $ classifyOne "M" VNil (intLit 1)
    , testCase "42"  $ assertValueMacro $ classifyOne "M" VNil (intLit 42)
    , testCase "-1"  $ assertValueMacro $ classifyOne "M" VNil (intLit (-1))
    ]

{-------------------------------------------------------------------------------
  Group 4: arithmetic expression bodies
-------------------------------------------------------------------------------}

tests_arithmetic :: TestTree
tests_arithmetic = testGroup "arithmetic expression bodies" [
      testCase "1 + 2"   $ assertValueMacro $ classifyOne "M" VNil (add (intLit 1) (intLit 2))
    , testCase "1 << 4"  $ assertValueMacro $ classifyOne "M" VNil (shiftLeft (intLit 1) (intLit 4))
    ]

{-------------------------------------------------------------------------------
  Group 5: function-like macro bodies (with formal parameters)
-------------------------------------------------------------------------------}

tests_functionLike :: TestTree
tests_functionLike = testGroup "function-like macro bodies" [
      testCase "identity: \\x -> x" $
        assertValueMacro $
          classifyOne "IDENTITY" ("x" ::: VNil) (mlocal IZ)
    , testCase "add: \\a b -> a + b" $
        assertValueMacro $
          classifyOne "ADD" ("a" ::: "b" ::: VNil) (add (mlocal I1) (mlocal IZ))
    ]

{-------------------------------------------------------------------------------
  Group 6: TypeEnv chain — value macro references
-------------------------------------------------------------------------------}

tests_typeEnvChain :: TestTree
tests_typeEnvChain = testGroup "TypeEnv chain (value macro references)" [
      testCase "B references A (both value macros)" $ do
        let results = runTcSeq
              [ ("A", intLit 1)
              , ("B", mvar "A")
              ]
        mapM_ assertValueMacro results

    , testCase "C references B which references A" $ do
        let results = runTcSeq
              [ ("A", intLit 42)
              , ("B", mvar "A")
              , ("C", add (mvar "B") (intLit 1))
              ]
        assertValueMacro $ last results
    ]

{-------------------------------------------------------------------------------
  Group 7: error cases
-------------------------------------------------------------------------------}

tests_errors :: TestTree
tests_errors = testGroup "error cases" [
      testCase "unbound variable" $
        -- A bare identifier not in TypeEnv and not a macro argument is an
        -- unbound variable.  'tcMacro' returns Left (TcErrors [UnboundVariable ...]).
        case classifyOne "M" VNil (mvar "unknown") of
          Left  _ -> pure ()
          Right _ -> assertFailure "expected Left for unbound variable"

    , testCase "value macro referencing unknown name" $
        -- Even inside arithmetic, an unbound reference fails.
        case classifyOne "M" VNil (add (intLit 1) (mvar "UNDEFINED")) of
          Left  _ -> pure ()
          Right _ -> assertFailure "expected Left for unbound reference in arithmetic"
    ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Signed int literal, used in multiple test groups.
intTy :: TypeLit
intTy = TypeInt (Just Signed) (Just SizeInt)
