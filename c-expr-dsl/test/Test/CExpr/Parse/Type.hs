-- | Unit tests for 'C.Expr.Parse.Type.parseMacroType'
module Test.CExpr.Parse.Type (tests) where

import Data.Either (isLeft)
import Data.Vec.Lazy (Vec (..))
import Test.Tasty
import Test.Tasty.HUnit

import C.Expr.Syntax

import Clang.CStandard

import Test.CExpr.Parse.Infra

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Parse.Type" [
      testWithCStd cStd | cStd <- [minBound .. maxBound :: CStandard]
    ]

testWithCStd :: CStandard -> TestTree
testWithCStd cStd = testGroup (show cStd) [
      testGroup "void and bool"       $ tests_voidBool     std
    , testGroup "integer types"       $ tests_int          std
    , testGroup "char"                $ tests_char         std
    , testGroup "float and double"    $ tests_float        std
    , testGroup "named types"         $ tests_named        std
    , testGroup "tagged types"        $ tests_tagged       std
    , testGroup "const qualifier"     $ tests_const        std
    , testGroup "pointer indirection" $ tests_pointer      std
    , testGroup "combined"            $ tests_combined     std
    , testGroup "keyword order"       $ tests_keywordOrder std
    , testGroup "failures"            $ tests_failures     std
    ]
  where
    std = ClangCStandard cStd DisableGnu

{-------------------------------------------------------------------------------
  void and bool
-------------------------------------------------------------------------------}

tests_voidBool :: ClangCStandard -> [TestTree]
tests_voidBool cStd = [
      testCase "void" $
        -- void
        checkType cStd [kw "void"]
          @?= Right (tyLit TypeVoid)
    , testCase "_Bool" $
        -- _Bool
        checkType cStd [kw "_Bool"]
          @?= Right (tyLit TypeBool)
      -- 'bool' as CXToken_Keyword (Clang >= 16)
    , testCase "bool (keyword)" $
        -- bool
        let res = checkType cStd [kw "bool"]
        in  case cStd of
              ClangCStandard std _ | std >= C23 ->
                res @?= Right (tyLit TypeBool)
              _ ->
                assertBool "bool not a kw" $ isLeft res
      -- 'bool' as CXToken_Identifier (older Clang): treated as a named type
    , testCase "bool (identifier)" $
        -- bool
        checkType cStd [ident "bool"]
          @?= Right (Term (Var NoXVar "bool" []))
    ]

{-------------------------------------------------------------------------------
  Integer types
-------------------------------------------------------------------------------}

tests_int :: ClangCStandard -> [TestTree]
tests_int cStd = [
      testCase "int" $
        -- int
        checkType cStd  [kw "int"]
          @?= Right (tyLit (TypeInt Nothing (Just SizeInt)))
    , testCase "signed" $
        -- signed
        checkType cStd [kw "signed"]
          @?= Right (tyLit (TypeInt (Just Signed) Nothing))
    , testCase "unsigned" $
        -- unsigned
        checkType cStd [kw "unsigned"]
          @?= Right (tyLit (TypeInt (Just Unsigned) Nothing))
    , testCase "short" $
        -- short
        checkType cStd [kw "short"]
          @?= Right (tyLit (TypeInt Nothing (Just SizeShort)))
    , testCase "long" $
        -- long
        checkType cStd [kw "long"]
          @?= Right (tyLit (TypeInt Nothing (Just SizeLong)))
    , testCase "long long" $
        -- long long
        checkType cStd [kw "long", kw "long"]
          @?= Right (tyLit (TypeInt Nothing (Just SizeLongLong)))
    , testCase "unsigned int" $
        -- unsigned int
        checkType cStd [kw "unsigned", kw "int"]
          @?= Right (tyLit (TypeInt (Just Unsigned) (Just SizeInt)))
    , testCase "signed int" $
        -- signed int
        checkType cStd [kw "signed", kw "int"]
          @?= Right (tyLit (TypeInt (Just Signed) (Just SizeInt)))
    , testCase "unsigned short" $
        -- unsigned short
        checkType cStd [kw "unsigned", kw "short"]
          @?= Right (tyLit (TypeInt (Just Unsigned) (Just SizeShort)))
    , testCase "unsigned long" $
        -- unsigned long
        checkType cStd [kw "unsigned", kw "long"]
          @?= Right (tyLit (TypeInt (Just Unsigned) (Just SizeLong)))
    , testCase "unsigned long long" $
        -- unsigned long long
        checkType cStd [kw "unsigned", kw "long", kw "long"]
          @?= Right (tyLit (TypeInt (Just Unsigned) (Just SizeLongLong)))
    , testCase "long long int" $
        -- long long int
        checkType cStd [kw "long", kw "long", kw "int"]
          @?= Right (tyLit (TypeInt Nothing (Just SizeLongLong)))
    ]

{-------------------------------------------------------------------------------
  Char
-------------------------------------------------------------------------------}

tests_char :: ClangCStandard -> [TestTree]
tests_char cStd = [
      testCase "char" $
        -- char
        checkType cStd [kw "char"]
          @?= Right (tyLit (TypeChar Nothing))
    , testCase "signed char" $
        -- signed char
        checkType cStd [kw "signed", kw "char"]
          @?= Right (tyLit (TypeChar (Just Signed)))
    , testCase "unsigned char" $
        -- unsigned char
        checkType cStd [kw "unsigned", kw "char"]
          @?= Right (tyLit (TypeChar (Just Unsigned)))
    ]

{-------------------------------------------------------------------------------
  Float and double
-------------------------------------------------------------------------------}

tests_float :: ClangCStandard -> [TestTree]
tests_float cStd = [
      testCase "float" $
        -- float
        checkType cStd [kw "float"]
          @?= Right (tyLit (TypeFloat SizeFloat))
    , testCase "double" $
        -- double
        checkType cStd [kw "double"]
          @?= Right (tyLit (TypeFloat SizeDouble))
    ]

{-------------------------------------------------------------------------------
  Named types (identifiers)
-------------------------------------------------------------------------------}

-- After parsing, we cannot tell if these are types or value expressions.
-- However, they are parsed by the type branch of the parser ('parseMacroType').

tests_named :: ClangCStandard -> [TestTree]
tests_named cStd = [
      testCase "size_t" $
        -- size_t
        checkType cStd [ident "size_t"]
          @?= Right (Term (Var NoXVar "size_t" []))
    , testCase "uint32_t" $
        -- uint32_t
        checkType cStd [ident "uint32_t"]
          @?= Right (Term (Var NoXVar "uint32_t" []))
      -- An identifier token spelled "int" is treated as a named type,
      -- not as the built-in int keyword (libclang always tokenizes keywords
      -- as CXToken_Keyword, so this case is mainly for documentation)
    , testCase "int as identifier" $
        -- int  (tokenised as CXToken_Identifier, not CXToken_Keyword)
        checkType cStd [ident "int"]
          @?= Right (Term (Var NoXVar "int" []))
    ]

{-------------------------------------------------------------------------------
  Tagged types
-------------------------------------------------------------------------------}

tests_tagged :: ClangCStandard -> [TestTree]
tests_tagged cStd = [
      testCase "struct Foo" $
        -- struct Foo
        checkType cStd [kw "struct", ident "Foo"]
          @?= Right (Term $ Literal (TypeTagged TagStruct "Foo"))
    , testCase "union Bar" $
        -- union Bar
        checkType cStd [kw "union", ident "Bar"]
          @?= Right (Term $ Literal (TypeTagged TagUnion "Bar"))
    , testCase "enum Baz" $
        -- enum Baz
        checkType cStd [kw "enum", ident "Baz"]
          @?= Right (Term $ Literal (TypeTagged TagEnum "Baz"))
    ]

{-------------------------------------------------------------------------------
  Const qualifier
-------------------------------------------------------------------------------}

tests_const :: ClangCStandard -> [TestTree]
tests_const cStd = [
      testCase "const int (leading)" $
        -- const int
        checkType cStd [kw "const", kw "int"]
          @?= Right (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil))
    , testCase "int const (trailing)" $
        -- int const
        checkType cStd [kw "int", kw "const"]
          @?= Right (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil))
    , testCase "const void" $
        -- const void
        checkType cStd [kw "const", kw "void"]
          @?= Right (TyApp Const (tyLit TypeVoid ::: VNil))
    , testCase "const size_t" $
        -- const size_t
        checkType cStd [kw "const", ident "size_t"]
          @?= Right (TyApp Const (Term (Var NoXVar "size_t" []) ::: VNil))
    , testCase "const struct Foo" $
        -- const struct Foo
        checkType cStd [kw "const", kw "struct", ident "Foo"]
          @?= Right (TyApp Const (Term (Literal (TypeTagged TagStruct "Foo")) ::: VNil))
    ]

{-------------------------------------------------------------------------------
  Pointer indirection
-------------------------------------------------------------------------------}

tests_pointer :: ClangCStandard -> [TestTree]
tests_pointer cStd = [
      testCase "int*" $
        -- int *
        checkType cStd [kw "int", punc "*"]
          @?= Right (TyApp Pointer (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil))
    , testCase "int**" $
        -- int **
        checkType cStd [kw "int", punc "*", punc "*"]
          @?= Right (TyApp Pointer (TyApp Pointer (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil))
    , testCase "void*" $
        -- void *
        checkType cStd [kw "void", punc "*"]
          @?= Right (TyApp Pointer (tyLit TypeVoid ::: VNil))
    , testCase "size_t*" $
        -- size_t *
        checkType cStd [ident "size_t", punc "*"]
          @?= Right (TyApp Pointer (Term (Var NoXVar "size_t" []) ::: VNil))
    , testCase "struct Foo*" $
        -- struct Foo *
        checkType cStd [kw "struct", ident "Foo", punc "*"]
          @?= Right (TyApp Pointer (Term (Literal (TypeTagged TagStruct "Foo")) ::: VNil))
    ]

{-------------------------------------------------------------------------------
  Combined complex types
-------------------------------------------------------------------------------}

tests_combined :: ClangCStandard -> [TestTree]
tests_combined cStd = [
      testCase "const int * (pointer to const int, const left-hand side)" $
        checkType cStd [kw "const", kw "int", punc "*"]
          @?= Right (TyApp Pointer (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil))
    , testCase "int const * (pointer to const int, const right-hand side)" $
        checkType cStd [kw "int", kw "const", punc "*"]
          @?= Right (TyApp Pointer (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil))
    , testCase "int * const (const pointer to int)" $
        checkType cStd [kw "int", punc "*", kw "const"]
          @?= Right (TyApp Const (TyApp Pointer (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil))
    , testCase "const int * const (const pointer to const int, const left-hand side)" $
        checkType cStd [kw "const", kw "int", punc "*", kw "const"]
          @?= Right (TyApp Const (TyApp Pointer (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil) ::: VNil))
    , testCase "int const * const (const pointer to const int, const right-hand side)" $
        checkType cStd [kw "int", kw "const", punc "*", kw "const"]
          @?= Right (TyApp Const (TyApp Pointer (TyApp Const (tyLit (TypeInt Nothing (Just SizeInt)) ::: VNil) ::: VNil) ::: VNil))
    , testCase "const unsigned long*" $
        -- const unsigned long *
        checkType cStd [kw "const", kw "unsigned", kw "long", punc "*"]
          @?= Right (TyApp Pointer (TyApp Const (tyLit (TypeInt (Just Unsigned) (Just SizeLong)) ::: VNil) ::: VNil))
    , testCase "const struct Foo**" $
        -- const struct Foo **
        checkType cStd [kw "const", kw "struct", ident "Foo", punc "*", punc "*"]
          @?= Right (TyApp Pointer (TyApp Pointer (TyApp Const (Term (Literal (TypeTagged TagStruct "Foo")) ::: VNil) ::: VNil) ::: VNil))
    , testCase "unsigned long long int" $
        -- unsigned long long int
        checkType cStd [kw "unsigned", kw "long", kw "long", kw "int"]
          @?= Right (tyLit (TypeInt (Just Unsigned) (Just SizeLongLong)))
    ]

{-------------------------------------------------------------------------------
  Keyword order independence
-------------------------------------------------------------------------------}

tests_keywordOrder :: ClangCStandard -> [TestTree]
tests_keywordOrder cStd = [
      -- C allows type specifier keywords in any order
      testCase "long unsigned == unsigned long" $
        checkType cStd [kw "long", kw "unsigned"]
          @?= checkType cStd [kw "unsigned", kw "long"]
    , testCase "int unsigned == unsigned int" $
        checkType cStd [kw "int", kw "unsigned"]
          @?= checkType cStd [kw "unsigned", kw "int"]
    , testCase "int long unsigned == unsigned long int" $
        checkType cStd [kw "int", kw "long", kw "unsigned"]
          @?= checkType cStd [kw "unsigned", kw "long", kw "int"]
    ]

{-------------------------------------------------------------------------------
  Failure cases
-------------------------------------------------------------------------------}

tests_failures :: ClangCStandard -> [TestTree]
tests_failures cStd = [
      testCase "bare punctuation" $
        -- *  (no preceding type specifier)
        assertBool "expected failure" $ isLeft (checkType cStd [punc "*"])
    , testCase "struct without name" $
        -- struct  (tag keyword not followed by a name)
        assertBool "expected failure" $ isLeft (checkType cStd [kw "struct"])
    , testCase "void void" $
        -- void void  (duplicate specifier)
        assertBool "expected failure" $ isLeft (checkType cStd [kw "void", kw "void"])
    , testCase "float double" $
        -- float double  (conflicting float specifiers)
        assertBool "expected failure" $ isLeft (checkType cStd [kw "float", kw "double"])
    , testCase "int char" $
        -- int char  (conflicting specifiers)
        assertBool "expected failure" $ isLeft (checkType cStd [kw "int", kw "char"])
    , testCase "literal token" $
        -- 42  (integer literal is not a type token)
        assertBool "expected failure" $ isLeft (checkType cStd [lit "42"])
    , testCase "trailing tokens rejected" $
        -- int * extra  (parseMacroType alone would succeed on [int, *], but
        -- checkType adds eof so the trailing identifier causes failure)
        assertBool "expected failure" $
          isLeft (checkType cStd [kw "int", punc "*", ident "extra"])
    ]
