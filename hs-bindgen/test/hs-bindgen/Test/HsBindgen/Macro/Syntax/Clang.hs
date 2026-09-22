-- | Tests for 'splitMacro' on token streams that @libclang@ actually produced
--
-- "Test.HsBindgen.Macro.Syntax" drives the same splitter with synthetic tokens
-- laid out by "Test.HsBindgen.Macro.Infra". That layout is a /model/ of how
-- @libclang@ assigns extents, and the model is load-bearing: the
-- object-like\/function-like decision is an extent comparison. These tests
-- validate the model by asserting the same 'Runtime.Macro.Raw' values against real
-- tokens.
--
-- Malformed definitions are not repeated here. @clang@ rejects them outright
-- (@#define F(x,) x@ is an error, not a macro we get to split), so they can only
-- be expressed synthetically.
module Test.HsBindgen.Macro.Syntax.Clang (tests) where

import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), Gen, Property, chooseInt,
                              elements, frequency, ioProperty, listOf, resize,
                              shuffle, testProperty, (===))

import Clang.Args (ClangArgs (ClangArgs))
import Clang.HighLevel.Types (Token, TokenSpelling)
import Clang.Version (ClangVersion (ClangVersion), runtimeClangVersion)

import HsBindgen.Runtime.Macro qualified as Runtime.Macro

import HsBindgen.Macro.Error (MacroParseError (..))
import HsBindgen.Macro.Parse (spelling)
import HsBindgen.Macro.Syntax (splitMacro)

import Test.HsBindgen.Clang (collectMacroTokens)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.HsBindgen.Macro.Syntax.Clang" $
      map testsFor standards
   ++ [testProperty "prop_renderRoundtrips" prop_renderRoundtrips]

-- | The definitions of one C standard, sharing a single @libclang@ invocation
testsFor :: CStandard -> TestTree
testsFor cStd =
    withResource (collectMacroTokens (clangArgs cStd) header) (const (pure ())) $
      \getMacros -> testGroup (show cStd) $
          testCase "all definitions found" (allFound getMacros)
        : [ testCase (label c) (checks getMacros c) | c <- cases ]
        ++ [ testCase "line continuation inside a token spelling"
               (continuationSpellings getMacros)
           ]

-- | The header driving all cases of one standard
header :: String
header = unlines $ map source cases

{-------------------------------------------------------------------------------
  C standards
-------------------------------------------------------------------------------}

-- | The standards the cases are run under
--
-- The standard decides which spellings @libclang@ reports as keywords rather
-- than identifiers: @bool@ is a keyword under C23 and an identifier under C17.
-- Running every case under both pins that the splitter is indifferent.
data CStandard = C17 | C23

instance Show CStandard where
  show = \case
      C17 -> "C17"
      C23 -> "C23"

clangArgs :: CStandard -> ClangArgs
clangArgs = ClangArgs . pure . \case
    C17 -> "-std=c17"
    C23 -> "-std=c2x"

standards :: [CStandard]
standards = C17 : [C23 | c23Supported]
  where
    c23Supported :: Bool
    c23Supported = case runtimeClangVersion of
        ClangVersion v -> v >= (15, 0, 0)
        _otherwise     -> False

{-------------------------------------------------------------------------------
  Cases
-------------------------------------------------------------------------------}

-- | A macro definition and the split it should produce
--
-- The source line is assembled from the name, so the name asserted against the
-- cursor spelling and the name in the source cannot drift apart.
data Case = Case {
      name     :: Text
      -- | Everything following the macro name, including any white space
    , rest     :: String
    , expected :: Runtime.Macro.Raw String
    }

source :: Case -> String
source c = "#define " ++ Text.unpack c.name ++ c.rest

-- | The source on one line, for use as a test name
label :: Case -> String
label = concatMap escape . source
  where
    escape :: Char -> String
    escape '\n' = "\\n"
    escape c    = [c]

cases :: [Case]
cases = [
      -- Object-like
      Case "FOO" " 1" $
        Runtime.Macro.objectLike "FOO" ["1"]
    , Case "EMPTY" "" $
        Runtime.Macro.objectLike "EMPTY" []
      -- White space before the @(@ makes this object-like, with the parentheses
      -- part of the body; see #1903.
    , Case "PARENS" " (1)" $
        Runtime.Macro.objectLike "PARENS" ["(", "1", ")"]

      -- Function-like
    , Case "ADD" "(x, y) x + y" $
        Runtime.Macro.functionLike "ADD" ["x", "y"] ["x", "+", "y"]
      -- Distinct from an object-like macro: the empty parameter list is
      -- 'Runtime.Macro.Params' @[] False@, not 'Runtime.Macro.NoParams'.
    , Case "NOW" "() 0" $
        Runtime.Macro.functionLike "NOW" [] ["0"]
    , Case "IGNORE" "(x)" $
        Runtime.Macro.functionLike "IGNORE" ["x"] []

      -- Variadic
    , Case "LOG" "(fmt, ...) fmt" $
        Runtime.Macro.variadic "LOG" ["fmt"] ["fmt"]
    , Case "WARN" "(...) __VA_ARGS__" $
        Runtime.Macro.variadic "WARN" [] ["__VA_ARGS__"]
      -- The GNU named variadic form: the name before the @...@ is not a
      -- parameter of its own.
    , Case "GNU" "(args...) args" $
        Runtime.Macro.variadicNamed "GNU" [] "args" ["args"]
    , Case "GNULOG" "(fmt, args...) fmt" $
        Runtime.Macro.variadicNamed "GNULOG" ["fmt"] "args" ["fmt"]

      -- A macro definition may give a new meaning to a keyword, so the name
      -- accepts one; so does a parameter, which the preprocessor sees as a
      -- pp-token either way.
    , Case "bool" " int" $
        Runtime.Macro.objectLike "bool" ["int"]
    , Case "KWPARAM" "(bool) bool" $
        Runtime.Macro.functionLike "KWPARAM" ["bool"] ["bool"]

      -- The body is returned verbatim, whatever it contains
    , Case "PASTE" "(a, b) a ## b" $
        Runtime.Macro.functionLike "PASTE" ["a", "b"] ["a", "##", "b"]
    , Case "STR" "(x) #x" $
        Runtime.Macro.functionLike "STR" ["x"] ["#", "x"]
    , Case "CALL" "(f) FOO(f)" $
        Runtime.Macro.functionLike "CALL" ["f"] ["FOO", "(", "f", ")"]

      -- A comment separates the name from the @(@, so the macro is object-like
      -- and the comment is part of the body.
    , Case "COMMENT" "/*c*/(1)" $
        Runtime.Macro.objectLike "COMMENT" ["/*c*/", "(", "1", ")"]

      -- A line continuation splices the lines before the macro is parsed, so
      -- the @(@ is still adjacent to the name.
    , continuation
    , Case "CONTP" "(x, \\\n      y) x" $
        Runtime.Macro.functionLike "CONTP" ["x", "y"] ["x"]
    , continuationInParams
    ]

-- | A line continuation between the name and the parameter list
--
-- Singled out because 'continuationSpellings' asserts its token spellings.
continuation :: Case
continuation = Case "CONT" "\\\n(x) x" $
    Runtime.Macro.functionLike "CONT" ["x"] ["x"]

-- | A line continuation inside the parameter list
--
-- Singled out for the same reason as 'continuation'.
continuationInParams :: Case
continuationInParams = Case "CONTC" "(x\\\n, y) x" $
    Runtime.Macro.functionLike "CONTC" ["x", "y"] ["x"]

{-------------------------------------------------------------------------------
  Assertions
-------------------------------------------------------------------------------}

type Macros = [(Text, [Token TokenSpelling])]

-- | @libclang@ reported a definition for every case, in source order
allFound :: IO Macros -> Assertion
allFound getMacros = do
    macros <- getMacros
    map (.name) cases @=? map fst macros

checks :: IO Macros -> Case -> Assertion
checks getMacros c = do
    tokens <- tokensOf getMacros c
    Right (Text.pack <$> c.expected) @=? split tokens

-- | @libclang@ puts a line continuation inside the token that follows it
--
-- This is the observation 'HsBindgen.Macro.Parse.removeMultilines' exists for:
-- without it the @(@ of @CONT@ would not compare equal to @\"(\"@ and the macro
-- would come out object-like.
continuationSpellings :: IO Macros -> Assertion
continuationSpellings getMacros = do
    nameToParen <- tokensOf getMacros continuation
    ["CONT", "\\\n(", "x", ")", "x"] @=? map spelling nameToParen
    inParams <- tokensOf getMacros continuationInParams
    ["CONTC", "(", "x", "\\\n,", "y", ")", "x"] @=? map spelling inParams

tokensOf :: IO Macros -> Case -> IO [Token TokenSpelling]
tokensOf getMacros c = do
    macros <- getMacros
    case lookup c.name macros of
      Just tokens -> pure tokens
      Nothing     -> assertFailure $
        "libclang reported no definition of " ++ Text.unpack c.name

-- | Split, keeping only the spellings; the splitter does not change them
split :: [Token TokenSpelling] -> Either String (Runtime.Macro.Raw Text)
split tokens =
    case splitMacro tokens of
      Left  err -> Left err.macroParseError
      Right raw -> Right $ spelling <$> raw

{-------------------------------------------------------------------------------
  Round trip
-------------------------------------------------------------------------------}

-- | 'Runtime.Macro.render' and 'splitMacro' are inverse
--
-- \[
--   \text{splitMacro} ~ (\text{tokenize} ~ (\text{render} ~ r)) = r
-- \]
--
-- Only in this direction: 'Runtime.Macro.render' is canonical, not faithful, so
-- @#define ADD(x,y) x+y@ does not survive a round trip the other way round.
-- 'Definitions' generates canonical values only.
prop_renderRoundtrips :: Definitions -> Property
prop_renderRoundtrips defs = ioProperty $ do
    macros <- collectMacroTokens (clangArgs C17) (renderAll defs)
    -- pure $ map Right defs.unwrap === map (split . snd) macros
    pure $ [ Right (Text.pack <$> m) | m <- defs.unwrap ] === map (split . snd) macros

-- | A header's worth of macro definitions
--
-- The names are made distinct, because @clang@ rejects a redefinition with a
-- different body and this property is not about redefinition.
newtype Definitions = Definitions { unwrap :: [Runtime.Macro.Raw String] }

instance Show Definitions where
  show = renderAll

renderAll :: Definitions -> String
renderAll defs = unlines $ map Runtime.Macro.render defs.unwrap

instance Arbitrary Definitions where
  arbitrary = Definitions . zipWith rename [0 :: Int ..] <$> listOf genMacro
    where
      rename :: Int -> Runtime.Macro.Raw String -> Runtime.Macro.Raw String
      rename i raw = raw{Runtime.Macro.name = raw.name <> "_" <> show i}

-- | A macro whose rendering @libclang@ tokenizes back to the same tokens
--
-- The body alphabet avoids everything that would not survive: no @#@ or @##@
-- (@clang@ rejects both outside their proper context), no unterminated literal
-- and no stray backslash. Adjacent tokens cannot merge, because
-- 'Runtime.Macro.render' separates them with a space.
genMacro :: Gen (Runtime.Macro.Raw String)
genMacro = do
    theName   <- elements ["A", "B", "FOO", "M", "x1"]
    theParams <- genParams
    theBody   <- resize 5 . listOf . elements $
                   paramNames theParams ++ bodyAlphabet
    pure Runtime.Macro.Raw{
        Runtime.Macro.name   = theName
      , Runtime.Macro.params = theParams
      , Runtime.Macro.body   = theBody
      }
  where
    genParams :: Gen (Runtime.Macro.Params String)
    genParams = frequency [
          (1, pure Runtime.Macro.NoParams)
        , (3, Runtime.Macro.Params <$> genParamNames <*> genVariadic)
        ]

    -- @args@ is not in 'genParamNames', so the GNU name never repeats a
    -- parameter.
    genVariadic :: Gen (Runtime.Macro.Variadic String)
    genVariadic = frequency [
          (3, pure Runtime.Macro.NotVariadic)
        , (1, pure Runtime.Macro.Ellipsis)
        , (1, pure $ Runtime.Macro.NamedEllipsis "args")
        ]

    -- Distinct, because @clang@ rejects a repeated parameter name.
    genParamNames :: Gen [String]
    genParamNames = do
      n <- chooseInt (0, 3)
      take n <$> shuffle ["a", "b", "c", "x"]

    -- Includes the GNU name, which is a parameter like any other.
    paramNames :: Runtime.Macro.Params String -> [String]
    paramNames Runtime.Macro.NoParams             = []
    paramNames (Runtime.Macro.Params ps variadic) = ps ++ toList variadic

    bodyAlphabet :: [String]
    bodyAlphabet = [
          "x", "y", "FOO"
        , "0", "1", "42", "0x1f", "1.5", "\"s\"", "'c'"
        , "+", "-", "*", "/", "(", ")", ",", "<", ">", "==", "&&"
        ]
