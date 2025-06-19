{-# LANGUAGE OverloadedStrings #-}

module TestClangBindings.Traversal (tests) where

import Control.Exception
import Data.Default
import Data.Tree (Tree(..), Forest)
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "TestClangBindings.Traversal" [
      testGroup "AST" [
          testCase "singleFunction" ast_singleFunction
        , testCase "singleStruct"   ast_singleStruct
        ]
    ]

{-------------------------------------------------------------------------------
  Simple test: parse a file, check that we can traverse it and build the AST
-------------------------------------------------------------------------------}

testGetAST ::
     String                           -- ^ Header contents
  -> Forest (SimpleEnum CXCursorKind) -- ^ Expected AST
  -> Assertion
testGetAST test expected =
    withTest test $ \unit -> do
      root   <- clang_getTranslationUnitCursor unit
      result <- HighLevel.clang_visitChildren root astShape
      assertEqual "" expected $ result
  where
    astShape :: Fold IO (Tree (SimpleEnum CXCursorKind))
    astShape = simpleFold $ \curr -> do
        kind <- clang_getCursorKind curr
        return $ Recurse astShape (return . Just . Node kind)

ast_singleFunction :: Assertion
ast_singleFunction =
    testGetAST input_singleFunction expected
  where
    expected :: Forest (SimpleEnum CXCursorKind)
    expected = [
          Node (simpleEnum CXCursor_FunctionDecl) []
        ]

ast_singleStruct :: Assertion
ast_singleStruct =
    testGetAST input_singleStruct expected
  where
    expected :: Forest (SimpleEnum CXCursorKind)
    expected = [
          Node (simpleEnum CXCursor_StructDecl) [
              Node (simpleEnum CXCursor_FieldDecl) []
            , Node (simpleEnum CXCursor_FieldDecl) []
            ]
        ]

{-------------------------------------------------------------------------------
  Example inputs
-------------------------------------------------------------------------------}

input_singleFunction :: String
input_singleFunction = unlines [
      "void f();"
    ]

input_singleStruct :: String
input_singleStruct = unlines [
      "struct foo {"
    , "  int x;"
    , "  int y;"
    , "};"
    ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withTest :: String -> (CXTranslationUnit -> IO ()) -> IO ()
withTest test onSuccess =
    HighLevel.withUnsavedFile "test.h" test $ \input ->
    HighLevel.withIndex DisplayDiagnostics  $ \ix    ->
    HighLevel.withTranslationUnit2
      ix
      (Just "test.h")
      def
      [input]
      mempty
      onFailure
      onSuccess
  where
    onFailure :: SimpleEnum CXErrorCode -> IO ()
    onFailure = throwIO . userError . show
