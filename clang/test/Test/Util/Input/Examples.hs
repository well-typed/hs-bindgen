-- | Example inputs
--
-- Intended for unqualified import.
module Test.Util.Input.Examples (ExampleInput(..)) where

import Clang.LowLevel.Core (CXCursorKind(..))

import Test.Util.AST (AST(..))
import Test.Util.AST qualified as AST
import Test.Util.Input (TestInput)
import Test.Util.Input qualified as TestInput

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data ExampleInput =
    SingleFunction
  | SingleStruct
  | ThreeStructs

instance AST.IsConcrete ExampleInput where
  toTestInput = \case
      SingleFunction -> inputSingleFunction
      SingleStruct   -> inputSingleStruct
      ThreeStructs   -> inputThreeStructs

  toAbstractAST = \case
      SingleFunction -> astSingleFunction
      SingleStruct   -> astSingleStruct
      ThreeStructs   -> astThreeStructs

{-------------------------------------------------------------------------------
  Example: single function
-------------------------------------------------------------------------------}

astSingleFunction :: AST AST.Descr
astSingleFunction = AST $ AST.Siblings [
      AST.Node (AST.defaultDescr "f" CXCursor_FunctionDecl) $
        AST.Siblings []
    ]

inputSingleFunction :: TestInput
inputSingleFunction = TestInput.unlines [
      "void f();"
    ]

{-------------------------------------------------------------------------------
  Example: single struct
-------------------------------------------------------------------------------}

inputSingleStruct :: TestInput
inputSingleStruct = TestInput.unlines [
      "struct foo {"
    , "  int x;"
    , "  int y;"
    , "};"
    ]

astSingleStruct :: AST AST.Descr
astSingleStruct = AST $ AST.Siblings [
      AST.Node (AST.defaultDescr "foo" CXCursor_StructDecl) $ AST.Siblings [
          AST.Node (AST.defaultDescr "x" CXCursor_FieldDecl) $ AST.Siblings []
        , AST.Node (AST.defaultDescr "y" CXCursor_FieldDecl) $ AST.Siblings []
        ]
    ]

{-------------------------------------------------------------------------------
  Example: three structs
-------------------------------------------------------------------------------}

inputThreeStructs :: TestInput
inputThreeStructs = TestInput.unlines [
      "struct foo {"
    , "  int a;"
    , "  int b;"
    , "};"
    , ""
    , "struct bar {"
    , "  int c;"
    , "  int d;"
    , "};"
    , ""
    , "struct baz {"
    , "  int e;"
    , "  int f;"
    , "};"
    ]

astThreeStructs :: AST AST.Descr
astThreeStructs = AST $ AST.Siblings [
      AST.Node (AST.defaultDescr "foo" CXCursor_StructDecl) $ AST.Siblings [
          AST.Node (AST.defaultDescr "a" CXCursor_FieldDecl) $ AST.Siblings []
        , AST.Node (AST.defaultDescr "b" CXCursor_FieldDecl) $ AST.Siblings []
        ]
    , AST.Node (AST.defaultDescr "bar" CXCursor_StructDecl) $ AST.Siblings [
          AST.Node (AST.defaultDescr "c" CXCursor_FieldDecl) $ AST.Siblings []
        , AST.Node (AST.defaultDescr "d" CXCursor_FieldDecl) $ AST.Siblings []
        ]
    , AST.Node (AST.defaultDescr "baz" CXCursor_StructDecl) $ AST.Siblings [
          AST.Node (AST.defaultDescr "e" CXCursor_FieldDecl) $ AST.Siblings []
        , AST.Node (AST.defaultDescr "f" CXCursor_FieldDecl) $ AST.Siblings []
        ]
    ]
