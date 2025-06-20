-- | Randomly generated C input: tree of structs
--
-- Intended for qualified import.
--
-- > import Test.Util.Input.StructForest (StructForest)
-- > import Test.Util.Input.StructForest qualified as StructForest
module Test.Util.Input.StructForest (
    -- * Definition
    StructForest(StructForest, structForest)
  , StructTree(..)
  , StructField(..)
    -- * Query
  , lookup
    -- * Execution
  , toAbstractAST
  , toTestInput
  ) where

import Prelude hiding (lookup)

import Data.Foldable (asum)
import Data.List qualified as List
import Data.String
import Data.Tree (Tree(Node))
import Test.QuickCheck

import Clang.LowLevel.Core (CXCursorKind(..))

import Test.Util.AST (AST(..))
import Test.Util.AST qualified as AST
import Test.Util.Input (TestInput(..))
import Test.Util.Input qualified as Input
import Test.Util.Shape (Shape)
import Test.Util.Shape qualified as Shape

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | List of struct trees
--
-- The annotation type (@a@) is used for 'FoldException.Info' or @()@.
data StructForest a =
    StructForest {
        structForest      :: [StructTree a]
      , structForestShape :: Shape a
      }
  deriving stock (Show)

-- | Tree of structs
data StructTree a =
    StructTree {
        structName   :: String
      , structFields :: [StructField a]
      , structAnn    :: a
      }
  deriving stock (Show)

data StructField a =
    StructField {
        fieldName :: String
      , fieldType :: FieldType a
      }
  deriving stock (Show)

data FieldType a =
    TypeInt
  | TypeStruct (StructTree a)
  deriving stock (Show)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

instance Arbitrary a => Arbitrary (StructForest a) where
  arbitrary = fromShape <$> arbitrary1
  shrink    = map fromShape . shrink1 . structForestShape

fromShape :: forall a. Shape a -> StructForest a
fromShape structForestShape = StructForest {
      structForest = map struct $ Shape.toForest structForestShape
    , structForestShape
    }
  where
    struct :: Tree (a, [Int]) -> StructTree a
    struct (Node (x, path) children) = StructTree {
          structName   = mkStructName path
        , structFields = map field children
        , structAnn    = x
        }

    field :: Tree (a, [Int]) -> StructField a
    field node@(Node (_x, path) children) = StructField {
          fieldName = mkFieldName path
        , fieldType = case children of
                        [] -> TypeInt
                        _  -> TypeStruct (struct node)
        }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | Get annotation of specified struct
lookup :: forall a. String -> StructForest a -> Maybe a
lookup key StructForest{structForest} =
    asum $ map goStruct structForest
  where
    goStruct :: StructTree a -> Maybe a
    goStruct StructTree{structName, structFields, structAnn}
      | structName == key = Just structAnn
      | otherwise         = asum $ map goField structFields

    goField :: StructField a -> Maybe a
    goField StructField{fieldType} =
        case fieldType of
          TypeInt           -> Nothing
          TypeStruct struct -> goStruct struct

{-------------------------------------------------------------------------------
  Expected AST
-------------------------------------------------------------------------------}

toAbstractAST :: forall a.
     a -- ^ Annotation on fields
  -> StructForest a -> AST (AST.Descr, a)
toAbstractAST fieldAnn =
    AST . AST.Siblings . map goStruct . structForest
  where
    goStruct :: StructTree a -> AST.Node (AST.Descr, a)
    goStruct StructTree{structName, structFields, structAnn} =
        AST.Node (descr, structAnn) $
          AST.Siblings (concatMap goField structFields)
      where
        descr :: AST.Descr
        descr = AST.defaultDescr structName CXCursor_StructDecl

    goField :: StructField a -> [AST.Node (AST.Descr, a)]
    goField StructField{fieldName, fieldType} =
        case fieldType of
          TypeInt -> [
              AST.Node (descr, fieldAnn) $ AST.Siblings []
            ]
          -- The clang AST has a weird quirk, where the struct is repeated (or
          -- at least visited) /twice/: once before the field, and once as a
          -- child /of/ the field.
          TypeStruct struct -> [
              goStruct struct
            , AST.Node (descr, fieldAnn) $ AST.Siblings [goStruct struct]
            ]
      where
        descr :: AST.Descr
        descr = AST.defaultDescr fieldName CXCursor_FieldDecl

{-------------------------------------------------------------------------------
  Generate test input
-------------------------------------------------------------------------------}

toTestInput :: forall a. AST.ShowComment a => StructForest a -> TestInput
toTestInput StructForest{structForest} = mconcat [
      Input.unlines [
          "#ifndef STRUCT_TREE"
        , "#define STRUCT_TREE"
        , ""
        ]
    , Input.intercalate "\n\n" $ map (goStruct Nothing) structForest
    , Input.unlines [
            ""
          , "#endif // STRUCT_TREE"
          ]
    ]
  where
    goStruct :: Maybe String -> StructTree a -> TestInput
    goStruct mFieldName StructTree{structName, structFields, structAnn} =
        Input.intercalate "\n" . mconcat $ [
            [ comment
            | Just comment <- [AST.showComment structAnn]
            ]
          , [ fromString $ "struct " ++ structName ++ " {"
            , Input.indent $ Input.intercalate "\n" $ map goField structFields
            , fromString $ "}" ++ maybe "" (" " ++) mFieldName ++ ";"
            ]
          ]

    goField :: StructField a -> TestInput
    goField StructField{fieldName, fieldType} =
        case fieldType of
          TypeInt           -> fromString $ "int " ++ fieldName ++ ";"
          TypeStruct struct -> goStruct (Just fieldName) struct

{-------------------------------------------------------------------------------
  'IsConcrete'
-------------------------------------------------------------------------------}

instance AST.IsConcrete (StructForest ()) where
  toAbstractAST = fmap fst . toAbstractAST ()
  toTestInput   = toTestInput

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

renderPath :: [Int] -> String
renderPath = List.intercalate "_" . map show

mkStructName :: [Int] -> [Char]
mkStructName path = "s" ++ renderPath path

mkFieldName :: [Int] -> String
mkFieldName path = "f" ++ renderPath path

