{-# LANGUAGE OverloadedStrings #-}

-- | Test exception handling in folds
--
-- Exception handling during folding is tricky, since each child in the AST is
-- processed by a separate callback from C to Haskell. We provide infrastructure
-- for handling this (making this transparent to the user), which we test here.
module Test.Test.Exceptions (tests) where

import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util.AST (AST (..))
import Test.Util.AST qualified as AST
import Test.Util.Clang qualified as Clang
import Test.Util.FoldException (FoldException (..))
import Test.Util.FoldException qualified as FoldException
import Test.Util.Input (TestInput)
import Test.Util.Input.Examples
import Test.Util.Input.StructForest (StructForest (..))
import Test.Util.Input.StructForest qualified as StructForest

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Test.Exceptions" [
      testGroup "Demo" [
          testCase "demo1" demo1
        , testCase "demo2" demo2
        , testCase "demo3" demo3
        ]
    , testGroup "Sanity" [
          testCase "withinLevel"  exceptions_withinLevel
        , testCase "acrossLevels" exceptions_acrossLevels
        , testCase "outsideFold"  exceptions_outsideFold
        ]
    , testProperty "property" prop_exceptions
    ]

{-------------------------------------------------------------------------------
  Demonstrate the need for specialized exception

  These examples are used in the documentation of 'foldWithHandler'.
-------------------------------------------------------------------------------}

demo1 :: Assertion
demo1 = do
    result <- Clang.parseUsing foldStruct $ AST.toTestInput ThreeStructs
    assertEqual "" result $ [["a", "b"], ["c", "d"], ["e", "f"]]
  where
    foldStruct :: Fold IO [Text]
    foldStruct = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          Right CXCursor_StructDecl ->
            foldRecursePure foldField concat
          _otherwise ->
            error $ "unexpected: " ++ show kind

    foldField :: Fold IO [Text]
    foldField = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          Right CXCursor_FieldDecl -> do
            name <- clang_getCursorSpelling curr
            foldContinueWith [name]
          _otherwise ->
            error $ "unexpected: " ++ show kind

demo2 :: Assertion
demo2 = do
    mResult <- try $ Clang.parseUsing foldStruct $ AST.toTestInput ThreeStructs
    case mResult of
      Left UnexpectedField -> return ()
      Right result -> assertFailure $ "Unexpected " ++ show result
  where
    foldStruct :: Fold IO [Text]
    foldStruct = simpleFold $ \curr ->
        handle (foldContinueWith . hasUnexpectedField) $ do
          kind <- fromSimpleEnum <$> clang_getCursorKind curr
          case kind of
            Right CXCursor_StructDecl ->
              foldRecursePure foldField concat
            _otherwise ->
              error $ "unexpected: " ++ show kind

    foldField :: Fold IO [Text]
    foldField = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          Right CXCursor_FieldDecl -> do
            name <- clang_getCursorSpelling curr
            if name == "c"
              then throwIO UnexpectedField
              else foldContinueWith [name]
          _otherwise ->
            error $ "unexpected: " ++ show kind

demo3 :: Assertion
demo3 = do
    result <- Clang.parseUsing foldStruct $ AST.toTestInput ThreeStructs
    assertEqual "" result $ [["a", "b"], [], ["e", "f"]]
  where
    foldStruct :: Fold IO [Text]
    foldStruct =
        foldWithHandler (\_curr -> return . Just . hasUnexpectedField) $ \curr -> do
          kind <- fromSimpleEnum <$> clang_getCursorKind curr
          case kind of
            Right CXCursor_StructDecl ->
              foldRecursePure foldField concat
            _otherwise ->
              error $ "unexpected: " ++ show kind

    foldField :: Fold IO [Text]
    foldField = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          Right CXCursor_FieldDecl -> do
            name <- clang_getCursorSpelling curr
            if name == "c"
              then throwIO UnexpectedField
              else foldContinueWith [name]
          _otherwise ->
            error $ "unexpected: " ++ show kind

data UnexpectedField = UnexpectedField
  deriving stock (Show)
  deriving anyclass (Exception)

hasUnexpectedField :: UnexpectedField -> [Text]
hasUnexpectedField UnexpectedField = []

{-------------------------------------------------------------------------------
  Sanity checks
-------------------------------------------------------------------------------}

-- | Sanity check: catch an exception within one level of the fold
exceptions_withinLevel :: Assertion
exceptions_withinLevel = do
    result <- AST.parseUsing fold $ AST.toTestInput SingleFunction
    assertEqual "" expected $ result
  where
    fold :: Fold IO (AST.Node AST.Descr)
    fold = foldWithHandler FoldException.handleAt $ \_curr ->
        throwIO $ FoldException 1

    expected :: AST AST.Descr
    expected = AST $ AST.Siblings [
          AST.Node descrAtException $ AST.Siblings []
        ]

    descrAtException :: AST.Descr
    descrAtException =
        FoldException.descrAt
          (AST.defaultDescr "f" CXCursor_FunctionDecl)
          (FoldException 1)

-- | At a higher level of the AST catch exception thrown at a lower level
exceptions_acrossLevels :: Assertion
exceptions_acrossLevels = do
    result<- AST.parseUsing higherLevel $ AST.toTestInput SingleStruct
    assertEqual "" expected $ result
  where
    higherLevel :: Fold IO (AST.Node AST.Descr)
    higherLevel = foldWithHandler FoldException.handleAt $ \curr -> do
        descr <- AST.descrAt curr
        foldRecursePure lowerLevel (AST.Node descr . AST.Siblings)

    lowerLevel :: Fold IO (AST.Node AST.Descr)
    lowerLevel = simpleFold $ \curr -> do
        descr <- AST.descrAt curr
        kind  <- clang_getCursorKind curr
        case fromSimpleEnum kind of
          Right CXCursor_FieldDecl ->
            throw $ FoldException 1
          _otherwise ->
            foldRecursePure lowerLevel (AST.Node descr . AST.Siblings)

    -- We only have an exception handler at the very top, so the exception is
    -- reported at @foo@, even though it was thrown at @x@.
    expected :: AST AST.Descr
    expected = AST $ AST.Siblings [
          AST.Node descrAtException $ AST.Siblings []
        ]

    descrAtException :: AST.Descr
    descrAtException =
        FoldException.descrAt
          (AST.defaultDescr "foo" CXCursor_StructDecl)
          (FoldException 1)

-- | Outside the scope of the fold catch exception thrown inside the scope
exceptions_outsideFold :: Assertion
exceptions_outsideFold = do
    result <- handle FoldException.handleTopLevel $
                AST.parseUsing fold $ AST.toTestInput SingleFunction
    assertEqual "" expected $ result
  where
    fold :: Fold IO (AST.Node AST.Descr)
    fold = simpleFold $ \_curr -> throwIO $ FoldException 1

    expected :: AST AST.Descr
    expected = AST $ AST.Siblings [
          AST.Node descrAtException $ AST.Siblings []
        ]

    descrAtException :: AST.Descr
    descrAtException = FoldException.descrTopLevel $ FoldException 1

{-------------------------------------------------------------------------------
  Exceptions: properties
-------------------------------------------------------------------------------}

prop_exceptions :: StructForest FoldException.Info -> Property
prop_exceptions structForest =
    counterexample ("test input: " ++ show testInput) $
      monadicIO go
  where
    testInput :: TestInput
    testInput = StructForest.toTestInput structForest

    expectedAST :: AST AST.Descr
    expectedAST =
        FoldException.model $
          StructForest.toAbstractAST FoldException.defaultInfo structForest

    go :: PropertyM IO Property
    go = do
        actualAST <- run $ FoldException.parse infoForCursor testInput
        return $ expectedAST === actualAST

    infoForCursor :: CXCursor -> IO FoldException.Info
    infoForCursor curr = do
        mKind <- fromSimpleEnum <$> clang_getCursorKind curr
        name  <- Text.unpack <$> clang_getCursorSpelling curr
        return $
          case mKind of
            Right CXCursor_StructDecl ->
              fromMaybe FoldException.defaultInfo $
                StructForest.lookup name structForest
            _otherwise ->
              FoldException.defaultInfo
