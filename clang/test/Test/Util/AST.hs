-- | Abstract syntax tree
--
-- Intended for qualified import.
--
-- > import Test.Util.AST (AST(..))
-- > import Test.Util.AST qualified as AST
module Test.Util.AST (
    -- * Definition
    AST(..)
  , Siblings(..)
  , Node(..)
  , Descr(..)
  , defaultDescr
    -- * Concrete ASTs
  , IsConcrete(..)
  , ShowComment(..)
    -- * Clang interop
  , descrAt
  , fold
  , parse
  , parseUsing
  ) where

import Data.Text qualified as Text
import Data.Tree (Forest, Tree)
import Data.Tree qualified as Tree

import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import Test.Util.Clang qualified as Clang
import Test.Util.Input (TestInput)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Top-level AST
--
-- The most common instantiation of @a@ is 'Descr'.
data AST a = AST (Siblings a)
  deriving stock (Eq, Functor)

-- | Siblings (children of the same node)
data Siblings a = Siblings [Node a]
  deriving stock (Eq, Functor)

-- | Single node in the AST
data Node a = Node a (Siblings a)
  deriving stock (Eq, Functor)

-- | Description of a node in the tree
newtype Descr = Descr String
  deriving newtype (Eq, Show)

-- | Default description of a name and a kind
defaultDescr :: String -> CXCursorKind -> Descr
defaultDescr name kind = Descr $ name ++ " (" ++ show kind ++ ")"

{-------------------------------------------------------------------------------
  'Show' instances
-------------------------------------------------------------------------------}

-- | Produce human readable AST
--
-- This instance (as well as other 'Show' instances in the test infrastructure)
-- is not law-abiding, as it does not generate valid Haskell code. It is used by
-- QuickCheck for more readable output.
instance Show a => Show (AST a) where
  show = ("\n" ++) . Tree.drawTree . addTop . toForest . fmap show
    where
      -- An artificial @<top>@ node makes the output more readable
      -- (otherwise top-level siblings seem unrelated)
      addTop :: Forest String -> Tree String
      addTop = Tree.Node "<top>"

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

fromSiblings :: Siblings a -> [Node a]
fromSiblings (Siblings xs) = xs

-- | Internal: translate to 'Forest'
--
-- We don't use 'Forest' and 'Tree' directly because we want to use 'Siblings'
-- instead of @[]@ for improved type-level clarity.
toForest :: forall a. AST a -> Forest a
toForest = \(AST xs) -> goSiblings xs
  where
    goSiblings :: Siblings a -> Forest a
    goSiblings = map goNode . fromSiblings

    -- goNode :: Node a -> Tree a
    goNode :: Node a -> Tree a
    goNode (Node x xs) = Tree.Node x (goSiblings xs)

{-------------------------------------------------------------------------------
  Concrete ASTs
-------------------------------------------------------------------------------}

-- | Concrete AST
--
-- For something to be a concrete AST, we need to be able to produce a C
-- header ('TestInput') from it, such that when we parse that C input, the
-- resulting abstract AST is precisely the expected abstract AST.
class IsConcrete a where
  toTestInput   :: a -> TestInput
  toAbstractAST :: a -> AST Descr

-- | Show something as an optional C comment
--
-- This is sometimes helpful when implementing 'toTestInput'.
class ShowComment a where
  showComment :: a -> Maybe TestInput

instance ShowComment () where
  showComment _ = Nothing

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

descrAt :: CXCursor -> IO Descr
descrAt curr = do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    name  <- Text.unpack <$> clang_getCursorSpelling curr
    case mKind of
      Right kind -> return $ defaultDescr name kind
      Left  err  -> error $ "Unknown kind:  " ++ show err

-- | Construct the AST
--
-- Since this is for testing purposes, we ignore most of the contents of the
-- AST, recording only its shape and names.
fold :: Fold IO (Node Descr)
fold = simpleFold $ \curr -> do
    node <- descrAt curr
    foldRecursePure fold (Node node . Siblings)

parseUsing :: Fold IO (Node Descr) -> TestInput -> IO (AST Descr)
parseUsing f input = AST . Siblings <$> Clang.parseUsing f input

parse :: TestInput -> IO (AST Descr)
parse = parseUsing fold
