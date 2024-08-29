-- | Higher-level bindings for traversing the API
module HsBindgen.Clang.Aux.Fold (
    Fold
  , Next(..)
  , clang_fold
  ) where

import Control.Monad
import Data.IORef

import HsBindgen.Clang.Core
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Typed fold over the AST
--
-- This is similar to 'CXCursorVisitor', but
--
-- * we allow for (typed) results
-- * when recursing into the children of a node, we get to specify a /different/
--   function
--
-- This provides for a much nicer user experience.
type Fold a = CXCursor -> IO (Next a)

-- | Result of visiting one node
--
-- This is the equivalent of 'CXChildVisitResult'
data Next a where
  -- | Stop folding early
  --
  -- This is the equivalent of 'CXChildVisit_Break'.
  Stop :: Maybe a -> Next a

  -- | Continue with the next sibling of the current node
  --
  -- This is the equivalent of 'CXChildVisit_Continue'.
  Continue :: Maybe a -> Next a

  -- | Recurse into the children of the current node
  --
  -- We can specify a different 'Fold' to process the children, and must
  -- provide a "summarize" function which turns the results obtained from
  -- processing the children into a result for the parent.
  --
  -- This is the equivalent of 'CXChildVisit_Recurse'.
  Recurse :: Fold b -> ([b] -> IO (Maybe a)) -> Next a

{-------------------------------------------------------------------------------
  Internal: stack
-------------------------------------------------------------------------------}

data Processing a = Processing {
      -- | The AST node whose children we are processing
      parent :: CXCursor

      -- | The 'Fold' we are applying at this level
    , currentFold :: Fold a

      -- | Results collected so far (in reverse order)
    , partialResults :: IORef [a]
    }

data Stack a where
  Bottom :: Processing a -> Stack a
  Push   :: Processing a -> ([a] -> IO (Maybe b)) -> Stack b -> Stack a

topProcessing :: Stack a -> Processing a
topProcessing (Bottom p)     = p
topProcessing (Push   p _ _) = p

topParent :: Stack a -> CXCursor
topParent = parent . topProcessing

topResults :: Stack a -> IORef [a]
topResults = partialResults . topProcessing

data SomeStack where
 SomeStack :: Stack a -> SomeStack

initStack ::
     CXCursor
  -> Fold a
  -> IO (Stack a)
initStack root topLevelFold = do
    partialResults <- newIORef []
    let p = Processing {
            parent      = root
          , currentFold = topLevelFold
          , partialResults
          }
    return $ Bottom p

push ::
     CXCursor
  -> Fold b
  -> ([b] -> IO (Maybe a))
  -> Stack a -> IO (Stack b)
push newParent fold collect stack = do
    partialResults <- newIORef []
    let p = Processing {
            parent      = newParent
          , currentFold = fold
          , partialResults
          }
    return $ Push p collect stack

popUntil :: IORef SomeStack -> CXCursor -> IO ()
popUntil someStack newParent = do
    SomeStack stack <- readIORef someStack
    writeIORef someStack =<< loop stack
  where
    loop :: Stack a -> IO SomeStack
    loop stack = do
        arrived <- clang_equalCursors (topParent stack) newParent
        if arrived then
          return $ SomeStack stack
        else
          case stack of
            Bottom _ ->
              error "popUntil: something has gone horribly wrong"
            Push p collect stack' -> do
              as <- readIORef (partialResults p)
              mb <- collect (reverse as)
              forM_ mb $ modifyIORef (topResults stack') . (:)
              loop stack'

{-------------------------------------------------------------------------------
  Traversal proper
-------------------------------------------------------------------------------}

-- | Fold the AST
--
-- This provides a higher-level API to 'clang_visitChildren', in which
--
-- * visitors can return results
-- * we can specify different visitors at different levels of the AST
clang_fold :: CXCursor -> Fold a -> IO [a]
clang_fold root topLevelFold = do
    stack     <- initStack root topLevelFold
    someStack <- newIORef $ SomeStack stack
    _terminatedEarly <- clang_visitChildren root $ visitor someStack
    popUntil someStack root
    reverse <$> readIORef (topResults stack)
  where
    visitor ::
         IORef SomeStack
      -> CXCursor
      -> CXCursor
      -> IO (SimpleEnum CXChildVisitResult)
    visitor someStack current parent = do
        popUntil someStack parent
        SomeStack stack <- readIORef someStack
        let p = topProcessing stack
        next <- currentFold p current
        case next of
          Stop ma -> do
            forM_ ma $ modifyIORef (partialResults p) . (:)
            return $ simpleEnum CXChildVisit_Break
          Continue ma -> do
            forM_ ma $ modifyIORef (partialResults p) . (:)
            return $ simpleEnum CXChildVisit_Continue
          Recurse fold collect -> do
            stack' <- push current fold collect stack
            writeIORef someStack $ SomeStack stack'
            return $ simpleEnum CXChildVisit_Recurse
