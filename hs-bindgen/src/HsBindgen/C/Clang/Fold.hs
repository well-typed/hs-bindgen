-- | Higher-level bindings for traversing the API
module HsBindgen.C.Clang.Fold (
    Fold
  , Next(..)
  , clang_fold
  ) where

import Control.Monad
import Data.IORef
import Foreign

import HsBindgen.C.Clang
import HsBindgen.Patterns

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

type Fold a = ForeignPtr CXCursor -> IO (Next a)

data Next a where
  Stop     :: Maybe a -> Next a
  Continue :: Maybe a -> Next a
  Recurse  :: Fold b -> ([b] -> IO a) -> Next a

{-------------------------------------------------------------------------------
  Internal: stack
-------------------------------------------------------------------------------}

data Processing a = Processing {
      -- | The AST node whose children we are processing
      parent :: ForeignPtr CXCursor

      -- | The 'Fold' we are applying at this level
    , currentFold :: Fold a

      -- | Results collected so far (in reverse order)
    , partialResults :: IORef [a]
    }

data Stack a where
  Bottom :: Processing a -> Stack a
  Push   :: Processing a -> ([a] -> IO b) -> Stack b -> Stack a

topProcessing :: Stack a -> Processing a
topProcessing (Bottom p)     = p
topProcessing (Push   p _ _) = p

topParent :: Stack a -> ForeignPtr CXCursor
topParent = parent . topProcessing

topResults :: Stack a -> IORef [a]
topResults = partialResults . topProcessing

data SomeStack where
 SomeStack :: Stack a -> SomeStack

initStack ::
     ForeignPtr CXCursor
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

push :: ForeignPtr CXCursor -> Fold b -> ([b] -> IO a) -> Stack a -> IO (Stack b)
push newParent fold collect stack = do
    partialResults <- newIORef []
    let p = Processing {
            parent      = newParent
          , currentFold = fold
          , partialResults
          }
    return $ Push p collect stack

popUntil :: IORef SomeStack -> ForeignPtr CXCursor -> IO ()
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
              b  <- collect (reverse as)
              modifyIORef (topResults stack') (b:)
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
clang_fold :: ForeignPtr CXCursor -> Fold a -> IO [a]
clang_fold root topLevelFold = do
    stack     <- initStack root topLevelFold
    someStack <- newIORef $ SomeStack stack
    _terminatedEarly <- clang_visitChildren root $ visitor someStack
    reverse <$> readIORef (topResults stack)
  where
    visitor ::
         IORef SomeStack
      -> ForeignPtr CXCursor
      -> ForeignPtr CXCursor
      -> IO (SimpleEnum CXChildVisitResult)
    visitor someStack current parent = do
        popUntil someStack parent
        SomeStack stack <- readIORef someStack
        let p = topProcessing stack
        next <- currentFold p current
        case next of
          Stop a -> do
            forM_ a $ modifyIORef (partialResults p) . (:)
            return $ simpleEnum CXChildVisit_Break
          Continue a -> do
            forM_ a $ modifyIORef (partialResults p) . (:)
            return $ simpleEnum CXChildVisit_Continue
          Recurse fold collect -> do
            stack' <- push current fold collect stack
            writeIORef someStack $ SomeStack stack'
            return $ simpleEnum CXChildVisit_Recurse
