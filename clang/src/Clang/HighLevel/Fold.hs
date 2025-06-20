-- | Higher-level bindings for traversing the API
--
-- Intended for unqualified import.
module Clang.HighLevel.Fold (
    -- * Folds
    Fold -- opaque
  , Next(..)
    -- * Construction
  , simpleFold
  , runFold
  , continueWith
  , recursePure
    -- * Execution
  , clang_visitChildren
  ) where

import Control.Exception
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.IORef (IORef, writeIORef, readIORef, modifyIORef, newIORef)

import Clang.Enum.Simple
import Clang.LowLevel.Core hiding (clang_visitChildren)
import Clang.LowLevel.Core qualified as Core

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
data Fold m a = Fold{
      getNext :: CXCursor -> m (Next m a)
    }

-- | Construct simple fold
simpleFold :: (CXCursor -> m (Next m a)) -> Fold m a
simpleFold = Fold

-- | Run 'Fold'
runFold :: Fold m a -> (CXCursor -> m (Next m a))
runFold Fold{getNext} = getNext

-- | Result of visiting one node
--
-- This is the equivalent of 'CXChildVisitResult'
data Next m a where
  -- | Stop folding early
  --
  -- This is the equivalent of 'CXChildVisit_Break'.
  Break :: Maybe a -> Next m a

  -- | Continue with the next sibling of the current node
  --
  -- This is the equivalent of 'CXChildVisit_Continue'.
  Continue :: Maybe a -> Next m a

  -- | Recurse into the children of the current node
  --
  -- We can specify a different 'Fold' to process the children, and must
  -- provide a "summarize" function which turns the results obtained from
  -- processing the children into a result for the parent.
  --
  -- This is the equivalent of 'CXChildVisit_Recurse'.
  Recurse :: Fold m b -> ([b] -> m (Maybe a)) -> Next m a

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

instance Functor m => Functor (Next m) where
  fmap f (Break x)     = Break (fmap f x)
  fmap f (Continue x)  = Continue (fmap f x)
  fmap f (Recurse r g) = Recurse r (fmap (fmap f) . g)

instance Functor m => Functor (Fold m) where
  fmap f Fold{getNext} = Fold{
        getNext = \curr -> fmap f <$> getNext curr
      }

continueWith :: Applicative m => Maybe a -> Fold m a
continueWith x = simpleFold $ \_curr -> pure (Continue x)

recursePure :: Applicative m => Fold m b -> ([b] -> Maybe a) -> Next m a
recursePure r f = Recurse r (pure . f)

{-------------------------------------------------------------------------------
  Internal: partial results
-------------------------------------------------------------------------------}

type PartialResults a = IORef (Either SomeException [a])

newPartialResults :: IO (PartialResults a)
newPartialResults = newIORef (Right [])

addPartialResult :: PartialResults a -> a -> IO ()
addPartialResult partial a = modifyIORef partial $
    either unexpectedException $ \as -> Right (a:as)

_recordException :: PartialResults a -> SomeException -> IO ()
_recordException partial newErr = modifyIORef partial $
    either unexpectedException $ \_as -> Left newErr

-- | Existing exceptions are impossible
--
-- We 'Break' after the first exception (see implementation of
-- 'clang_visitChildren' below), so when we update the partial results, we never
-- expect to see an exception already present.
--
-- The use of @error@ here is an exception in its own right, which we don't
-- propagate (it will be caught in the low-level 'Core.clang_visitChildren'
-- function). However, that's okay: if this @error@ ever triggers, it indicates
-- a bug in this infrastructure, which can't really be handlded anyway.
unexpectedException :: SomeException -> a
unexpectedException err = error $ concat [
      "The impossible happened: we break at the first exception, "
    , "yet here we are: " ++ show err
    ]

getPartialResults :: PartialResults a -> IO [a]
getPartialResults partial =
        readIORef partial
    >>= either throwIO (return . reverse)

{-------------------------------------------------------------------------------
  Internal: stack
-------------------------------------------------------------------------------}

data Processing m a = Processing {
      -- | The AST node whose children we are processing
      parent :: CXCursor

      -- | The 'Fold' we are applying at this level
    , currentFold :: Fold m a

      -- | Results collected so far (in reverse order)
    , partialResults :: PartialResults a
    }

data Stack m a where
  Bottom :: Processing m a -> Stack m a
  Push   :: Processing m a -> ([a] -> m (Maybe b)) -> Stack m b -> Stack m a

topProcessing :: Stack m a -> Processing m a
topProcessing (Bottom p)     = p
topProcessing (Push   p _ _) = p

topParent :: Stack m a -> CXCursor
topParent = parent . topProcessing

topResults :: Stack m a -> PartialResults a
topResults = partialResults . topProcessing

data SomeStack m where
 SomeStack :: Stack m a -> SomeStack m

initStack ::
     CXCursor
  -> Fold m a
  -> IO (Stack m a)
initStack root topLevelFold = do
    partialResults <- newPartialResults
    let p = Processing {
            parent      = root
          , currentFold = topLevelFold
          , partialResults
          }
    return $ Bottom p

push ::
     CXCursor
  -> Fold m b
  -> ([b] -> m (Maybe a))
  -> Stack m a -> IO (Stack m b)
push newParent fold collect stack = do
    partialResults <- newPartialResults
    let p = Processing {
            parent      = newParent
          , currentFold = fold
          , partialResults
          }
    return $ Push p collect stack

popUntil :: forall m.
     (forall b. m b -> IO b)
  -> IORef (SomeStack m)
  -> CXCursor -> IO ()
popUntil runInIO someStack newParent = do
    SomeStack stack <- liftIO $ readIORef someStack
    stack' <- loop stack
    liftIO $ writeIORef someStack stack'
  where
    loop :: Stack m a -> IO (SomeStack m)
    loop stack = do
        arrived <- clang_equalCursors (topParent stack) newParent
        if arrived then
          return $ SomeStack stack
        else
          case stack of
            Bottom _ ->
              error "popUntil: something has gone horribly wrong"
            Push p collect stack' -> do
              as <- getPartialResults (partialResults p)
              mb <- runInIO $ collect as
              forM_ mb $ addPartialResult (topResults stack')
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
--
-- See also 'clang_getTranslationUnitCursor'.
clang_visitChildren :: forall m a.
     MonadUnliftIO m
  => CXCursor -> Fold m a -> m [a]
clang_visitChildren root topLevelFold = withRunInIO $ \runInIO -> do
    stack     <- initStack root topLevelFold
    someStack <- newIORef $ SomeStack stack
    _terminatedEarly <- Core.clang_visitChildren root $ visitor runInIO someStack
    popUntil runInIO someStack root
    getPartialResults (topResults stack)
  where
    visitor ::
         (forall b. m b -> IO b)
      -> IORef (SomeStack m)
      -> CXCursor
      -> CXCursor
      -> IO (SimpleEnum CXChildVisitResult)
    visitor runInIO someStack current parent = do
        popUntil runInIO someStack parent
        SomeStack stack <- readIORef someStack
        let p = topProcessing stack
        next <- runInIO $ runFold (currentFold p) current
        case next of
          Break ma -> do
            forM_ ma $ addPartialResult (partialResults p)
            return $ simpleEnum CXChildVisit_Break
          Continue ma -> do
            forM_ ma $ addPartialResult (partialResults p)
            return $ simpleEnum CXChildVisit_Continue
          Recurse fold collect -> do
            stack' <- push current fold collect stack
            writeIORef someStack $ SomeStack stack'
            return $ simpleEnum CXChildVisit_Recurse
