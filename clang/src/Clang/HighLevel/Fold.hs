-- | Higher-level bindings for traversing the API
--
-- Intended for unqualified import.
module Clang.HighLevel.Fold (
    -- * Folds
    Fold -- opaque
  , Next -- opaque
    -- * Construction
  , simpleFold
  , foldWithHandler
  , runFold
    -- * Fold-specific operations
  , foldBreak
  , foldBreakWith
  , foldBreakOpt
  , foldContinue
  , foldContinueWith
  , foldContinueOpt
  , foldRecurse
  , foldRecurseWith
  , foldRecurseOpt
  , foldRecursePure
  , foldRecursePureOpt
    -- * Execution
  , clang_visitChildren
  ) where

import Control.Exception (SomeException, Exception(..))
import Control.Exception qualified as Base
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.IORef
import GHC.Stack

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
      foldNext    :: CXCursor -> m (Next m a)
    , foldHandler :: CXCursor -> SomeException -> m (Maybe a)
    }

-- | Construct simple fold
--
-- See also 'foldWithHandler'.
simpleFold :: forall m a. MonadIO m => (CXCursor -> m (Next m a)) -> Fold m a
simpleFold foldNext =
    Fold{foldNext, foldHandler}
  where
    foldHandler :: CXCursor -> SomeException -> m (Maybe a)
    foldHandler _curr = liftIO . throwIO

-- | Fold with exception handler
--
-- The exception handler is provided a 'CXCursor' pointing to the location in
-- the AST where the exception was caught.
--
-- == Exception handling in folds
--
-- Suppose we have a C file that looks like this:
--
-- > struct foo {
-- >   int a;
-- >   int b;
-- > };
-- >
-- > struct bar {
-- >   int c;
-- >   int d;
-- > };
-- >
-- > struct baz {
-- >   int e;
-- >   int f;
-- > };
--
-- and suppose we want to define a fold that extracts all struct field names,
-- so that we end up with
--
-- > [["a", "b"], ["c", "d"], ["e", "f"]]
--
-- We can write such a fold as follows:
--
-- > foldStruct :: Fold IO [Text]
-- > foldStruct = simpleFold $ \curr -> do
-- >     kind <- fromSimpleEnum <$> clang_getCursorKind curr
-- >     case kind of
-- >       Right CXCursor_StructDecl ->
-- >         foldRecursePure foldField concat
-- >       _otherwise ->
-- >         error $ "unexpected: " ++ show kind
-- >
-- > foldField :: Fold IO [Text]
-- > foldField = simpleFold $ \curr -> do
-- >     kind <- fromSimpleEnum <$> clang_getCursorKind curr
-- >     case kind of
-- >       Right CXCursor_FieldDecl -> do
-- >         name <- clang_getCursorSpelling curr
-- >         foldContinueWith [name]
-- >       _otherwise ->
-- >         error $ "unexpected: " ++ show kind
--
-- Let's consider what happens if 'foldField' throws an exception; for example,
-- perhaps it throws an exception when it sees the name \"c\":
--
-- > data UnexpectedField = UnexpectedField
-- >
-- > foldField :: Fold IO [Text]
-- > foldField = simpleFold $ \curr -> do
-- >     kind <- fromSimpleEnum <$> clang_getCursorKind curr
-- >     case kind of
-- >       Right CXCursor_FieldDecl -> do
-- >         name <- clang_getCursorSpelling curr
-- >         if name == "c"
-- >           then throwIO UnexpectedField
-- >           else foldContinueWith [name]
-- >       _otherwise ->
-- >         error $ "unexpected: " ++ show kind
--
-- Perhaps we want to try and recover from this in 'foldStruct', returning the
-- empty list of fields for that particular struct:
--
-- > hasUnexpectedField :: UnexpectedField -> [Text]
-- > hasUnexpectedField UnexpectedField = []
-- >
-- > foldStruct :: Fold IO [Text]
-- > foldStruct = simpleFold $ \curr ->
-- >     handle (foldContinueWith . hasUnexpectedField) $ do
-- >       -- .. body as before
--
-- Unfortunately, this will not work (that is, the handler will not catch the
-- exception). The problem is that 'foldStruct' does not /really/ recurse to
-- parse the fields: instead, it /returns/ a value which indicates that we are
-- interested in the children of the node, along with a function that processes
-- the results of parsing those child nodes (see 'foldRecurseWith').
--
-- Instead of using a local exception handler, you must therefore use
-- 'foldWithHandler':
--
-- > foldStruct :: Fold IO [Text]
-- > foldStruct =
-- >     foldWithHandler (\_curr -> return . Just . hasUnexpectedField) $ \curr -> do
-- >       -- .. body as before
--
-- With this handler in place, we will get the expected result
--
-- > [["a", "b"], [], ["e", "f"]]
--
-- Put another way, 'foldWithHandler' can be used to install an exception handler
-- which behaves /as if/ the fold truly made recursive calls.
foldWithHandler :: forall m e a.
     (MonadIO m, Exception e)
  => (CXCursor -> e -> m (Maybe a)) -- ^ Exception handler
  -> (CXCursor -> m (Next m a))
  -> Fold m a
foldWithHandler handler foldNext =
    Fold{foldNext, foldHandler}
  where
    foldHandler :: CXCursor -> SomeException -> m (Maybe a)
    foldHandler curr e =
        case fromException e of
          Just e' -> handler curr e'
          Nothing -> liftIO $ throwIO e

-- | Run 'Fold'
--
-- If the fold has an associated exception handler, it will get a chance to
-- catch any exceptions thrown. See 'foldWithHandler' for detailed discussion.
runFold :: MonadUnliftIO m => Fold m a -> CXCursor -> m (Next m a)
runFold fold curr = withRunInIO $ \runInIO -> runFoldUsing runInIO fold curr

-- | Internal generalization of 'runFold'
runFoldUsing ::
     ( MonadIO n
     , Functor m
     )
  => RunInIO m -> Fold m a -> CXCursor -> n (Next m a)
runFoldUsing runInIO Fold{foldNext, foldHandler} curr =
    handleUnliftUsing runInIO (fmap Continue . foldHandler curr) $
      foldNext curr

-- | Result of visiting one node
--
-- This is the equivalent of 'CXChildVisitResult'
data Next m a where
  Break    :: Maybe a -> Next m a
  Continue :: Maybe a -> Next m a
  Recurse  :: Fold m b -> ([b] -> m (Maybe a)) -> Next m a

{-------------------------------------------------------------------------------
  Constructing 'Next' ('Next' itself is intentionally opaque)
-------------------------------------------------------------------------------}

-- | Stop folding early, without a result
--
-- See also 'foldBreakWith' and 'foldBreakOpt'.
--
-- NOTE: \"break\" means that the fold is terminated entirely; it does /not/
-- mean \"break to the immediate parent\".
--
-- This is the equivalent of 'CXChildVisit_Break'.
foldBreak :: Monad m => m (Next m a)
foldBreak = foldBreakOpt Nothing

-- | Like 'foldBreak', but producing a result
foldBreakWith :: Monad m => a -> m (Next m a)
foldBreakWith = foldBreakOpt . Just

-- | Generalization of 'foldBreak' and 'foldBreakWith' with optional result
foldBreakOpt :: Monad m => Maybe a -> m (Next m a)
foldBreakOpt = pure . Break

-- | Continue with the next sibling of the current node, without a result
--
-- See also 'foldContinueWith' and 'foldContinueOpt'.
--
-- This is the equivalent of 'CXChildVisit_Continue'.
foldContinue :: Monad m => m (Next m a)
foldContinue = foldContinueOpt Nothing

-- | Like 'foldContinue', but producing a result
foldContinueWith :: Monad m => a -> m (Next m a)
foldContinueWith = foldContinueOpt . Just

-- | Generalization of 'foldContinue' and 'foldContinueWith' with optional result
foldContinueOpt :: Monad m => Maybe a -> m (Next m a)
foldContinueOpt = pure . Continue

-- | Recurse into the children of the current node, without producing a result
--
-- Unlike in the low-level @clang@ interface, each time we recurse we can
-- use a /different/ fold.
--
-- See also 'foldRecurseWith' if you want the results of the recursion.
--
-- This is the equivalent of 'CXChildVisit_Recurse'.
foldRecurse :: Monad m => Fold m () -> m (Next m a)
foldRecurse fold = foldRecursePureOpt fold (const Nothing)

-- | Like 'foldRecurse', but producing a result
--
-- In this case, we must provide a @summarize@ function which turns the results
-- obtained from processing the children into a result for the parent.
foldRecurseWith :: Monad m => Fold m b -> ([b] -> m a) -> m (Next m a)
foldRecurseWith fold summarize = foldRecurseOpt fold (fmap Just . summarize)

-- | Generalization of 'foldRecurseWith' with an /optional/ result
foldRecurseOpt :: Monad m => Fold m b -> ([b] -> m (Maybe a)) -> m (Next m a)
foldRecurseOpt fold summarize = pure $ Recurse fold summarize

-- | Pure variant on 'foldRecurseWith'
foldRecursePure :: Monad m => Fold m b -> ([b] -> a) -> m (Next m a)
foldRecursePure fold summarize = foldRecurseWith fold (pure . summarize)

-- | Pure variant on 'foldRecurseOpt'
foldRecursePureOpt :: Monad m => Fold m b -> ([b] -> Maybe a) -> m (Next m a)
foldRecursePureOpt fold summarize = foldRecurseOpt fold (pure . summarize)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

instance Functor m => Functor (Next m) where
  fmap f (Break x)     = Break (fmap f x)
  fmap f (Continue x)  = Continue (fmap f x)
  fmap f (Recurse r g) = Recurse r (fmap (fmap f) . g)

instance Functor m => Functor (Fold m) where
  fmap f Fold{foldNext, foldHandler} = Fold{
        foldNext    = \curr -> fmap (fmap f) $ foldNext curr
      , foldHandler = \curr -> fmap (fmap f) . foldHandler curr
      }

{-------------------------------------------------------------------------------
  Internal: exception handling

  We do not use @handle@ from @unlift@, as it excludes async exceptions, and we
  want it to be up to the exception handler to decide if it wants to deal with
  async exceptions or not.
-------------------------------------------------------------------------------}

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Base.throwIO

type RunInIO m = forall a. m a -> IO a

handleUnliftUsing ::
     ( MonadIO n
     , Exception e
     )
  => RunInIO m -> (e -> m a) -> m a -> n a
handleUnliftUsing runInIO handler action = liftIO $
    Base.handle (runInIO . handler) (runInIO action)

{-------------------------------------------------------------------------------
  Internal: partial results

  NOTE: These functions are ultimately called from a Haskel callback called from
  the clang C library. They do not need to be (nor are) thread safe.
-------------------------------------------------------------------------------}

type PartialResults a = IORef (Either SomeException [a])

newPartialResults :: IO (PartialResults a)
newPartialResults = newIORef (Right [])

addPartialResult :: HasCallStack => PartialResults a -> a -> IO ()
addPartialResult ref x = do
    mResults <- readIORef ref
    case mResults of
      Left  oldErr -> unexpectedException oldErr
      Right xs     -> writeIORef ref $ Right (x:xs)

recordException :: HasCallStack => PartialResults a -> SomeException -> IO ()
recordException ref newErr = do
    mResults <- readIORef ref
    case mResults of
      Left  oldErr -> unexpectedException oldErr
      Right _xs    -> writeIORef ref $ Left newErr

-- | Existing exceptions are impossible
--
-- As soon as any partial result throws an exception, we skip over any of the
-- remaining children, and immediately to the @summarize@ function of the
-- parent. This means that an existing exception should be impossible.
--
-- The use of @error@ here is an exception in its own right, which we don't
-- propagate (it will be caught in the low-level 'Core.clang_visitChildren'
-- function). However, that's okay: if this @error@ ever triggers, it indicates
-- a bug in this infrastructure, which can't really be handlded anyway.
unexpectedException :: HasCallStack => SomeException -> IO a
unexpectedException oldErr = error $ concat [
      "The impossible happened: we break at the first exception, "
    , "yet here we are: " ++ show oldErr ++ ".\n"
    , prettyCallStack callStack
    ]

getPartialResults :: MonadIO m => PartialResults a -> m [a]
getPartialResults ref = liftIO $ do
    mResults <- readIORef ref
    case mResults of
      Left  e  -> throwIO e
      Right xs -> return (reverse xs)

partialResultsIsException :: PartialResults a -> IO Bool
partialResultsIsException ref = do
    mResults <- readIORef ref
    case mResults of
      Left  _e  -> return True
      Right _xs -> return False

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

topFold :: Stack m a -> Fold m a
topFold = currentFold . topProcessing

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
push newParent fold summarize stack = do
    partialResults <- newPartialResults
    let p = Processing {
            parent      = newParent
          , currentFold = fold
          , partialResults
          }
    return $ Push p summarize stack

popUntil :: forall m.
     MonadIO m
  => RunInIO m
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
            Push p summarize (stack' :: Stack m b) -> do
              let handler :: SomeException -> m (Maybe b)
                  handler = foldHandler (topFold stack') (parent p)
              mb <- Base.try $
                handleUnliftUsing runInIO handler $
                  summarize =<< getPartialResults (partialResults p)
              case mb of
                Right Nothing  -> return ()
                Right (Just b) -> addPartialResult (topResults stack') b
                Left  ex       -> recordException  (topResults stack') ex
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
         RunInIO m
      -> IORef (SomeStack m)
      -> CXCursor
      -> CXCursor
      -> IO (SimpleEnum CXChildVisitResult)
    visitor runInIO someStack current parent = do
        popUntil runInIO someStack parent
        SomeStack stack <- readIORef someStack
        let p = topProcessing stack
        previousException <- partialResultsIsException (partialResults p)

        if previousException then
          return $ simpleEnum CXChildVisit_Continue
        else do
          next <- Base.try $ runFoldUsing runInIO (currentFold p) current
          case next of
            Right (Break ma) -> do
              forM_ ma $ addPartialResult (partialResults p)
              return $ simpleEnum CXChildVisit_Break
            Right (Continue ma) -> do
              forM_ ma $ addPartialResult (partialResults p)
              return $ simpleEnum CXChildVisit_Continue
            Right (Recurse fold summarize) -> do
              stack' <- push current fold summarize stack
              writeIORef someStack $ SomeStack stack'
              return $ simpleEnum CXChildVisit_Recurse
            Left ex -> do
              recordException (partialResults p) ex
              return $ simpleEnum CXChildVisit_Continue
