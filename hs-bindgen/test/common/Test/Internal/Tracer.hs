{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Test.Internal.Tracer
  ( -- * Predicate
    TraceExpectation (..)
  , TracePredicate
  , defaultTracePredicate
  , singleTracePredicate
  , customTracePredicate
  , TraceExpectationException
    -- * Tracer
  , withTracePredicate
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Dynamic (Typeable)
import Data.Foldable qualified as Foldable
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

import HsBindgen.Lib (HasDefaultLogLevel (getDefaultLogLevel), Level (..),
                      PrettyForTrace (prettyTrace), Tracer, simpleTracer)

data TraceExpectation = Expected String | Tolerated | Unexpected
  deriving stock (Show, Eq, Ord)

data TracePredicate a = TracePredicate {
    _tracePredicateFunction       :: (a -> TraceExpectation)
  , _tracePredicateExpectedCounts :: Counter String
  }

-- | By default, we do not expect any warnings, nor errors ('Unexpected'). Info
-- and debug messages are 'Tolerate'd.
defaultTracePredicate :: HasDefaultLogLevel a => TracePredicate a
defaultTracePredicate = TracePredicate predicate Map.empty
  where predicate = \case
          Error        -> Unexpected
          Warning      -> Unexpected
          _lowerLevels -> Tolerated
          . getDefaultLogLevel

-- | 'Expect' a trace with given name exactly one time.
singleTracePredicate :: HasDefaultLogLevel a
  => String -> (a -> Maybe TraceExpectation)
  -> TracePredicate a
singleTracePredicate name = customTracePredicate [name]

customTracePredicate :: HasDefaultLogLevel a
  => [String]
  -- ^ Names/identifiers of expected traces. If a trace is expected N times, add
  -- the name/identifier N times to the list.
  -> (a -> Maybe TraceExpectation)
  -- ^ 'Nothing' defaults to 'defaultTracePredicate'.
  -> TracePredicate a
customTracePredicate names mpredicate = TracePredicate predicate counter
  where (TracePredicate defaultPredicate defaultNames) = defaultTracePredicate
        predicate trace = fromMaybe (defaultPredicate $ trace) (mpredicate trace)
        counter = sumCounters (count names) defaultNames

data WrongCount = WrongCount {
    expectationName :: String
  , expectedCount   :: Int
  , actualCount     :: Int
  }

data TraceExpectationException a = TraceExpectationException {
      unexpectedTraces              :: [a]
    , expectedTracesWithWrongCounts :: [WrongCount]
    }

instance (PrettyForTrace a, HasDefaultLogLevel a)
  => Show (TraceExpectationException a) where
  show (TraceExpectationException {..}) = unlines $
       (if null unexpectedTraces then []
          else "Unexpected traces:" : map formatTrace unexpectedTraces)
    ++ (if null expectedTracesWithWrongCounts then []
          else "Expected traces with wrong counts:" :
               map formatWrongCount expectedTracesWithWrongCounts)
    where formatTrace trace =
            show (getDefaultLogLevel trace) <> ": " <> prettyTrace trace
          formatWrongCount WrongCount {..} = concat
            [ "Name: ", expectationName
            , ", expected count: ", show expectedCount
            , ", actual count: "  , show actualCount
            ]

instance (Typeable a, PrettyForTrace a, HasDefaultLogLevel a)
  => Exception (TraceExpectationException a)

checkTracePredicate :: MonadError (TraceExpectationException a) m
  => TracePredicate a -> [a] -> m ()
checkTracePredicate (TracePredicate predicate expectedCounts) traces =
  if null unexpectedTraces && expectedCounts == actualCounts
    then pure ()
    else
      let additionalCounts = actualCounts `Map.difference` expectedCounts
          additionalWrongCounts = [ WrongCount name 0 actual
                                  | (name, actual) <- Map.toList additionalCounts
                                  ]
          wrongCounts = [ WrongCount name expected actual
                        | (name, expected) <- Map.toList expectedCounts
                        , let actual = fromMaybe 0 (name `Map.lookup` actualCounts)
                        , actual /= expected
                        ]
          expectedTracesWithWrongCounts = wrongCounts ++ additionalWrongCounts
       in throwError $ TraceExpectationException {..}
  where
    (unexpectedTraces, actualCounts) = Foldable.foldl' checkTrace ([], Map.empty) traces
    checkTrace (ts, counts) trace = case predicate trace of
      Expected name -> (ts        , addN 1 counts name)
      Tolerated     -> (ts        , counts            )
      Unexpected    -> (trace : ts, counts            )

-- | Run an action with a tracer that collects all trace messages.
--
-- Use a 'Predicate' to decide whether traces are expected, or unexpected.
--
-- NOTE: This would be nicer if we could use the 'Writer' monad, but we fix the
-- monad to 'IO' in many cases.
--
-- > mkWriterTracer :: MonadWriter [a] m => Tracer m a
-- > mkWriterTracer = Tracer $ emit (tell . singleton)
--
-- > withWriterTracer :: (MonadWriter [a] m1, Monad m2) => (Tracer m1 a -> m2 b) -> m2 (b, [a])
-- > withWriterTracer action = runWriterT (WriterT $ (, []) <$> (action mkWriterTracer))
withTracePredicate
  :: forall m a b. (MonadIO m, PrettyForTrace a, HasDefaultLogLevel a, Typeable a)
  => TracePredicate a -> (Tracer m a -> m b) -> m b
withTracePredicate predicate action = do
  tracesRef <- liftIO $ newIORef []
  actionRes <- action $ mkWriterTracer tracesRef
  traces <- liftIO $ readIORef tracesRef
  eitherError <- runExceptT (checkTracePredicate predicate traces)
  case eitherError of
    Left  e -> liftIO $ throwIO e
    Right _ -> pure actionRes

mkWriterTracer :: MonadIO m => IORef [a] -> Tracer m a
mkWriterTracer tracesRef = simpleTracer addTrace
  where addTrace trace = liftIO $ modifyIORef' tracesRef (\xs -> trace : xs)

{-------------------------------------------------------------------------------
  Aux.
-------------------------------------------------------------------------------}

type Counter a = Map a Int

addN :: (Ord a) => Int -> Counter a -> a -> Counter a
addN n m k = Map.insertWith (const (+ n)) k n m

count :: (Foldable f, Ord a) => f a -> Counter a
count = Foldable.foldl' (addN 1) Map.empty

sumCounters :: Ord a => Counter a -> Counter a -> Counter a
sumCounters left right = Foldable.foldl' mergeItem left (Map.toList right)
  where mergeItem counter (item, n) = addN n counter item
