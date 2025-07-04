-- | Predicates on trace messages
--
-- Intended for unqualified import.
module Test.Common.HsBindgen.TracePredicate (
    -- * Predicate
    TraceExpectation (..)
  , TracePredicate -- opaque
  , WrongCountMsg(..)
  , defaultTracePredicate
  , singleTracePredicate
  , customTracePredicate
  , customTracePredicate'
  , TraceExpectationException
    -- * Tracer
  , withTracePredicate
  ) where

import Control.Exception
import Control.Monad.Except (Except, throwError, runExcept)
import Control.Monad.IO.Class
import Data.Foldable qualified as Foldable
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import HsBindgen.Errors
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Lib
import Text.SimplePrettyPrint (CtxDoc, (><))
import Text.SimplePrettyPrint qualified as PP

{-------------------------------------------------------------------------------
  Trace predicates
-------------------------------------------------------------------------------}

data TraceExpectation b = Expected b | Tolerated | Unexpected
  deriving stock (Show, Eq, Ord)

newtype TracePredicate a = TracePredicate {
    _tracePredicate :: [a] -> Except (TraceExpectationException a) ()
  }

-- | By default, we do not expect any warnings, nor errors ('Unexpected'). Info
-- and debug messages are 'Tolerate'd.
defaultTracePredicate :: HasDefaultLogLevel a => TracePredicate a
defaultTracePredicate = customTracePredicate [] (const Nothing)

-- | 'Expect' a trace with given name exactly one time.
singleTracePredicate :: HasDefaultLogLevel a
  => (a -> Maybe (TraceExpectation ()))
  -> TracePredicate a
singleTracePredicate predicate = customTracePredicate' [()] predicate

customTracePredicate
  :: HasDefaultLogLevel a
  => [String]
  -- ^ Names/identifiers of expected traces. If a trace is expected N times, add
  -- the name/identifier N times to the list.
  -> (a -> Maybe (TraceExpectation String))
  -- ^ 'Nothing' defaults to 'defaultTracePredicate'.
  -> TracePredicate a
customTracePredicate = customTracePredicate'

customTracePredicate' :: forall a b. (HasDefaultLogLevel a, Ord b, WrongCountMsg b)
  => [b]
  -> (a -> Maybe (TraceExpectation b))
  -> TracePredicate a
customTracePredicate' names mpredicate = TracePredicate $ \traces -> do
  let (unexpectedTraces, actualCounts) =
        Foldable.foldl' checkTrace ([], Map.empty) traces
      checkTrace (ts, counts) trace = case predicate trace of
        Expected name -> (ts        , addN 1 counts name)
        Tolerated     -> (ts        , counts            )
        Unexpected    -> (trace : ts, counts            )
  if null unexpectedTraces && expectedCounts == actualCounts
    then pure ()
    else
      let additionalCounts = actualCounts `Map.difference` expectedCounts
          additionalWrongCounts = [ wrongCount name 0 actual
                                  | (name, actual) <- Map.toList additionalCounts
                                  ]
          wrongCounts = [ wrongCount name expected actual
                        | (name, expected) <- Map.toList expectedCounts
                        , let actual = fromMaybe 0 (name `Map.lookup` actualCounts)
                        , actual /= expected
                        ]
          expectedTracesWithWrongCounts = wrongCounts ++ additionalWrongCounts
       in throwError $ TraceExpectationException {..}
  where
    defaultTracePredicateSimple :: a -> TraceExpectation b
    defaultTracePredicateSimple = \case
        Error        -> Unexpected
        Warning      -> Unexpected
        _lowerLevels -> Tolerated
        . getDefaultLogLevel

    predicate :: a -> TraceExpectation b
    predicate trace = fromMaybe (defaultTracePredicateSimple $ trace) (mpredicate trace)

    expectedCounts :: Counter b
    expectedCounts = count names

{-------------------------------------------------------------------------------
  Tracer
-------------------------------------------------------------------------------}

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
withTracePredicate (TracePredicate predicate) action = do
  tracesRef <- liftIO $ newIORef []
  actionRes <- action $ mkWriterTracer tracesRef
  traces <- liftIO $ readIORef tracesRef
  case runExcept (predicate traces) of
    Left  e -> liftIO $ throwIO e
    Right _ -> pure actionRes

mkWriterTracer :: MonadIO m => IORef [a] -> Tracer m a
mkWriterTracer tracesRef = simpleTracer addTrace
  where addTrace trace = liftIO $ modifyIORef' tracesRef (\xs -> trace : xs)

{-------------------------------------------------------------------------------
  Trace exception
-------------------------------------------------------------------------------}

data TraceExpectationException a = TraceExpectationException {
      unexpectedTraces              :: [a]
    , expectedTracesWithWrongCounts :: [CtxDoc]
    }

instance (PrettyForTrace a, HasDefaultLogLevel a)
      => Show (TraceExpectationException a) where
  show (TraceExpectationException {..}) = PP.renderCtxDoc PP.defaultContext $
      PP.vcat $
           ( if null unexpectedTraces
               then []
               else "Unexpected traces:"
                  : map formatTrace unexpectedTraces
           )
        ++ ( if null expectedTracesWithWrongCounts
               then []
               else "Expected traces with wrong counts:"
                  : expectedTracesWithWrongCounts
           )
    where
      formatTrace trace =
        PP.hang
          (PP.showToCtxDoc (getDefaultLogLevel trace) >< ":")
          2
          (prettyForTrace trace)

instance (Typeable a, PrettyForTrace a, HasDefaultLogLevel a)
  => Exception (TraceExpectationException a)

{-------------------------------------------------------------------------------
  Wrong counts
-------------------------------------------------------------------------------}

class WrongCountMsg b where
  wrongCount :: b -> Int -> Int -> CtxDoc

-- | The general case, with user-defined labels as documents
instance WrongCountMsg CtxDoc where
  wrongCount name expectedCount actualCount = PP.cat
    [ "Name: ",             name
    , ", expected count: ", PP.showToCtxDoc expectedCount
    , ", actual count: "  , PP.showToCtxDoc actualCount
    ]

-- | Traces with multiple outcome, with user-defined labels
instance WrongCountMsg String where
  wrongCount = wrongCount . PP.string

-- | It is often useful to check for warnings/errors for specific declarations
instance WrongCountMsg DeclId where
  wrongCount = wrongCount . prettyForTrace

-- | The most common case: traces with just one outcome
instance WrongCountMsg () where
  wrongCount _ 1 n = case compare n 1 of
    LT -> "Expected a single trace but no trace was emitted"
    EQ -> panicPure "error: received correct count"
    GT -> "Expected a single trace but more traces were emitted"
  wrongCount _ i j = wrongCount ("AnonymousTracePredicate" :: String) i j

{-------------------------------------------------------------------------------
  Counter
-------------------------------------------------------------------------------}

type Counter a = Map a Int

addN :: (Ord a) => Int -> Counter a -> a -> Counter a
addN n m k = Map.insertWith (const (+ n)) k n m

count :: (Foldable f, Ord a) => f a -> Counter a
count = Foldable.foldl' (addN 1) Map.empty
