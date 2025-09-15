-- | Predicates on trace messages
--
-- Intended for unqualified import.
module Test.Common.HsBindgen.TracePredicate (
    -- * Predicate
    TraceExpectation (..)
  , TracePredicate -- opaque
  , WrongCountMsg(..)
  , Labelled(..)
  , defaultTracePredicate
  , singleTracePredicate
  , customTracePredicate
  , customTracePredicate'
  , TraceExpectationException
    -- * Tracer
  , withTracePredicate
  , withTraceConfigPredicate
  ) where

import Control.Exception (Exception, finally, throwIO)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Foldable qualified as Foldable
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Test.Common.HsBindgen.Trace
import Text.SimplePrettyPrint (CtxDoc)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports (Default (def))
import HsBindgen.Util.Tracer

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
defaultTracePredicate :: (IsTrace Level a, Show a)
  => TracePredicate a
defaultTracePredicate = customTracePredicate [] (const Nothing)

-- | 'Expect' a trace with given name exactly one time.
singleTracePredicate :: (IsTrace Level a, Show a)
  => (a -> Maybe (TraceExpectation ()))
  -> TracePredicate a
singleTracePredicate predicate = customTracePredicate' [()] predicate

customTracePredicate :: (IsTrace Level a, Show a)
  => [String]
  -- ^ Names/identifiers of expected traces. If a trace is expected N times, add
  -- the name/identifier N times to the list.
  -> (a -> Maybe (TraceExpectation String))
  -- ^ 'Nothing' defaults to 'defaultTracePredicate'.
  -> TracePredicate a
customTracePredicate = customTracePredicate'

customTracePredicate' :: forall a b. (IsTrace Level a, Ord b, WrongCountMsg a b)
  => [b]
  -> (a -> Maybe (TraceExpectation b))
  -> TracePredicate a
customTracePredicate' names mpredicate = TracePredicate $ \traces -> do
  let (unexpectedTraces, actualCounts) =
        Foldable.foldl' checkTrace ([], Map.empty) traces
      checkTrace (ts, counts) trace = case predicate trace of
        Expected name -> (ts        , Map.insertWith (<>) name [trace] counts)
        Tolerated     -> (ts        , counts            )
        Unexpected    -> (trace : ts, counts            )
  if null unexpectedTraces && expectedCounts == Map.map length actualCounts
    then pure ()
    else
      let additionalCounts = actualCounts `Map.difference` expectedCounts
          additionalWrongCounts = [ wrongCount name 0 (length actual) actual
                                  | (name, actual) <- Map.toList additionalCounts
                                  ]
          wrongCounts = [ wrongCount name expected (length actual) actual
                        | (name, expected) <- Map.toList expectedCounts
                        , let actual = fromMaybe [] (name `Map.lookup` actualCounts)
                        , length actual /= expected
                        ]
          expectedTracesWithWrongCounts = wrongCounts ++ additionalWrongCounts
       in throwError $ TraceExpectationException {..}
  where
    defaultTracePredicateSimple :: a -> TraceExpectation b
    defaultTracePredicateSimple = \case
        Error        -> Unexpected
        Warning      -> Unexpected
        Notice       -> Unexpected
        Info         -> Tolerated
        Debug        -> Tolerated
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
withTracePredicate
  :: (IsTrace Level a , Typeable a, Show a)
  => TracePredicate a -> (Tracer IO a -> IO b) -> IO b
withTracePredicate predicate action = fmap fst $
  withTraceConfigPredicate predicate $ \traceConfig ->
    withTracer' traceConfig action

-- | Run an action with a tracer configuration that collects all trace messages.
--
-- Use a 'Predicate' to decide whether traces are expected, or unexpected.
withTraceConfigPredicate
  :: forall a b. (IsTrace Level a , Typeable a, Show a)
  => TracePredicate a -> (TracerConfig IO Level a -> IO b) -> IO b
withTraceConfigPredicate (TracePredicate predicate) action = do
  tracesRef <- newIORef []
  let writer :: Report IO a
      writer _ trace _ = modifyIORef' tracesRef ((:) trace)
  (action $ def {
      tVerbosity    = Verbosity Info
    , tOutputConfig = OutputCustom writer DisableAnsiColor
    }) `finally` do
      traces <- readIORef tracesRef
      case runExcept (predicate traces) of
        Left  e -> throwIO e
        Right _ -> pure ()

{-------------------------------------------------------------------------------
  Trace exception
-------------------------------------------------------------------------------}

data TraceExpectationException a = TraceExpectationException {
      unexpectedTraces              :: [a]
    , expectedTracesWithWrongCounts :: [CtxDoc]
    }

instance (IsTrace l a, Show a) => Show (TraceExpectationException a) where
  show (TraceExpectationException {..}) = PP.renderCtxDoc PP.defaultContext $
      PP.vcat $
           ( if null unexpectedTraces
               then []
               else "Unexpected traces:"
                  : map reportTrace unexpectedTraces
           )
        ++ ( if null expectedTracesWithWrongCounts
               then []
               else "Expected traces with wrong counts:"
                  : expectedTracesWithWrongCounts
           )


instance (Typeable a, IsTrace l a, Show l, Show a)
  => Exception (TraceExpectationException a)

{-------------------------------------------------------------------------------
  Wrong counts
-------------------------------------------------------------------------------}

class WrongCountMsg a b where
  wrongCount ::
       b    -- ^ Name
    -> Int  -- ^ Expected count
    -> Int  -- ^ Actual count
    -> [a]  -- ^ List of traces
    -> CtxDoc

-- | The general case, with user-defined labels as documents
instance (IsTrace l a, Show a) => WrongCountMsg a CtxDoc where
  wrongCount name expectedCount actualCount traces =
    PP.hangs' intro 2 $ map reportTrace traces
    where
      intro = PP.hcat
        [ "Name: ",             name
        , ", expected count: ", PP.showToCtxDoc expectedCount
        , ", actual count: "  , PP.showToCtxDoc actualCount
        ]

-- | Traces with multiple outcome, with user-defined labels
instance (IsTrace l a, Show a) => WrongCountMsg a String where
  wrongCount = wrongCount . PP.string

-- | It is often useful to check for warnings/errors for specific declarations
instance (IsTrace l a, Show a) => WrongCountMsg a C.PrelimDeclId where
  wrongCount = wrongCount . prettyForTrace

-- | The most common case: traces with just one outcome
instance (IsTrace l a, Show a) => WrongCountMsg a () where
  wrongCount _ 1 n _      = case compare n 1 of
    LT -> "Expected a single trace but no trace was emitted"
    EQ -> panicPure "error: received correct count"
    GT -> "Expected a single trace but more traces were emitted"
  wrongCount _ i j traces = wrongCount ("AnonymousTracePredicate" :: String) i j traces

-- | Distinguish cases based on test-specific label
data Labelled a = Labelled String a
  deriving stock (Show, Eq, Ord)

instance PrettyForTrace a => PrettyForTrace (Labelled a) where
  prettyForTrace (Labelled label x) = PP.hcat [
        PP.string label
      , ": "
      , prettyForTrace x
      ]

instance (IsTrace l a, PrettyForTrace b, Show a)
  => WrongCountMsg a (Labelled b) where
  wrongCount = wrongCount . prettyForTrace

{-------------------------------------------------------------------------------
  Counter
-------------------------------------------------------------------------------}

type Counter a = Map a Int

addN :: (Ord a) => Int -> Counter a -> a -> Counter a
addN n m k = Map.insertWith (const (+ n)) k n m

count :: (Foldable f, Ord a) => f a -> Counter a
count = Foldable.foldl' (addN 1) Map.empty
