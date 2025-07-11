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
  ) where

import Control.Exception
import Control.Monad.Except (Except, runExcept, throwError)
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
defaultTracePredicate
  :: (PrettyForTrace a, HasDefaultLogLevel a, Show a)
  => TracePredicate a
defaultTracePredicate = customTracePredicate [] (const Nothing)

-- | 'Expect' a trace with given name exactly one time.
singleTracePredicate :: (PrettyForTrace a, HasDefaultLogLevel a, Show a)
  => (a -> Maybe (TraceExpectation ()))
  -> TracePredicate a
singleTracePredicate predicate = customTracePredicate' [()] predicate

customTracePredicate
  :: (PrettyForTrace a, HasDefaultLogLevel a, Show a)
  => [String]
  -- ^ Names/identifiers of expected traces. If a trace is expected N times, add
  -- the name/identifier N times to the list.
  -> (a -> Maybe (TraceExpectation String))
  -- ^ 'Nothing' defaults to 'defaultTracePredicate'.
  -> TracePredicate a
customTracePredicate = customTracePredicate'

customTracePredicate'
  :: forall a b. (HasDefaultLogLevel a, Ord b, WrongCountMsg a b)
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
  :: forall m a b.
     (MonadIO m, PrettyForTrace a, HasDefaultLogLevel a, Typeable a, Show a)
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

instance (PrettyForTrace a, HasDefaultLogLevel a, Show a)
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
        PP.hangs'
          (PP.showToCtxDoc (getDefaultLogLevel trace) >< ":")
          2
          (prettyAndShowTrace trace)


instance (Typeable a, PrettyForTrace a, HasDefaultLogLevel a, Show a)
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
instance (PrettyForTrace a, Show a) => WrongCountMsg a CtxDoc where
  wrongCount name expectedCount actualCount traces =
    PP.hangs' intro 2 $ concatMap prettyAndShowTrace traces
    where
      intro = PP.hcat
        [ "Name: ",             name
        , ", expected count: ", PP.showToCtxDoc expectedCount
        , ", actual count: "  , PP.showToCtxDoc actualCount
        ]

-- | Traces with multiple outcome, with user-defined labels
instance (PrettyForTrace a, Show a) => WrongCountMsg a String where
  wrongCount = wrongCount . PP.string

-- | It is often useful to check for warnings/errors for specific declarations
instance (PrettyForTrace a, Show a) => WrongCountMsg a DeclId where
  wrongCount = wrongCount . prettyForTrace

-- | The most common case: traces with just one outcome
instance (PrettyForTrace a, Show a) => WrongCountMsg a () where
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

instance (PrettyForTrace a, PrettyForTrace b, Show a)
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

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- Seeing both, the pretty trace and the 'Show' instance greatly simplifies test
-- design and debugging.
prettyAndShowTrace :: (PrettyForTrace a, Show a) => a -> [CtxDoc]
prettyAndShowTrace trace =
          [ "prettyForTrace: " >< prettyForTrace trace
          , "show:           " >< PP.showToCtxDoc trace
          ]
