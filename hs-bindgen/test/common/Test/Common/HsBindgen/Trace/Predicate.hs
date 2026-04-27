-- | Predicates on trace messages
--
-- Intended for unqualified import.
module Test.Common.HsBindgen.Trace.Predicate (
    -- * Predicate
    TraceExpectation (..)
  , TracePredicate -- opaque
  , RenderLabel(..)
  , GotExpectedTrace(..)
  , GotTraceLabelled(..)
  , defaultTracePredicate
  , tolerateAll
  , singleTracePredicate
  , multiTracePredicate
  , multiTracePredicateCustomLogLevel
  , TraceExpectationException
    -- * Tracer
  , quietTracerConfig
  , withTracePredicate
  , withTraceConfigPredicate
  ) where

import Control.Exception (Exception (..), catch, throwIO)
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Foldable qualified as Foldable
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (Typeable)
import Text.SimplePrettyPrint (CtxDoc)
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Clang
import HsBindgen.Errors
import HsBindgen.Frontend.Naming
import HsBindgen.Imports (Default (def))
import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.Trace

{-------------------------------------------------------------------------------
  Trace predicates
-------------------------------------------------------------------------------}

data TraceExpectation b = Expected b | Tolerated | Unexpected
  deriving stock (Show, Eq, Ord, Functor)

data TracePredicate l a = TracePredicate {
    _customLogLevel :: CustomLogLevel l a
  , _tracePredicate :: [a] -> Except (TraceExpectationException a) ()
  }

-- | By default, we do not expect any warnings, nor errors ('Unexpected'). Info
-- and debug messages are 'Tolerate'd.
defaultTracePredicate :: IsTrace Level a => TracePredicate Level a
defaultTracePredicate =
    customTracePredicateAux @GotExpectedTrace mempty [] (const Nothing)

-- | Tolerate /all/ traces
--
-- Useful for tests where we don't care about the trace messages, and want to
-- look at test output only.
tolerateAll :: IsTrace Level a => TracePredicate Level a
tolerateAll =
    customTracePredicateAux @GotExpectedTrace mempty [] (const $ Just Tolerated)

-- | Expect a trace with given name exactly one time.
singleTracePredicate ::
     IsTrace Level a
  => (a -> Maybe (TraceExpectation ()))
  -> TracePredicate Level a
singleTracePredicate predicate =
    customTracePredicateAux
      mempty
      [GotExpectedTrace]
      (fmap (fmap (\() -> GotExpectedTrace)) . predicate)

multiTracePredicate :: forall b a.
     (IsTrace Level a, Show a, Ord b, RenderLabel b)
  => [b]
  -- ^ Names/identifiers of expected traces. If a trace is expected N times, add
  -- the name/identifier N times to the list.
  -> (a -> Maybe (TraceExpectation b))
  -- ^ 'Nothing' defaults to 'defaultTracePredicate'.
  -> TracePredicate Level a
multiTracePredicate expected predicate =
    customTracePredicateAux
       mempty
       (map GotTraceLabelled expected)
       (fmap (fmap GotTraceLabelled) . predicate)

-- | Like 'multiTracePredicate' but allows for a custom log level.
multiTracePredicateCustomLogLevel :: forall b a.
     (IsTrace Level a, Show a, Ord b, RenderLabel b)
  => CustomLogLevel Level a
  -> [b]
  -> (a -> Maybe (TraceExpectation b))
  -> TracePredicate Level a
multiTracePredicateCustomLogLevel customLogLevel expected predicate =
    customTracePredicateAux
       customLogLevel
       (map GotTraceLabelled expected)
       (fmap (fmap GotTraceLabelled) . predicate)

-- | Internal generalization
customTracePredicateAux :: forall b a.
     (IsTrace Level a, Ord b, WrongCountMsg a b)
  => CustomLogLevel Level a
  -> [b]
  -> (a -> Maybe (TraceExpectation b))
  -> TracePredicate Level a
customTracePredicateAux customLogLevel names mpredicate =
    TracePredicate customLogLevel $ \traces -> do
      let (unexpected, actualCounts) =
            Foldable.foldl' checkTrace ([], Map.empty) traces
          checkTrace (ts, counts) trace = case predicate trace of
            Expected name -> (ts        , Map.insertWith (<>) name [trace] counts)
            Tolerated     -> (ts        , counts            )
            Unexpected    -> (trace : ts, counts            )
      if null unexpected && expectedCounts == Map.map length actualCounts
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
           in throwError $ TraceExpectationException {
                  unexpectedTraces = unexpected
                , wrongCounts      = wrongCounts ++ additionalWrongCounts
                }
  where
    getLogLevel :: a -> Level
    getLogLevel x = applyCustomLogLevel customLogLevel x (getDefaultLogLevel x)

    defaultTracePredicateSimple :: a -> TraceExpectation b
    defaultTracePredicateSimple x = case getLogLevel x of
        Error        -> Unexpected
        Bug          -> Unexpected
        Warning      -> Unexpected
        Notice       -> Tolerated
        Info         -> Tolerated
        Debug        -> Tolerated

    predicate :: a -> TraceExpectation b
    predicate trace = fromMaybe (defaultTracePredicateSimple $ trace) (mpredicate trace)

    expectedCounts :: Counter b
    expectedCounts = count names

{-------------------------------------------------------------------------------
  Tracer
-------------------------------------------------------------------------------}

quietTracerConfig :: TracerConfig l a
quietTracerConfig = def {
      outputConfig = OutputConfigCustom OutputCustom{
          report    = noOutput
        , ansiColor = DisableAnsiColor
        }
    }
  where
    noOutput _lvl _trace _traceStr = pure ()

-- | Run an action with a tracer that collects all trace messages.
--
-- Use a t'TracePredicate' to decide whether traces are expected, or unexpected.
withTracePredicate ::
     (Typeable a, IsTrace Level a, Show a)
  => (String -> IO ())
  -> TracePredicate Level a
  -> (Tracer a -> IO b)
  -> IO b
withTracePredicate report predicate action =
    withTraceConfigPredicate report predicate $ \traceConfig ->
      withTracerUnsafe traceConfig (\t _ -> action t)

-- | Run an action with a tracer configuration that collects all trace messages.
--
-- Use a t'TracePredicate' to decide whether traces are expected, or unexpected.
withTraceConfigPredicate ::
     forall a b l. (Typeable a, IsTrace l a, Show a)
  => (String -> IO ())
  -> TracePredicate l a
  -> (TracerConfig l a -> IO b)
  -> IO b
withTraceConfigPredicate report (TracePredicate customLogLevel predicate) action = do
    tracesRef <- newIORef []

    let writer :: Report a
        writer _ trace _ = modifyIORef' tracesRef ((:) trace)

        tracerConfig :: TracerConfig l a
        tracerConfig = def {
            customLogLevel = customLogLevel
          , verbosity      = Verbosity Info
          , outputConfig   = OutputConfigCustom OutputCustom{
                report    = writer
              , ansiColor = DisableAnsiColor
              }
          }

        reportTraces :: IO ()
        reportTraces = do
          traces <- readIORef tracesRef
          mapM_ (report . show . prettyForTrace) traces


        checkTraces :: IO ()
        checkTraces = do
          traces <- readIORef tracesRef
          case runExcept (predicate traces) of
            Left  e -> throwIO e
            Right _ -> pure ()

    (action tracerConfig <* reportTraces <* checkTraces) `catch` \e ->
      case fromException e of
        Just (_ :: LibclangException) -> reportTraces >> throwIO e
        _                             -> throwIO e

{-------------------------------------------------------------------------------
  Trace exception
-------------------------------------------------------------------------------}

data TraceExpectationException a = TraceExpectationException {
      unexpectedTraces :: [a]
    , wrongCounts      :: [CtxDoc]
    }

instance (IsTrace l a, Show a) => Show (TraceExpectationException a) where
  show e = show $
      PP.vcat $
           ( if null e.unexpectedTraces
               then []
               else "Unexpected traces:"
                  : map reportTrace e.unexpectedTraces
                  ++ ["\n"]
           )
        ++ ( if null e.wrongCounts
               then []
               else "Expected traces with wrong counts:"
                  : e.wrongCounts
                  ++ ["\n"]
           )


instance (Typeable a, IsTrace l a, Show a)
  => Exception (TraceExpectationException a)

{-------------------------------------------------------------------------------
  Wrong counts
-------------------------------------------------------------------------------}

-- | Emit message about unexpected traces
--
-- Given a particular type of trace message (say, @a = TraceMsg@), we typically
-- distill some info from this message (say, @b = C.DeclName@), and then compare
-- that info against a list of expected info. There are then two main cases
-- (that is, main choses for @b@):
--
-- * t'GotExpectedTrace': we either expect the trace message, or we don't; we
--   don't expect to get more than one.
-- * t'GotTraceLabelled': we expect multiple traces for this case, and label each
--   one. In this case, we show specific counts.
--
-- This is an internal class (not exported).
class WrongCountMsg a b where
  wrongCount ::
       b    -- ^ Name
    -> Int  -- ^ Expected count
    -> Int  -- ^ Actual count
    -> [a]  -- ^ List of traces
    -> CtxDoc

-- | See 'WrongCountMsg' for discussion
data GotExpectedTrace = GotExpectedTrace
  deriving stock (Show, Eq, Ord)

-- | See 'WrongCountMsg' for discussion
newtype GotTraceLabelled b = GotTraceLabelled b
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString)

-- | The most common case: traces with just one outcome
instance IsTrace l a => WrongCountMsg a GotExpectedTrace where
  wrongCount _ 1 n _ =
      case compare n 1 of
        LT -> "Expected a single trace but no trace was emitted"
        EQ -> panicPure "Received correct count"
        GT -> "Expected a single trace but more traces were emitted"
  wrongCount _ i _ _ =
      panicPure $ "Unexpected \"expected count\": " ++ show i

-- | The general case, with user-defined labels as documents
instance (IsTrace l a, Show a, RenderLabel b)
      => WrongCountMsg a (GotTraceLabelled b) where
  wrongCount (GotTraceLabelled label) expectedCount actualCount traces =
      PP.hangs' intro 2 $ map reportTrace traces
    where
      intro = PP.hcat
        [ "Name: ",             renderLabel label
        , ", expected count: ", PP.show expectedCount
        , ", actual count: "  , PP.show actualCount
        ]

{-------------------------------------------------------------------------------
  Auxiliary to 'GotTraceLabelled': render labels
-------------------------------------------------------------------------------}

class RenderLabel b where
  renderLabel :: b -> CtxDoc

  default renderLabel :: PrettyForTrace b => b -> CtxDoc
  renderLabel = prettyForTrace

instance RenderLabel CtxDoc where renderLabel = id
instance RenderLabel String where renderLabel = PP.string
instance RenderLabel Text   where renderLabel = PP.string . Text.unpack

instance RenderLabel CDeclName

instance RenderLabel a => RenderLabel (Maybe a) where
  renderLabel Nothing  = ""
  renderLabel (Just x) = renderLabel x

instance RenderLabel () where
  renderLabel () = "()"
instance ( RenderLabel a
         , RenderLabel b
         ) => RenderLabel (a, b) where
  renderLabel (x, y) = PP.parens $ PP.hlist "(" ")" [
        renderLabel x
      , renderLabel y
      ]
instance ( RenderLabel a
         , RenderLabel b
         , RenderLabel c
         ) => RenderLabel (a, b, c) where
  renderLabel (x, y, z) = PP.parens $ PP.hlist "(" ")" [
        renderLabel x
      , renderLabel y
      , renderLabel z
      ]

{-------------------------------------------------------------------------------
  Counter
-------------------------------------------------------------------------------}

type Counter a = Map a Int

addN :: (Ord a) => Int -> Counter a -> a -> Counter a
addN n m k = Map.insertWith (const (+ n)) k n m

count :: (Foldable f, Ord a) => f a -> Counter a
count = Foldable.foldl' (addN 1) Map.empty
