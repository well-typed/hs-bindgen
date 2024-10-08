{-# LANGUAGE Arrows #-}

-- | Logging
--
-- Indended for unqualified import.
module HsBindgen.Util.Tracer (
    Tracer -- opaque
  , nullTracer
  , PrettyLogMsg(..)
    -- * Using the tracer
  , Level(..)
  , traceWith
  , contramap
    -- * Constructing tracers
  , mkTracer
  , mkTracerIO
  , mkTracerQ
  , traceThrow
  ) where

import Control.Tracer qualified as Contra
import Control.Tracer.Arrow
import Data.Bifunctor
import Data.Functor.Contravariant

import Language.Haskell.TH.Syntax
import Control.Exception

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

newtype Tracer m a = Wrap {
      unwrap :: Contra.Tracer m (Level, a)
    }

instance Monad m => Contravariant (Tracer m) where
  contramap f = Wrap . contramap (second f) . unwrap

data Level = Error | Warning | Info

traceWith :: Monad m => Tracer m a -> Level -> a -> m ()
traceWith t l a = Contra.traceWith (unwrap t) (l, a)

{-------------------------------------------------------------------------------
  Internal: general tracer construction
-------------------------------------------------------------------------------}

nullTracer :: Monad m => Tracer m a
nullTracer = Wrap Contra.nullTracer

mkTracer :: forall m a.
     Monad m
  => (a -> m ())  -- ^ Output error
  -> (a -> m ())  -- ^ Output warning
  -> (a -> m ())  -- ^ Output info
  -> Bool         -- ^ Enable 'Info' (verbose mode)
  -> Tracer m a
mkTracer outputError outputWarning outputInfo verbose =
    Wrap $ Contra.arrow aux
  where
    aux :: TracerA m (Level, a) ()
    aux = proc (level, msg) -> do
        case level of
          Error   -> emit outputError   -< msg
          Warning -> emit outputWarning -< msg
          Info    -> if verbose
                       then emit outputInfo -< msg
                       else squelch         -< msg

{-------------------------------------------------------------------------------
  Specific tracers
-------------------------------------------------------------------------------}

-- | Standard tracer for use in the 'IO' monad
--
-- Intended for use preprocessor mode.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/84>
-- We should support some ANSI colors here.
mkTracerIO ::
     Bool -- ^ Verbose
  -> Tracer IO String
mkTracerIO =
    mkTracer
      (putStrLn . ("Error: "   ++))
      (putStrLn . ("Warning: " ++))
      (putStrLn . ("Info: "    ++))

-- | Tracer intended for use in TH mode
mkTracerQ ::
     Quasi m
  => Bool -- ^ Verbose
  -> Tracer m String
mkTracerQ  =
    mkTracer
      (qReport True)
      (qReport False)
      (qReport False . ("Info: " ++))

-- | Throw any messages that aren't suppressed by the verbosity level
traceThrow ::
     Exception a
  => Bool  -- ^ Verbose
  -> Tracer IO a
traceThrow =
    mkTracer
      throwIO
      throwIO
      throwIO

{-------------------------------------------------------------------------------
  Type class intended for rendering log messages
-------------------------------------------------------------------------------}

class PrettyLogMsg a where
  prettyLogMsg :: a -> String
