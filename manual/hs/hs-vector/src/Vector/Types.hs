{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Vector.Types (
    Length(UnsafeWrap, Length)
  ) where

import GHC.Show

import HsBindgen.Runtime.Internal.Prelude qualified as RIP

-- | Vector length
--
-- Invariant: must be non-negative.
newtype Length = UnsafeWrap { unwrap :: Double }
  deriving stock (Eq, Ord)
  deriving newtype (RIP.HasFFIType)

instance Show Length where
  showsPrec p (UnsafeWrap x) = showParen (p >= appPrec1) $
        showString "Length " -- use the pattern synonym
      . showsPrec appPrec1 x

pattern Length :: Double -> Length
pattern Length x <- (unwrap -> x)
  where
    Length x
      | x < 0     = error "Length must be non-negative"
      | otherwise = UnsafeWrap x
