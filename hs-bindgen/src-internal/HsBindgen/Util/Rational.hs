module HsBindgen.Util.Rational (
    canBeRepresentedAsRational
  ) where

{-# SPECIALISE canBeRepresentedAsRational :: Float  -> Bool #-}
{-# SPECIALISE canBeRepresentedAsRational :: Double -> Bool #-}

-- | Can this floating-point value be represented (losslessly) as a 'Rational'?
canBeRepresentedAsRational :: RealFloat a => a -> Bool
canBeRepresentedAsRational f = not $ or
  [ isNaN f
  , isInfinite f
  , isNegativeZero f
  , isDenormalized f -- not strictly necessary, but let's be conservative
  ]
