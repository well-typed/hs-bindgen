-- | 'Flip' is useful when working with 'HsBindgen.Macro.Type.HasTypes'
module HsBindgen.Macro.Flip (
    Flip(..)
  , flipF
  , flipM
  ) where

newtype Flip f a l = Flip { unflip :: f l a }
  deriving stock (Eq, Show)

flipF :: (Flip f n a -> Flip f n b) -> f a n -> f b n
flipF f x =
  let (Flip x') = f (Flip x)
  in  x'

flipM :: Monad m => (Flip f n a -> m (Flip f n b)) -> f a n -> m (f b n)
flipM f x = do
  (Flip x') <- f (Flip x)
  pure x'
