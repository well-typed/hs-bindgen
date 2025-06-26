module HsBindgen.Util.Monad (
    mapMaybeM
  ) where

--------------------------------------------------------------------------------

mapMaybeM :: forall a b m. Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr aux (pure [])
  where
    aux :: a -> m [b] -> m [b]
    aux x doRest = f x >>= \case
      Just y  -> (y :) <$> doRest
      Nothing -> doRest
