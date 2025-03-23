{-# LANGUAGE CPP #-}

module HsBindgen.Util.Compat (
    (!?)
  ) where

#if MIN_VERSION_base(4,19,0)
import Data.List ((!?))
#endif

#if !MIN_VERSION_base(4,19,0)
(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
#endif
