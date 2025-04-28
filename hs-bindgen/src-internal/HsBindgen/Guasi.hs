module HsBindgen.Guasi (
    Guasi (..),
) where

import Data.Char (toLower, isLetter)
import Language.Haskell.TH.Syntax qualified as TH

-- | An intermediate class between 'TH.Quote' and 'TH.Quasi'
-- which doesn't provide reification functionality of 'TH.Quasi',
-- but has a bit more than 'TH.Quote'.
class TH.Quote g => Guasi g where
    -- | Return a valid identifier string which uniqueily identifies the module.
    --
    -- The purpose is to generate unique names for C wrappers.
    getModuleUnique :: g String

    addDependentFile :: FilePath -> g ()
    extsEnabled :: g [TH.Extension]
    reportError :: String -> g ()
    -- TODO: addForeignSource :: TH.ForeignSourceLang -> String -> q ()

-- |
--
-- >>> :seti -XTemplateHaskell
-- >>> import Language.Haskell.TH.Syntax
-- >>> $(getModuleUnique >>= lift)
-- "interactive_Ghci"
--
instance Guasi TH.Q where
    getModuleUnique = do
        loc <- TH.location
        return $ mapHead toLower $ filter isLetter (TH.loc_package loc) ++ "_" ++ filter isLetter (TH.loc_module loc)

    addDependentFile = TH.addDependentFile
    extsEnabled = TH.extsEnabled
    reportError = TH.reportError

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs
