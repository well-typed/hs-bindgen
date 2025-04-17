module UserlandCApi where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.IORef
import Data.Char
import Control.Monad

data S = S
    { unique :: String -- ^ unique string identifying package&module
    , includes :: [String]
    , csource :: [String]
    , decs :: [DecQ]
    }

newtype UC a = UC { unUC :: IORef S -> IO a }

instance Functor UC where
    fmap = liftM
instance Applicative UC where
    pure x = UC (\_ -> return x)
    (<*>) = ap
instance Monad UC where
    return = pure
    m >>= k = UC $ \ref -> do
        x <- unUC m ref
        unUC (k x) ref

addInclude :: String -> UC ()
addInclude incl = UC $ \ref -> do
    s <- readIORef ref
    writeIORef ref s { includes = includes s ++ [incl] }

-- TODO: add counter
freshCName :: String -> UC String
freshCName n = UC $ \ref -> do
    s <- readIORef ref
    return $ unique s ++"_"++n++"_wrapper"

addC :: String -> UC ()
addC c = UC $ \ref -> do
    s <- readIORef ref
    writeIORef ref s { csource = csource s ++ [c] }

addDec :: DecQ -> UC ()
addDec d = UC $ \ref -> do
    s <- readIORef ref
    writeIORef ref s { decs = decs s ++ [d] }

runUC :: String -> UC () -> IO S
runUC n m = do
    ref <- newIORef (S n [] [] [])
    unUC m ref
    readIORef ref

foreignImport :: String -> String -> TypeQ -> DecQ
foreignImport cname hsname ty = ForeignD . ImportF CCall Safe cname (mkName hsname) <$> ty

userlandCApi
    :: UC ()
    -> DecsQ
userlandCApi uc = do
    loc <- location
    runIO $ putStrLn $ "hello at compile time: " ++ show loc
    let unitid' = filter isLetter (loc_package loc) ++ "_" ++ filter isLetter (loc_module loc)
    s <- runIO (runUC unitid' uc)
    let source' = source s
    runIO $ putStrLn source'
    addForeignSource LangC source'
    sequence (decs s)
  where
    source s = unlines $
        [ "#include " ++ l
        | l <- includes s
        ] ++ csource s
