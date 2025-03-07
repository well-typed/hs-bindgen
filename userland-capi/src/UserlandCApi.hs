module UserlandCApi where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.IORef
import Data.Char
import Control.Monad

data S = S
    { lname :: Name
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
    return $ case lname s of
        Name _ (NameG _ (PkgName p) (ModName m)) -> filter isLetter (p++m)++"_"++n++"_wrapper"
        _ -> n ++ "_wrapper"

addC :: String -> UC ()
addC c = UC $ \ref -> do
    s <- readIORef ref
    writeIORef ref s { csource = csource s ++ [c] }

addDec :: DecQ -> UC ()
addDec d = UC $ \ref -> do
    s <- readIORef ref
    writeIORef ref s { decs = decs s ++ [d] }

runUC :: Name -> UC () -> IO S
runUC n m = do
    ref <- newIORef (S n [] [] [])
    unUC m ref
    readIORef ref

foreignImport :: String -> String -> TypeQ -> DecQ
foreignImport cname hsname ty = ForeignD . ImportF CCall Safe cname (mkName hsname) <$> ty

userlandCApi
    :: Name -- ^ This name is used to create unique identifiers, based on unit-id and current module.
    -> UC ()
    -> DecsQ
userlandCApi localname uc = do
    runIO $ putStrLn "hello at compile time"
    s <- runIO (runUC localname uc)
    let source' = source s
    runIO $ print localname
    runIO $ putStrLn source'
    addForeignSource LangC source'
    sequence (decs s)
  where
    source s = unlines $
        [ "#include " ++ l
        | l <- includes s
        ] ++ csource s
