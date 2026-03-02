-- | Utilities for filesystem trees
--
-- Intended for unqualified import.
module Test.Common.Util.FSTree (
    -- * Shortcut
    ShortcutF (..)
  , Shortcut
    -- * FSTree
  , FSTreeF (..)
  , FSTreeSkeleton
  , FSTree
    -- * Item
  , Item (Item)
    -- * Comparison
  , cmpAnsiDiff_
  , cmpAnsiDiff
    -- * Merge
  , merges1
  , merge_
  , merge
    -- * Construction
  , fromFile
    -- * IO
  , listDirectoryRecursive
  , withFullPaths_
  , withFullPaths
  , readFiles_
  , readFiles
  , writeFiles_
  , writeFiles
  ) where

import Prelude hiding (readFile, writeFile)

import Control.Monad
import Control.Monad.Error.Class (MonadError (throwError))
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import Data.String
import System.Directory
import System.FilePath
import System.FilePath.Posix qualified
import System.FilePath.Windows qualified
import Text.Printf (PrintfArg, printf)

import HsBindgen.Errors (panicPure)

import Test.Common.Util.AnsiDiff (ansidiff)

{-------------------------------------------------------------------------------
  Shortcut
-------------------------------------------------------------------------------}

data ShortcutF n a = Shortcut {
    shortcut :: FilePath
  , tree :: FSTreeF n a
  }
  deriving stock (Show, Eq, Ord)
  deriving stock (Functor, Foldable, Traversable)

type ShortcutSkeleton = ShortcutF Item ()
type Shortcut a = ShortcutF Item a

{-------------------------------------------------------------------------------
  FSTree
-------------------------------------------------------------------------------}

data FSTreeF n a =
    Dir (Map n (FSTreeF n a))
  | File a
  deriving stock (Show, Eq, Ord)
  deriving stock (Functor, Foldable, Traversable)

type FSTreeSkeleton = FSTreeF Item ()
type FSTree a = FSTreeF Item a

{-------------------------------------------------------------------------------
  Item
-------------------------------------------------------------------------------}

-- | Name of a file or directory
newtype Item = UnsafeItem { un :: String }
  deriving stock (Eq, Ord)
  deriving newtype (Show)
  deriving newtype (PrintfArg, IsString)

{-# COMPLETE Item #-}
pattern Item :: String -> Item
pattern Item s <- UnsafeItem s
  where Item s = mkItem s

mkItem :: String -> Item
mkItem s
  | isValidItem s = UnsafeItem s
  | otherwise = panicPure $ printf "Invalid item name: %s" s

isValidItem :: String -> Bool
isValidItem s =
    and [ all isValidChar s
        , strLength >= 1
        , strLength <= 64
        , System.FilePath.Posix.isValid s
        , System.FilePath.Windows.isValid s
        ]
  where
    strLength :: Int
    strLength = length s

    isValidChar :: Char -> Bool
    isValidChar c =
        isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-_." :: String)

{-------------------------------------------------------------------------------
  Comparison
-------------------------------------------------------------------------------}

newtype CmpA a = CmpA { un :: Either String a}
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadError String CmpA

runCmpA :: CmpA a -> Maybe String
runCmpA action = case action.un of
    Left e -> Just e
    Right _ -> Nothing

cmpAnsiDiff_ ::
     Shortcut String
  -> Shortcut String
  -> Maybe String
cmpAnsiDiff_ (Shortcut p1 t1) (Shortcut p2 t2)
  | p1 /= p2
  = Just $ printf "Shortcut prefixes do not match: %s /= %s" p1 p2
  | otherwise
  = runCmpA $ cmpAnsiDiff' p1 t1 t2

cmpAnsiDiff :: FSTree String -> FSTree String -> Maybe String
cmpAnsiDiff t1 t2 = runCmpA $ cmpAnsiDiff' "" t1 t2

cmpAnsiDiff' :: FilePath -> FSTree String -> FSTree String -> CmpA (FSTree String)
cmpAnsiDiff' pathToHere = \t1 t2 ->
    case (t1, t2) of
      (Dir is, Dir js)
        -> Dir <$> Map.mergeA
            (Map.traverseMissing missingLeft)
            (Map.traverseMissing missingRight)
            (Map.zipWithAMatched matched)
            is js
      (File xs, File ys)
        | xs /= ys
        -> err $ printf "File contents do not match:\n%s" (ansidiff xs ys)
        | otherwise
        -> success (File xs)
      (Dir _, File _)
        -> err $ printf "Both a directory and a file."
      (File _, Dir _)
        -> err $ printf "Both a file and a directory."
  where
    success :: forall a. a -> CmpA a
    success = pure

    err :: String -> CmpA a
    err msg = throwError $ printf "Mismatch at path '%s'. %s" pathToHere msg

    missingLeft ::
          Item
      -> FSTreeF Item String
      -> CmpA (FSTreeF Item String)
    missingLeft i _t2 =
        err $ printf "Can not find entry '%s' in the left tree." (show i)

    missingRight ::
          Item
      -> FSTreeF Item String
      -> CmpA (FSTreeF Item String)
    missingRight i _t1 =
        err $ printf "Can not find entry '%s' in the right tree." (show i)

    matched ::
          Item
      -> FSTreeF Item String
      -> FSTreeF Item String
      -> CmpA (FSTreeF Item String)
    matched i t1 t2 = cmpAnsiDiff' (pathToHere </> i.un) t1 t2

{-------------------------------------------------------------------------------
  Merge
-------------------------------------------------------------------------------}

newtype MergeA a = MergeA { un :: Either String a}
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadError String MergeA

runMergeA :: MergeA a -> Either String a
runMergeA action = action.un

merges1 :: (Show a, Eq a) => [FSTree a] -> Either String (FSTree a)
merges1 [] = panicPure "merges1: no input"
merges1 (t:ts) = foldM merge t ts

merge_ ::
     (Show a, Eq a)
  => Shortcut a
  -> Shortcut a
  -> Either String (Shortcut a)
merge_ (Shortcut p1 t1) (Shortcut p2 t2)
  | p1 /= p2
  = Left $ printf "Shortcut prefixes do not match: %s /= %s" p1 p2
  | otherwise
  = Shortcut p1 <$> runMergeA (merge' p1 t1 t2)

merge :: forall a. (Show a, Eq a) => FSTree a -> FSTree a -> Either String (FSTree a)
merge = \t1 t2 -> runMergeA $ merge' "" t1 t2

merge' :: forall a. (Show a, Eq a) => FilePath -> FSTree a -> FSTree a -> MergeA (FSTree a)
merge' pathToHere = \t1 t2 ->
    case (t1, t2) of
      (Dir is, Dir js)
        -> Dir <$> Map.mergeA
            (Map.traverseMissing missingLeft)
            (Map.traverseMissing missingRight)
            (Map.zipWithAMatched matched)
            is js
      (File xs, File ys)
        | xs /= ys
        -> err $ printf "File contents do not match:\n %s"
                    (ansidiff (show xs) (show ys))
        | otherwise
        -> success (File xs)
      (Dir _, File _)
        -> err $ printf "Both a directory and a file"
      (File _, Dir _)
        -> err $ printf "Both a file and a directory"
  where
    success :: forall b. b -> MergeA b
    success = pure

    err :: forall b. String -> MergeA b
    err msg = throwError $ printf "Mismatch at %s. %s" pathToHere msg

    missingLeft ::
          Item
      -> FSTreeF Item a
      -> MergeA (FSTreeF Item a)
    missingLeft _i t2 = pure t2

    missingRight ::
          Item
      -> FSTreeF Item a
      -> MergeA (FSTreeF Item a)
    missingRight _ t1 = pure t1

    matched ::
          Item
      -> FSTreeF Item a
      -> FSTreeF Item a
      -> MergeA (FSTreeF Item a)
    matched i t1 t2 = merge' (pathToHere </> i.un) t1 t2

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

-- | Create an 'FSTree' from a 'FilePath'.
--
-- >>> fromFile "foo.txt" 1
-- Dir (fromList [("foo.txt",File 1)])
--
-- >>> fromFile "foo/bar.txt" 2
-- Dir (fromList [("foo",Dir (fromList [("bar.txt",File 2)]))])
--
-- >>> fromFile "/foo/bar/baz.txt" 3
-- PanicException [("panicPure",SrcLoc {srcLocPackage = "hs-bindgen-0.1.0-inplace-test-common", srcLocModule = "Test.Common.Util.FSTree", srcLocFile = "/home/joris/repos/github/hs-bindgen/hs-bindgen/test/common/Test/Common/Util/FSTree.hs", srcLocStartLine = 92, srcLocStartCol = 17, srcLocEndLine = 92, srcLocEndCol = 26})] "Invalid item name: /"
--
fromFile :: FilePath -> a -> FSTree a
fromFile path x = case splitFileName (normalise path) of
    (_, "") -> panicPure $ printf "fromFiles: %s is not a file" path
    ("./", file) -> Dir (Map.singleton (Item file) (File x))
    (dir, file) ->
        let dirs = splitDirectories dir
        in  Prelude.foldr
              (\d acc -> Dir (Map.singleton (Item d) acc))
              (Dir (Map.singleton (Item file) (File x)))
              dirs

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

withFullPaths_ :: Shortcut a -> Shortcut (FilePath, a)
withFullPaths_ sc = sc { tree = go <$> withFullPaths sc.tree }
  where
    go (path, x) = (sc.shortcut </> path, x)

withFullPaths :: FSTree a -> FSTree (FilePath, a)
withFullPaths = go ""
  where
    go path = \case
        Dir xs -> Dir $ Map.mapWithKey (\i x -> go (path </> i.un) x) xs
        File x -> File (path, x)

{-------------------------------------------------------------------------------
  IO
-------------------------------------------------------------------------------}

listDirectoryRecursive :: FilePath -> IO ShortcutSkeleton
listDirectoryRecursive = \path -> Shortcut path <$> goDir path
  where
    goDir :: FilePath -> IO FSTreeSkeleton
    goDir path = do
        xs <- listDirectory path
        fmap Dir $
          fmap Map.fromList $
          forM xs $ \x -> do
            (Item x,) <$> goDirOrFile (path </> x)

    goFile :: IO FSTreeSkeleton
    goFile = pure $ File ()

    goDirOrFile :: FilePath -> IO FSTreeSkeleton
    goDirOrFile path = do
        isDir <- doesDirectoryExist path
        isFile <- doesFileExist path
        if isDir then
          goDir path
        else if isFile then
          goFile
        else
          panicPure $
            printf "listDirectoryRecursive: %s is not a file and not a directory"
              path

readFiles_ :: Shortcut FilePath -> IO (Shortcut String)
readFiles_ = mapM readFile

readFiles :: FSTree FilePath -> IO (FSTree String)
readFiles = mapM readFile

readFile :: FilePath -> IO String
readFile path = UTF8.toString <$> BS.readFile path

writeFiles_ :: Shortcut (FilePath, String) -> IO ()
writeFiles_ = mapM_ (uncurry writeFile)

writeFiles :: FSTree (FilePath, String) -> IO ()
writeFiles = mapM_ (uncurry writeFile)

writeFile :: FilePath -> String -> IO ()
writeFile path s = do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path (UTF8.fromString s)
