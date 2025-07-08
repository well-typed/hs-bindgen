#!/usr/bin/env runghc
{-|
Module      : include-graph
Description : Traverses the current directory tree, finds @.h@ header files,
              builds an include‑dependency graph, and prints it in topologically
              sorted order (one stanza per file).
              If a cycle exists, the script reports it explicitly.
-}

{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- Imports --------------------------------------------------------------------
-------------------------------------------------------------------------------
import Control.Monad          (forM, foldM)
import Data.Char              (isSpace)
import Data.List              (delete, intersperse, sort)
import Data.Maybe             (catMaybes, isJust)
import Data.Text              (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Map.Strict       as Map
import           Data.Map.Strict        (Map)
import System.Directory       (doesDirectoryExist, doesFileExist,
                               findFile, getCurrentDirectory, listDirectory)
import System.FilePath        ((</>), takeExtension, makeRelative,
                               dropTrailingPathSeparator, takeFileName)

-- | The include‑graph: each header maps to the list of headers it includes.
type IncludeGraph = Map Text [Text]

-- | Pretty‑print a single key (header) and its dependencies.
prettyKey :: Text -> IncludeGraph -> Maybe Text
prettyKey k ig = do
  deps <- Map.lookup k ig
  let quoted = map (\d -> "\"" <> d <> "\"") deps
  pure $ "[" <> k <> "]\n" <> T.unlines quoted

-- | Pretty‑print the whole graph, topologically sorted.
prettyGraphTopSorted :: IncludeGraph -> Maybe Text
prettyGraphTopSorted ig = do
  order  <- topologicalSort ig
  blocks <- traverse (`prettyKey` ig) order
  pure $ T.intercalate "\n" blocks

-- | Pretty‑print the graph in whatever order @Map.assocs@ gives.
prettyGraph :: IncludeGraph -> Text
prettyGraph =
  T.concat . map render . Map.assocs
  where
    render (k,deps) =
      "[" <> k <> "]\n"
      <> T.unlines (map (\d -> "\"" <> d <> "\"") deps)
      <> "\n"

-- | Kahn's topological sort.  Returns 'Nothing' when a cycle exists.
topologicalSort :: IncludeGraph -> Maybe [Text]
topologicalSort ig =
    let zeroInDeg = [h | (h, deps) <- Map.assocs ig, null deps]
    in go zeroInDeg [] [] ig
  where
    -- | 'go currentQueue visited result graph'
    --   * @currentQueue@: nodes with zero in‑degree yet to visit
    --   * @visited@:      nodes already removed from the graph (used to avoid re‑queuing)
    --   * @result@:       final topological order being accumulated (reversed)
    go []     _    res m | length res == Map.size m = Just (reverse res)
                         | otherwise                = Nothing   -- cycle detected
    go (x:xs) seen res m = do
      let m'   = Map.map (delete x) m
          new0 = [ h
                 | (h, deps) <- Map.assocs m'
                 , null deps
                 , h `notElem` (x:xs)
                 , h `notElem` seen
                 ]
      go (xs ++ new0) (x:seen) (x:res) m'

-- | Recursively collect all @*.h@ files starting from a directory,
--   returning *relative* paths (to the working directory).
getHeaderFiles :: FilePath -> IO [FilePath]
getHeaderFiles dir = do
  entries <- listDirectory dir
  fmap concat . forM entries $ \e -> do
    let p = dir </> e
    isDir  <- doesDirectoryExist p
    isFile <- doesFileExist p
    if isDir
      then getHeaderFiles p
      else pure [ makeRelative "." p
                | isFile && takeExtension e == ".h" ]

-- | Extract the header name from a @#include "foo/bar.h"@ line.
--   Returns @Nothing@ for any non‑include line or if the file
--   cannot be resolved under the current directory.
stripInclude :: T.Text -> IO (Maybe Text)
stripInclude line = do
  cwd <- getCurrentDirectory
  let cwdName = takeFileName $ dropTrailingPathSeparator cwd
  siblings <- listDirectory cwd
  let headerPart = do
        rest <- T.stripPrefix "#include" line
        let trimmed = T.dropWhile isSpace rest
        T.stripSuffix "\"" =<< T.stripPrefix "\"" trimmed
  case headerPart of
    Nothing      -> pure Nothing
    Just rawName -> do
      let rel = makeRelative cwdName (T.unpack rawName)
      found <- isJust <$> findFile (cwd:siblings) rel
      pure $ if found then Just (T.pack rel) else Nothing

main :: IO ()
main = do
  headers <- sort <$> getHeaderFiles "."
  graph   <- foldM build Map.empty headers

  case prettyGraphTopSorted graph of
    Nothing  -> T.putStrLn "Circular dependency found"
    Just doc -> T.putStrLn doc

  where
    build m h = do
      lines'   <- T.lines <$> T.readFile h
      includes <- catMaybes <$> (traverse stripInclude lines')
      pure $ Map.insertWith (<>) (T.pack h) includes m
