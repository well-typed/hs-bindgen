-- | Generate bindings for a C library, one module per header (@--library@)
--
-- Intended for qualified import.
--
-- > import HsBindgen.Library qualified as Library
module HsBindgen.Library (
    -- * Library directories
    LibraryDirs -- opaque
  , getLibraryDirs
    -- * Plan
  , LibraryPlan(..)
  , LibraryModule(..)
  , planLibrary
  , typesModule
  , librarySelection
  , moduleSelection
  , moduleNameOf
  , moduleNameClashes
    -- * Output
  , fileClashes
    -- * Errors
  , LibraryError(..)
  ) where

import Control.Monad (filterM)
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath qualified as FilePath
import Text.SimplePrettyPrint ((<+>), (><))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Backend.Category (Category (..))
import HsBindgen.Config.Prelims
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
import HsBindgen.Frontend.Analysis.UseDeclGraph qualified as UseDeclGraph
import HsBindgen.Frontend.DeclMeta
import HsBindgen.Frontend.Pass.MangleNames.CreateNames (withDeclNamespace)
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass
import HsBindgen.Frontend.Pass.Select (matchSelectionRoots)
import HsBindgen.Frontend.Predicate
import HsBindgen.Frontend.TranslationUnit qualified as C
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Library directories
-------------------------------------------------------------------------------}

-- | The directories of a library, which exist, as canonical paths
--
-- A header belongs to the library when its real path lies in one of them.
newtype LibraryDirs = LibraryDirs (NonEmpty FilePath)
  deriving stock (Show)

-- | Check that the directories of a library exist, and make their paths
-- canonical
--
-- 'Left' lists the directories that do not exist.
getLibraryDirs :: NonEmpty FilePath -> IO (Either (NonEmpty FilePath) LibraryDirs)
getLibraryDirs dirs = do
    missing <- filterM (fmap not . doesDirectoryExist) (NonEmpty.toList dirs)
    case NonEmpty.nonEmpty missing of
      Just missing' -> pure $ Left missing'
      Nothing       -> Right . LibraryDirs <$> traverse canonicalizePath dirs

{-------------------------------------------------------------------------------
  Plan
-------------------------------------------------------------------------------}

-- | Where every declaration of a library goes
data LibraryPlan = LibraryPlan {
      -- | The modules, in include order of their headers
      modules   :: [LibraryModule]
      -- | The types modules that each declaration ends up with
      --
      -- A type ends up with exactly one, and a value with every one that
      -- needs it (see 'HsBindgen.Frontend.Pass.MangleNames.ByTypesModule').
    , placement :: Map C.DeclId (Set Hs.ModuleName)
    }
  deriving stock (Show)

-- | A module of a library, generated from the declarations of one header
data LibraryModule = LibraryModule {
      name    :: BaseModuleName
    , header  :: RealPath
      -- | The types modules of the other headers whose types its declarations
      -- reach, directly or through the types of other modules
      --
      -- Its term modules import these for their instances (see
      -- 'LibraryImports').
    , reaches :: Set Hs.ModuleName
    }
  deriving stock (Show)

-- | The types module of a module of a library
typesModule :: LibraryModule -> Hs.ModuleName
typesModule libraryModule = fromBaseModuleName libraryModule.name (Just CType)

-- | Plan a library
--
-- The plan takes the translation unit after the user's binding specifications
-- are applied, so a type they bind belongs to no module. It decides which
-- headers get a module, what each module is called, and which declarations
-- end up with each module.
--
-- Each module ends up with what its own run will select: the roots that the
-- user's selection predicate matches in its header ('moduleSelection'), and
-- everything they reach. In that run, the types of every other module are
-- external, so the plan finds the roots with the function that @Select@ uses
-- ('matchSelectionRoots'), and reaches from them the way @Select@ does, over
-- the graph that @ResolveBindingSpecs@ leaves when it makes the types of the
-- other modules external.
--
-- A type belongs to the module whose header defines it, if that header selects
-- it. A squashed typedef and its target are one Haskell type, so a typedef that
-- is a root takes along a target that no header selects. Any other type that a
-- module reaches, from outside the library or from a header that does not
-- select it, belongs to the first module in include order that reaches it. A
-- value belongs to every module that reaches it: values are not shared.
planLibrary ::
     LibraryDirs
  -> BaseModuleName
  -> IsMainHeader
  -> IsInMainHeaderDir
  -> Boolean SelectionPredicate
  -> C.TranslationUnit l ResolveBindingSpecs
  -> Either LibraryError LibraryPlan
planLibrary dirs baseModule isMainHeader isInMainHeaderDir predicate unit = do
    when (null headers) $
      Left $ NoLibraryHeader dirs
    named <- forM withRoots $ \((path, inLibrary), roots) ->
      case moduleNameOf baseModule inLibrary of
        Nothing   -> Left $ NoModuleName path
        Just name -> Right (name, path, roots)
    forM_ (NonEmpty.nonEmpty $ moduleNameClashes [(n, p) | (n, p, _) <- named]) $
      Left . ModuleNameClash
    let slices :: [(Hs.ModuleName, Set C.DeclId)]
        slices = sliceModules unit [
            (fromBaseModuleName name (Just CType), roots)
          | (name, _path, roots) <- named
          ]
    pure LibraryPlan{
        modules   = [
            LibraryModule{name = name, header = path, reaches = reached}
          | ((name, path, _roots), reached) <-
              zip named (reachedTypesModules unit slices)
          ]
      , placement = Map.fromListWith (<>) [
            (declId, Set.singleton typesModule')
          | (typesModule', slice) <- slices
          , declId <- Set.toList slice
          ]
      }
  where
    headers :: [(RealPath, [FilePath])]
    headers = libraryHeaders dirs unit.includeGraph

    withRoots :: [((RealPath, [FilePath]), Set C.DeclId)]
    withRoots = [
        (h, roots)
      | h@(path, _) <- headers
      , let roots = rootsIn path
      , not (Set.null roots)
      ]

    rootsIn :: RealPath -> Set C.DeclId
    rootsIn path =
        DeclIndex.keysSet $
          matchSelectionRoots
            isMainHeader
            isInMainHeaderDir
            (selectInHeader predicate path)
            unit.meta.declUseGraph
            unit.meta.declIndex

-- | The library headers among the headers read, in include order
--
-- Each comes with its path in the outermost library directory that holds it.
libraryHeaders :: LibraryDirs -> IncludeGraph -> [(RealPath, [FilePath])]
libraryHeaders (LibraryDirs dirs) includeGraph = [
      (path, inLibrary)
    | path <- IncludeGraph.toSortedList includeGraph
    , Just inLibrary <- [pathInLibrary path]
    ]
  where
    -- Outermost first
    dirComponents :: [[FilePath]]
    dirComponents =
        List.sortOn length $
          map FilePath.splitDirectories (NonEmpty.toList dirs)

    pathInLibrary :: RealPath -> Maybe [FilePath]
    pathInLibrary path = listToMaybe [
          drop (length dir) components
        | dir <- dirComponents
        , dir `List.isPrefixOf` components
        ]
      where
        components :: [FilePath]
        components = FilePath.splitDirectories (getRealPath path)

-- | What each module reaches from its roots, as its own run will select it
sliceModules ::
     C.TranslationUnit l ResolveBindingSpecs
     -- | The types module and the roots of each module, in include order
  -> [(Hs.ModuleName, Set C.DeclId)]
  -> [(Hs.ModuleName, Set C.DeclId)]
sliceModules unit modules = slices Set.empty modules
  where
    -- What each module reaches from its roots, given the types that modules
    -- earlier in include order took along
    slices ::
         Set C.DeclId
      -> [(Hs.ModuleName, Set C.DeclId)]
      -> [(Hs.ModuleName, Set C.DeclId)]
    slices _       []                          = []
    slices earlier ((typesModule', roots) : later) =
        (typesModule', slice) : slices (earlier <> takenAlong) later
      where
        external :: Set C.DeclId
        external = Map.keysSet (Map.filter (/= typesModule') owned) <> earlier

        slice :: Set C.DeclId
        slice =
          UseDeclGraph.getTransitiveDeps
            (UseDeclGraph.fromDeclUseGraph $
              DeclUseGraph.deleteRevDeps external unit.meta.declUseGraph)
            roots

        takenAlong :: Set C.DeclId
        takenAlong = Set.intersection slice types `Set.difference` Map.keysSet owned

    -- The types that a header defines and selects, and the targets that its
    -- squashed typedefs take along
    owned :: Map C.DeclId Hs.ModuleName
    owned = rootTypes <> Map.fromList [
          (target, typesModule')
        | (typedef, typesModule') <- Map.toList rootTypes
        , Just target <- [Map.lookup typedef squashTargets]
        , not (Map.member target rootTypes)
        ]

    rootTypes :: Map C.DeclId Hs.ModuleName
    rootTypes = Map.fromList [
          (declId, typesModule')
        | (typesModule', roots) <- modules
        , declId <- Set.toList (Set.intersection roots types)
        ]

    types :: Set C.DeclId
    types = libraryTypes unit

    -- Which typedefs are squashed does not depend on where declarations end
    -- up (only the suffixes of 'TypedefAnalysis.AddSuffix' do), so any
    -- placement gives the same answer.
    squashTargets :: Map C.DeclId C.DeclId
    squashTargets = Map.fromList [
          (typedef, squash.targetId)
        | (typedef, TypedefAnalysis.Squash squash) <-
            Map.toList $
              (TypedefAnalysis.fromDecls
                (\_ _ -> True)
                unit.meta.declUseGraph
                unit.decls
              ).map
        ]

-- | For each module, the types modules of the other modules whose types the
-- declarations of the module reach
--
-- They reach a type through any declaration, and through the types of other
-- modules. That is a little more than the term modules need for their
-- instances, which is harmless.
reachedTypesModules ::
     C.TranslationUnit l ResolveBindingSpecs
     -- | What each module reaches from its roots (see 'sliceModules')
  -> [(Hs.ModuleName, Set C.DeclId)]
  -> [Set Hs.ModuleName]
reachedTypesModules unit slices = [
      Set.delete typesModule' $ Set.fromList [
          owner
        | declId <-
            Set.toList $
              UseDeclGraph.getTransitiveDeps unit.meta.useDeclGraph slice
        , Just owner <- [Map.lookup declId typeModules]
        ]
    | (typesModule', slice) <- slices
    ]
  where
    -- A type ends up with one module only
    typeModules :: Map C.DeclId Hs.ModuleName
    typeModules = Map.fromList [
          (declId, typesModule')
        | (typesModule', slice) <- slices
        , declId <- Set.toList (Set.intersection slice types)
        ]

    types :: Set C.DeclId
    types = libraryTypes unit

-- | The declarations that get a binding specification: those with a type name
-- that lie in a header
libraryTypes :: C.TranslationUnit l ResolveBindingSpecs -> Set C.DeclId
libraryTypes unit = Set.fromList [
      decl.info.id
    | decl <- unit.decls
    , C.InHeader _ <- [singleLocPath decl.info.loc]
    , withDeclNamespace decl.kind $ \(_ :: Proxy ns) ->
        Hs.namespaceOf (Hs.singNamespace @ns) == Hs.NsTypeConstr
    ]

-- | The selection of the run of the whole library: the user's selection,
-- restricted to the library headers
--
-- This selects what the modules select together, since a library header that
-- is not a module is one in which the user's selection matches nothing. The
-- regex names the library directories rather than the headers of the modules:
-- PCRE refuses a pattern past a size limit, which an alternation of a few
-- hundred header paths reaches.
librarySelection ::
     Boolean SelectionPredicate
  -> LibraryDirs
  -> Boolean SelectionPredicate
librarySelection predicate (LibraryDirs dirs) =
    selectMatching predicate $ concat [
        "\\A(?:"
      , List.intercalate "|" $
          map
            (escapeRegex . FilePath.addTrailingPathSeparator)
            (NonEmpty.toList dirs)
      , ")"
      ]

-- | The selection of the run of a module: the user's selection, restricted to
-- its header
moduleSelection ::
     Boolean SelectionPredicate
  -> LibraryModule
  -> Boolean SelectionPredicate
moduleSelection predicate libraryModule =
    selectInHeader predicate libraryModule.header

-- | The selection, restricted to declarations in this header
selectInHeader ::
     Boolean SelectionPredicate
  -> RealPath
  -> Boolean SelectionPredicate
selectInHeader predicate path =
    selectMatching predicate $ "\\A" ++ escapeRegex (getRealPath path) ++ "\\z"

-- | The selection, restricted to the headers whose real paths match the regex
--
-- 'HeaderPathMatches' matches the real path of a header.
selectMatching ::
     Boolean SelectionPredicate
  -> String
  -> Boolean SelectionPredicate
selectMatching predicate regex =
    BAnd predicate $ BIf $ SelectHeader $ HeaderPathMatches $ fromString regex

-- | Quote a string, so that a regex matches it literally
--
-- In a PCRE, a backslash makes any character that is not a letter or a digit
-- stand for itself.
escapeRegex :: String -> String
escapeRegex = concatMap $ \c ->
    if isAscii c && not (isAsciiUpper c || isAsciiLower c || isDigit c)
      then ['\\', c]
      else [c]

-- | The base module name of a library header, from its path in the library
--
-- Each component of the path becomes a component of the module name, after the
-- base module name: characters other than ASCII letters, digits and @_@ become
-- @_@, the first ASCII letter is upper-cased, and a component that starts with a
-- digit or @_@ gets the prefix @C@. The extension of the header, from the last
-- dot on, is dropped. 'Nothing' if that leaves no name, as for @.h@.
--
-- >>> moduleNameOf "Lib" ["mylib", "foo-bar.h"]
-- Just "Lib.Mylib.Foo_bar"
moduleNameOf :: BaseModuleName -> [FilePath] -> Maybe BaseModuleName
moduleNameOf baseModule inLibrary = do
    components <- NonEmpty.nonEmpty inLibrary
    let stem = FilePath.dropExtension (NonEmpty.last components)
    guard $ not (null stem)
    pure $ BaseModuleName $ Text.intercalate "." $
        baseModule.text
      : map
          (Text.pack . moduleNameComponent)
          (NonEmpty.init components ++ [stem])
  where
    moduleNameComponent :: String -> String
    moduleNameComponent = prefix . upperFirstLetter . map validChar

    validChar :: Char -> Char
    validChar c
      | isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' = c
      | otherwise                                                 = '_'

    upperFirstLetter :: String -> String
    upperFirstLetter s =
        case break isAsciiLetter s of
          (before, c : after) -> before ++ toUpper c : after
          (_, [])             -> s

    isAsciiLetter :: Char -> Bool
    isAsciiLetter c = isAsciiUpper c || isAsciiLower c

    prefix :: String -> String
    prefix s = case s of
        c : _ | isDigit c || c == '_' -> 'C' : s
        _otherwise                    -> s

-- | The headers whose module names are equal when case is ignored, in groups of
-- two or more
--
-- Their files would clash on a file system that ignores case.
moduleNameClashes ::
     [(BaseModuleName, RealPath)]
  -> [NonEmpty (BaseModuleName, RealPath)]
moduleNameClashes modules = [
      clash
    | clash@(_ :| (_ : _)) <-
        Map.elems $ Map.fromListWith (flip (<>)) [
            (Text.toCaseFold name.text, (name, path) :| [])
          | (name, path) <- modules
          ]
    ]

{-------------------------------------------------------------------------------
  Output
-------------------------------------------------------------------------------}

-- | The files that the modules would write whose paths are equal when case is
-- ignored, in groups of two or more
--
-- Two headers can give such files without giving such module names: @foo.h@
-- and @foo/safe.h@ both give @Foo/Safe.hs@.
fileClashes ::
     [(BaseModuleName, [FilePath])]
  -> [NonEmpty (BaseModuleName, FilePath)]
fileClashes files = [
      clash
    | clash@(_ :| (_ : _)) <-
        Map.elems $ Map.fromListWith (flip (<>)) [
            (Text.toCaseFold (Text.pack path), (name, path) :| [])
          | (name, paths) <- files
          , path <- paths
          ]
    ]

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data LibraryError =
    -- | None of the headers read lies in a library directory
    NoLibraryHeader LibraryDirs
    -- | The path of a header in its library directory leaves no module name
  | NoModuleName RealPath
    -- | Headers whose module names are equal when case is ignored
  | ModuleNameClash (NonEmpty (NonEmpty (BaseModuleName, RealPath)))
    -- | Files of the modules whose paths are equal when case is ignored
  | FileClash (NonEmpty (NonEmpty (BaseModuleName, FilePath)))
  deriving stock (Show)

instance PrettyForTrace LibraryError where
  prettyForTrace = \case
    NoLibraryHeader (LibraryDirs dirs) -> PP.vsep [
        "None of the headers read is in a library directory:"
          <+> PP.hcat (List.intersperse ", " $ map PP.string (NonEmpty.toList dirs))
      , "Give the headers of the library as arguments, or include them from one."
      ]
    NoModuleName path ->
      "No module name for header" <+> PP.string (getRealPath path)
        >< ": its file name is only an extension"
    ModuleNameClash clashes -> PP.vsep [
        PP.vcat $
            "Headers whose module names are equal when case is ignored:"
          : [ PP.nest 2 $ PP.vcat [
                  PP.string (baseModuleNameToString name)
                    >< ":" <+> PP.string (getRealPath path)
                | (name, path) <- NonEmpty.toList clash
                ]
            | clash <- NonEmpty.toList clashes
            ]
      , "Leave all but one out with --select-except-by-header-path, or use one"
          <+> "directory that holds all of them as the library."
      ]
    FileClash clashes -> PP.vsep [
        PP.vcat $
            "Modules that would write files whose paths are equal when case is"
              <+> "ignored:"
          : [ PP.nest 2 $ PP.vcat [
                  PP.string (baseModuleNameToString name)
                    >< ":" <+> PP.string path
                | (name, path) <- NonEmpty.toList clash
                ]
            | clash <- NonEmpty.toList clashes
            ]
      , "Leave all but one of their headers out with"
          <+> "--select-except-by-header-path."
      ]
