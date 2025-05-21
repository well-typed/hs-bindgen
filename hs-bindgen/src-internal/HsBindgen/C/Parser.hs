-- | Process the @clang@ C AST.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.Parser qualified as C
module HsBindgen.C.Parser (
    -- * Parsing
    ParseCHeadersException(..)
  , parseCHeaders
    -- * Debugging/development
  , getTargetTriple
  ) where

import Control.Exception
import Control.Tracer (Tracer)
import Data.List qualified as List
import Data.List.Compat ((!?))
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord (comparing)
import Data.Text qualified as Text
import Data.Tree (Tree)
import Data.Tree qualified as Tree
import GHC.Stack (callStack)

import Clang.Args
import Clang.Enum.Bitfield
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Data.DynGraph (DynGraph)
import Data.DynGraph qualified as DynGraph
import HsBindgen.C.AST qualified as C
import HsBindgen.C.Predicate (Predicate)
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.Clang.Args (ExtraClangArgsLog, withExtraClangArgs)
import HsBindgen.Errors
import HsBindgen.ExtBindings
import HsBindgen.Frontend (processTranslationUnit)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Util.Trace (Trace (TraceDiagnostic, TraceExtraClangArgs, TraceSkipped))
import HsBindgen.Util.Tracer (TraceWithCallStack, traceWithCallStack, useTrace)

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

-- | Failed to parse the C source
--
-- This is thrown by 'parseCHeaders'.
data ParseCHeadersException =
    -- | Input header file not found
    ParseCHeadersInputFileNotFound CHeaderIncludePath

    -- | Errors in the C file
    --
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/174> We should
    -- have a pretty renderer for diagnostics. For now we rely on
    -- 'diagnosticFormatted'.
  | ParseCHeadersCErrors [Text]

    -- | We failed to process file for some other reason
  | ParseCHeadersUnknownError (SimpleEnum CXErrorCode)
  deriving stock (Show)

instance Exception ParseCHeadersException where
  toException = hsBindgenExceptionToException
  fromException = hsBindgenExceptionFromException
  displayException = \case
    ParseCHeadersInputFileNotFound path ->
      "header not found: " ++ getCHeaderIncludePath path
    ParseCHeadersCErrors errs -> unlines $ map Text.unpack errs
    ParseCHeadersUnknownError errCode ->
      "unknown error parsing C headers: " ++ show errCode

parseCHeaders ::
     HasCallStack =>
     Tracer IO (TraceWithCallStack Trace)
  -> ClangArgs
  -> Predicate
  -> ExtBindings
  -> [CHeaderIncludePath]
  -> IO ([SourcePath], C.Header) -- ^ List of included headers and parsed header
parseCHeaders tracer args p extBindings headerIncludePaths =
  withExtraClangArgs (useTrace TraceExtraClangArgs tracer) args $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hFilePath hContent $ \file ->
        HighLevel.withTranslationUnit2 index RootHeader.name args' [file] opts $
          \case
            Left err -> throwIO $ ParseCHeadersUnknownError err
            Right unit -> do
              (errors, warnings) <- List.partition diagnosticIsError
                <$> HighLevel.clang_getDiagnostics unit Nothing
              unless (null errors) $ throwIO (getError errors)
              forM_ warnings $ traceWithCallStack
                                 (useTrace TraceDiagnostic tracer)
                                 callStack

              unit' <- processTranslationUnit unit
              print unit'

              error "UHOH"

{-
              (decls, finalDeclState) <-
                C.runFoldState C.initDeclState $
                  HighLevel.clang_visitChildren rootCursor $
                    C.foldDecls (useTrace TraceSkipped tracer) p extBindings headerIncludePaths unit
              let decls' =
                    [ d
                    | C.TypeDecl _ d <-
                        toList (C.typeDeclarations finalDeclState)
                    ]
                  depPaths = List.delete C.RootHeader.name $
                    DynGraph.topSort (C.cIncludePathGraph finalDeclState)
              return (depPaths, C.Header (sortDecls depPaths (decls ++ decls')))
-}
  where
    hFilePath :: FilePath
    hFilePath = getSourcePath RootHeader.name

    hContent :: String
    hContent = RootHeader.content headerIncludePaths

    opts :: BitfieldEnum CXTranslationUnit_Flags
    opts = bitfieldEnum [
          CXTranslationUnit_SkipFunctionBodies
        , CXTranslationUnit_DetailedPreprocessingRecord
        , CXTranslationUnit_IncludeAttributedTypes
        , CXTranslationUnit_VisitImplicitAttributes
        ]

    getError :: [Diagnostic] -> ParseCHeadersException
    getError diags =
      case (Maybe.listToMaybe (mapMaybe getInputFileNotFoundError diags)) of
        Just e  -> e
        Nothing -> ParseCHeadersCErrors $ map diagnosticFormatted diags

    getInputFileNotFoundError :: Diagnostic -> Maybe ParseCHeadersException
    getInputFileNotFoundError Diagnostic{..} = do
      let sloc = multiLocExpansion diagnosticLocation
      guard $ singleLocPath sloc == RootHeader.name
      guard $ " file not found" `Text.isSuffixOf` diagnosticSpelling
      headerIncludePath <- headerIncludePaths !? (singleLocLine sloc - 1)
      return $ ParseCHeadersInputFileNotFound headerIncludePath

{-------------------------------------------------------------------------------
  Debugging/development
-------------------------------------------------------------------------------}

getTargetTriple ::
  Tracer IO (TraceWithCallStack ExtraClangArgsLog) -> ClangArgs -> IO Text
getTargetTriple tracer args =
  withExtraClangArgs tracer args $ \args' ->
    HighLevel.withIndex DontDisplayDiagnostics $ \index ->
      HighLevel.withUnsavedFile hName hContent $ \file ->
        HighLevel.withTranslationUnit2 index hPath args' [file] opts $
          \case
            Left err -> panicPure $
              "Clang parse translation unit error while getting target triple: "
                ++ show err
            Right unit ->
              bracket
                (clang_getTranslationUnitTargetInfo unit)
                clang_TargetInfo_dispose
                clang_TargetInfo_getTriple
  where
    hName :: FilePath
    hName = "hs-bindgen-triple.h"

    hPath :: SourcePath
    hPath = SourcePath $ Text.pack hName

    hContent :: String
    hContent = ""

    opts :: BitfieldEnum CXTranslationUnit_Flags
    opts = bitfieldEnum []

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Sort declarations
--
-- 1. Source path, in specified (topological) order
-- 2. Dependencies
-- 3. Line number
-- 4. Column number
sortDecls :: [SourcePath] -> [C.Decl] -> [C.Decl]
sortDecls sourcePaths unorderedDecls = orderedDecls
  where
    -- (1) The declarations are annotated with information used for sorting.
    -- The are sorted by source location first because the order of insertion
    -- into the dependency graph is significant.  Annotations are retained
    -- becauses they are needed again later.
    annDecls :: [(Ann, C.Decl)]
    annDecls = List.sortBy (comparing fst) $ map annotate unorderedDecls

    annotate :: C.Decl -> (Ann, C.Decl)
    annotate decl =
      let sloc = getSingleLoc decl
          ann  = Ann
                  (Map.findWithDefault maxBound (singleLocPath sloc) sourceMap)
                  (singleLocLine sloc)
                  (singleLocColumn sloc)
      in  (ann, decl)

    sourceMap :: Map SourcePath Int
    sourceMap = Map.fromList $ zip sourcePaths [0..]

    -- (2) These declarations are folded to produce three data structures:
    untypedDecls :: [(Ann, C.Decl)]           -- declarations without types
    typeMap      :: Map C.Type (Ann, C.Decl)  -- map from type to declaration
    depGraph     :: DynGraph C.Type           -- dependency graph
    (untypedDecls, typeMap, depGraph) =
      foldl' indexDecl ([], Map.empty, DynGraph.empty) annDecls

    -- (3) The spanning forest of the dependency graph is calculated.  The order
    -- is significant, determined by the order of insertion.
    forest :: [Tree C.Type]
    forest = DynGraph.dff depGraph

    -- (4) The root node of each tree is resolved.
    forest' :: [((Ann, C.Decl), [Tree C.Type])]
    forest' = map unTree forest

    unTree :: Tree C.Type -> ((Ann, C.Decl), [Tree C.Type])
    unTree (Tree.Node nodeType children) = case Map.lookup nodeType typeMap of
      Just annDecl -> (annDecl, children)
      Nothing      -> panicPure $ "type not found: " ++ show nodeType

    -- (5) The untyped declarations are merged into the forest.
    forest'' :: [((Ann, C.Decl), [Tree C.Type])]
    forest'' = mergeBy (comparing (fst . fst)) forest' $
      (, []) <$> reverse untypedDecls

    -- (6) Each tree is converted to a list of declarations using postorder
    -- traversal.  Child nodes are sorted.
    annDeclss :: [[(Ann, C.Decl)]]
    annDeclss = flip postorder [] <$> forest''

    postorder ::
         ((Ann, C.Decl), [Tree C.Type])
      -> [(Ann, C.Decl)]
      -> [(Ann, C.Decl)]
    postorder (annDecl, children) = postorderF (unTrees children) . (annDecl :)

    postorderF ::
         [((Ann, C.Decl), [Tree C.Type])]
      -> [(Ann, C.Decl)]
      -> [(Ann, C.Decl)]
    postorderF = foldr ((.) . postorder) id

    unTrees :: [Tree C.Type] -> [((Ann, C.Decl), [Tree C.Type])]
    unTrees = List.sortOn (fst . fst) . map unTree

    -- (7) The ordered declarations are extracted.
    orderedDecls :: [C.Decl]
    orderedDecls = concatMap (map snd) annDeclss

-- | Annotation: path index, line number, column number
data Ann = Ann Int Int Int
  deriving (Eq, Ord, Show)

getSingleLoc :: C.Decl -> SingleLoc
getSingleLoc = \case
    C.DeclStruct s       -> C.structSourceLoc s
    C.DeclOpaqueStruct o -> C.opaqueStructSourceLoc o
    C.DeclUnion u        -> C.unionSourceLoc u
    C.DeclTypedef t      -> C.typedefSourceLoc t
    C.DeclEnum e         -> C.enumSourceLoc e
    C.DeclOpaqueEnum o   -> C.opaqueEnumSourceLoc o
    C.DeclMacro m        -> case m of
      C.MacroReparseError{} -> C.macroReparseErrorSourceLoc m
      C.MacroTcError{}      -> C.macroTcErrorSourceLoc m
      C.MacroDecl{}         -> C.macroDeclSourceLoc m
    C.DeclFunction f     -> C.functionSourceLoc f

indexDecl ::
     ([(Ann, C.Decl)], Map C.Type (Ann, C.Decl), DynGraph C.Type)
  -> (Ann, C.Decl)
  -> ([(Ann, C.Decl)], Map C.Type (Ann, C.Decl), DynGraph C.Type)
indexDecl (otherAcc, typeMap, typeGraph) x@(_ann, decl) = case getDeps decl of
    Just (type', types') ->
      ( otherAcc
      , Map.insert type' x typeMap
      , foldr
          (DynGraph.insertEdge type')
          (DynGraph.insertVertex type' typeGraph)
          types'
      )
    Nothing -> (x : otherAcc, typeMap, typeGraph)

getDeps :: C.Decl -> Maybe (C.Type, [C.Type])
getDeps = fmap (fmap aux) . \case
    C.DeclStruct s ->
      Just (C.TypeStruct (C.structDeclPath s), C.fieldType <$> C.structFields s)
    C.DeclOpaqueStruct o ->
      Just (C.TypeStruct (C.DeclPathName (C.opaqueStructTag o)), [])
    C.DeclUnion u ->
      Just (C.TypeUnion (C.unionDeclPath u), C.ufieldType <$> C.unionFields u)
    C.DeclTypedef t -> Just (C.TypeTypedef (C.typedefName t), [C.typedefType t])
    C.DeclEnum e -> Just (C.TypeEnum (C.enumDeclPath e), [])
    C.DeclOpaqueEnum o ->
      Just (C.TypeEnum (C.DeclPathName (C.opaqueEnumTag o)), [])
    C.DeclMacro m -> case m of
      C.MacroReparseError{} -> Nothing
      C.MacroTcError{} -> Nothing
      C.MacroDecl{macroDeclMacro = macro, macroDeclMacroTy = macroTy} ->
        case (macroTy, C.macroBody macro) of
          (Macro.Quant bf, C.TypeMacro tyNm)
            | Macro.isPrimTy bf
            , Right ty <- C.typeNameType tyNm
            -> Just (C.TypeTypedef (C.macroName macro), [ty])
            | otherwise -> Nothing
          _otherwise -> Nothing
    C.DeclFunction{} -> Nothing
  where
    aux :: [C.Type] -> [C.Type]
    aux = concatMap $ \case
      C.TypePrim{}            -> []
      t@C.TypeStruct{}        -> [t]
      t@C.TypeUnion{}         -> [t]
      t@C.TypeEnum{}          -> [t]
      t@C.TypeTypedef{}       -> [t]
      C.TypePointer{}         -> [] -- avoid circular dependencies
      C.TypeConstArray _ t    -> aux [t]
      C.TypeFun ts t          -> aux $ t : ts
      C.TypeVoid              -> []
      C.TypeIncompleteArray t -> aux [t]
      C.TypeExtBinding{}      -> []

mergeBy :: forall a. (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy f = aux
  where
    aux :: [a] -> [a] -> [a]
    aux xs [] = xs
    aux [] ys = ys
    aux xs@(x:xs') ys@(y:ys')
      | f x y /= GT = x : aux xs' ys
      | otherwise   = y : aux xs  ys'

