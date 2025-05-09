module HsBindgen.C.Fold.DeclState (
    DeclState(..)
  , TypeDecl (..)
    -- * Construction
  , initDeclState
  , registerMainHeader
  , registerMacroExpansion
  , registerMacroType
  , registerInclude
    -- * Query
  , containsMacroExpansion
  , macroTypeEnv
  ) where

import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths
import Data.DynGraph (DynGraph)
import Data.DynGraph qualified as DynGraph
import HsBindgen.C.AST
import HsBindgen.C.AST.Macro qualified as Macro
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Declaration state, maintained during AST construction
data DeclState = DeclState {
      -- | Main header currently being processed
      currentMainHeader :: Maybe (CHeaderIncludePath, SourcePath)
      -- | Macro expansions
      --
      -- Whenever @clang@ expands a macro, we get a (faux?) AST node of type
      -- 'CXCursor_MacroExpansion', which tells us /where/ a macro was
      -- expanded. We keep track of these, so that we know to look out for them;
      -- for example, it can alert us to the fact that a struct field has a
      -- type which is macro defined.
    , macroExpansions  :: !(Set SingleLoc)
    , macroTypes       :: !Macro.MacroTypes
    -- | Type declarations
    --
    -- We accumulate type declarations in (insert)ordered map,
    -- so the ordering resembles the one in the header.
    , typeDeclarations :: !(OMap.OMap CXType TypeDecl)
    -- | C header include path graph
    --
    -- We create a DAG of C header paths with an edge for each @#include@.  The
    -- edges are /reversed/ to represent an \"included by\" relation.
    , cIncludePathGraph :: DynGraph SourcePath
    }
  deriving stock (Show)

data TypeDecl
    = -- | The type is processing with the specified aliases
      --
      -- The list of aliases contains aliases where more than one C spelling
      -- are mapped to the same C AST node, when a @typedef@ name is the same
      -- as a @struct@, @union@, or @enum@ tag.  This is a subset of the
      -- aliases tracked in the following constructor.
      TypeDeclProcessing Type [CName]
    | -- | An alias, for @typedef@s, etc.
      TypeDeclAlias Type
    | TypeDecl Type Decl
  deriving Show

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

initDeclState :: DeclState
initDeclState = DeclState {
      currentMainHeader = Nothing
    , macroExpansions   = Set.empty
    , macroTypes        = Map.empty
    , typeDeclarations  = OMap.empty
    , cIncludePathGraph = DynGraph.empty
    }

registerMainHeader :: CHeaderIncludePath -> SourcePath -> DeclState -> DeclState
registerMainHeader headerIncludePath sourcePath st = st{
      currentMainHeader = Just (headerIncludePath, sourcePath)
    }

registerMacroExpansion :: MultiLoc -> DeclState -> DeclState
registerMacroExpansion loc st = st{
      macroExpansions = Set.insert (multiLocExpansion loc) (macroExpansions st)
    }

registerMacroType :: CName -> Macro.Quant ( Macro.FunValue, Macro.Type Macro.Ty ) -> DeclState -> DeclState
registerMacroType nm ty st = st{
      macroTypes = Map.insert nm ty (macroTypes st)
    }

registerInclude ::
     SourcePath -- ^ Path of header that includes the following header
  -> SourcePath -- ^ Path of the included header
  -> DeclState
  -> DeclState
registerInclude header incHeader st = st{
      cIncludePathGraph =
        DynGraph.insertEdge incHeader header (cIncludePathGraph st)
    }

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

containsMacroExpansion :: Range MultiLoc -> DeclState -> Bool
containsMacroExpansion range DeclState{macroExpansions} = or [
      -- Do a quick O(log n) check first, for the common case that the macro
      -- is right at the start of the range. For example, this would capture
      -- cases such as
      --
      -- > #define T int
      -- >
      -- > struct ExampleStruct {
      -- >   T field;
      -- > };
      Set.member (rangeStart range') macroExpansions

      -- If that fails, do a O(n) scan through all macro expansions
    , any (\e -> fromMaybe False (rangeContainsLoc range' e)) macroExpansions
    ]
  where
    range' :: Range SingleLoc
    range' = multiLocExpansion <$> range

macroTypeEnv :: DeclState -> Macro.TypeEnv
macroTypeEnv st = Macro.TypeEnv{
      typeEnvMacros   = macroTypes st
    , typeEnvTypedefs = knownTypedefs st
    }

{-------------------------------------------------------------------------------
  All known typedefs in the state

  This is non-trivial. For take, let's take these 4 struct definitions:

  >         struct struct1 { int a; };
  > typedef struct         { int a; } struct2;
  > typedef struct struct3 { int a; } struct3_t;
  > typedef struct struct4 { int a; } struct4;

  We get the following declarations in the state:

  == Option 1 (tag, no typedef)

  > TypeDecl (TypeStruct (DeclPathName "struct1" DeclPathCtxtTop)) (
  >    DeclStruct (
  >      Struct {
  >        structDeclPath = DeclPathName "struct1" DeclPathCtxtTop
  >      , structAliases = []
  >   ...

  == Option 2 (typedef, no tag)

  > TypeDecl (TypeStruct (DeclPathAnon (DeclPathCtxtTypedef "struct2"))) (
  >    DeclStruct (
  >      Struct {
  >        structDeclPath = DeclPathAnon (DeclPathCtxtTypedef "struct2")
  >      , structAliases = []
  >   ...

  == Option 3 (typedef and tag, but different)

  > TypeDecl (TypeStruct (DeclPathName "struct3" DeclPathCtxtTop)) (
  >    DeclStruct (
  >      Struct {
  >        structDeclPath = DeclPathName "struct3" DeclPathCtxtTop
  >      , structAliases = []
  >   ...

  with a separate

  > TypeDecl (TypeTypedef "struct3_t") (
  >    DeclTypedef (
  >      Typedef {
  >        typedefName = "struct3_t"
  >   ...

  Option 4 (typedef and tag, identical)

  > TypeDecl (TypeStruct (DeclPathName "struct4" DeclPathCtxtTop)) (
  >    DeclStruct (
  >      Struct {
  >        structDeclPath = DeclPathName "struct4" DeclPathCtxtTop
  >      , structAliases = ["struct4"]
  >   ...
-------------------------------------------------------------------------------}

knownTypedefs :: DeclState -> Map CName Macro.TypedefUnderlyingType
knownTypedefs =
      mconcat
    . map (typeDeclTypedef . snd)
    . OMap.assocs
    . typeDeclarations
  where

typeDeclTypedef :: TypeDecl -> Map CName Macro.TypedefUnderlyingType
typeDeclTypedef (TypeDecl _typ decl) =
    case decl of
      DeclTypedef Typedef{typedefName} ->
        Map.singleton typedefName Macro.NormalTypedef
      DeclStruct Struct{structDeclPath, structAliases} ->
        Map.fromList $
          concat [
            [ (typedef, Macro.AnonStructTypedef)
            | DeclPathAnon (DeclPathCtxtTypedef typedef) <- [structDeclPath]
            ]
            , map (, Macro.NormalTypedef) structAliases
            ]
      DeclEnum Enu{enumDeclPath, enumAliases} ->
        Map.fromList $
          concat [
            [ (typedef, Macro.AnonEnumTypedef)
            | DeclPathAnon (DeclPathCtxtTypedef typedef) <- [enumDeclPath]
            ]
            , map (, Macro.NormalTypedef) enumAliases
            ]
      _otherwise
        -> Map.empty
typeDeclTypedef TypeDeclProcessing{} =
    Map.empty
typeDeclTypedef TypeDeclAlias{} =
    Map.empty
