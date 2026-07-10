-- | Doxygen-driven export resolution
--
-- Builds a hierarchical 'ExportEntry' tree from a flat list of 'SDecl' values
-- and an 'ExportTags' map.  Section nesting is intrinsic to the tree: depth
-- corresponds to recursion level, so the pretty-printer never has to carry an
-- explicit depth field.
--
-- Per-declaration tag computation lives upstream (see @computeExportTags@ in
-- "HsBindgen") and walks the final C declarations.  Consumers here perform a
-- single 'ExportTags' lookup keyed by the Haskell export name: no fallbacks,
-- no rediscovery of C information from the Hs AST.
module HsBindgen.Backend.HsModule.Translation.Doxygen (
    ExportTags
  , ExportGroupTag(..)
  , computeExportTags
  , resolveExports
  ) where

import Data.List (partition)
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.HsModule.Translation (ExportEntry (..), ExportItem,
                                               resolveDeclExports)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.MangleNames.IsPass (FlamNames (..),
                                                   TypedefNames (..))
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Translation
import HsBindgen.Language.Haskell qualified as Hs

import Doxygen.Parser (Doxygen, lookupGroupInfo, lookupGroupMembership)

{-------------------------------------------------------------------------------
  ExportTags
-------------------------------------------------------------------------------}

-- | Per-Haskell-name precomputed export-section tag.
--
-- Computed at the C-declaration level (see @computeExportTags@ in "HsBindgen")
-- and consumed by 'resolveExports'. Keys are Haskell names as they appear in
-- the export list.
--
-- Backend-synthesised companion declarations such as the @_Aux@ newtypes
-- generated from typedef function pointers, or the FLAM auxiliary struct, are
-- inserted under their own Haskell name and inherit the tag of their parent
-- C declaration. This makes 'resolveExports' a single-step lookup.
--
-- A missing key is interpreted as 'Derived', meaning the declaration inherits
-- the preceding 'Grouped' section in source order. Top-level C declarations
-- without group membership are explicitly inserted as 'Ungrouped'.
--
-- Example: for a header with an ungrouped @api_version_t@ and a
-- @\@defgroup core "Core"@ containing @config_t@ plus an @enum color@:
--
-- > fromList
-- >   [ ("Api_version_t", Ungrouped)
-- >   , ("Config_t",      Grouped ["Core"])
-- >   , ("Color",         Grouped ["Core"])
-- >   ]
type ExportTags = Map Text ExportGroupTag

{-------------------------------------------------------------------------------
  Tagging
-------------------------------------------------------------------------------}

-- | How a declaration relates to doxygen group sections.
data ExportGroupTag =
    -- | Top-level C declaration not in any doxygen group.  Hoisted before
    -- all section headers by 'resolveExports'.
    Ungrouped
    -- | Backend-derived declaration (e.g. pattern synonyms, anonymous inner
    -- types) without its own group membership.  Inherits the enclosing
    -- section from the preceding grouped declaration.
  | Derived
    -- | Member of a doxygen group.  The path lists section titles from
    -- root to leaf (e.g. @[\"Outer Group\", \"Inner A\"]@).
  | Grouped [Text]

-- | Build an 'ExportTags' map from Doxygen metadata and the final C
-- declarations.
--
-- The resulting map is keyed by Haskell name. 'resolveExports' performs a single
-- lookup with no fallbacks.
--
-- For each 'C.Decl Final':
--
--  * If the originating C name belongs to a @\@defgroup@, the tag is
--    @'Grouped' path@ (root-to-leaf section titles).
--  * Otherwise, if the declaration is top-level (@null info.enclosing@), the
--    tag is 'Ungrouped' and the declaration is explicitly hoisted before any
--    section headers in the export list.
--  * Otherwise (nested, no group), the declaration is omitted from the map.
--    Missing keys are interpreted as 'Derived' by 'resolveExports', so the
--    declaration inherits the preceding 'Grouped' section in source order.
--
-- Backend-synthesised companion declarations are inserted under their own
-- Haskell name with the same tag as their parent:
--
--  * Typedef function pointers contribute an auxiliary newtype @F_Aux@ whose
--    name comes from @typedef.names.aux@; the @_Aux@ form is delayed during
--    Hs translation and therefore not adjacent to its parent in source
--    order, so an entry under its own Hs name is necessary to keep it in
--    its parent's section.
--  * Structs with flexible array members contribute an auxiliary type whose
--    name comes from the @FlamNames@ carried by the struct's @C.Flam@ field
--    (@struct.flam@); same reasoning.
--
-- Example: given a C header with
--
-- > /** @defgroup core "Core Data Types" @{ */
-- > typedef struct { ... } config_t;
-- > /** @} */
--
-- and the Haskell name @Config_t@, this produces:
--
-- > fromList [("Config_t", Grouped ["Core Data Types"])]
computeExportTags :: Doxygen -> [C.Decl l Final] -> ExportTags
computeExportTags doxy decls =
    Map.fromList $ concatMap declTagEntries decls
  where
    declTagEntries :: C.Decl l Final -> [(Text, ExportGroupTag)]
    declTagEntries decl =
        case declTag decl of
          Nothing  -> []
          Just tag ->
            (decl.info.id.hsName.text, tag) : auxEntries decl.kind tag

    -- | Compute the tag for a top-level or grouped declaration.  Returns
    -- 'Nothing' for nested declarations without a group (those flow through
    -- as 'Derived' by lookup miss in 'resolveExports').
    declTag :: C.Decl l Final -> Maybe ExportGroupTag
    declTag decl =
      case groupPath decl.info.id.cName.name.text of
        Just path -> Just (Grouped path)
        Nothing
          | null decl.info.enclosing -> Just Ungrouped
          | otherwise                -> Nothing

    -- | Extra entries for backend-synthesised companion decls that share
    -- their parent's group membership.
    auxEntries :: C.DeclKind l Final -> ExportGroupTag -> [(Text, ExportGroupTag)]
    auxEntries kind tag = case kind of
      C.DeclTypedef typedef
        | Just (auxName, _) <- typedef.names.aux ->
            [(auxName.text, tag)]
      C.DeclStruct struct
        | C.Flam _ flamNames <- struct.flam ->
            [(flamNames.aux.text, tag)]
      _ -> []

    -- | Resolve the full group title path (root to leaf) for a C name.
    --
    -- Walks the Doxygen group hierarchy upward from the declaration's
    -- immediate group to the root, collecting titles along the way.
    --
    -- > groupPath "config_t"
    -- >   -- lookupGroupMembership → Just "core_types"
    -- >   -- lookupGroupInfo "core_types" → Just ("Core Data Types", Nothing)
    -- >   ==> Just ["Core Data Types"]
    -- >
    -- > groupPath "inner_typ"
    -- >   -- lookupGroupMembership → Just "inner_a"
    -- >   -- lookupGroupInfo "inner_a" → Just ("Inner A", Just "outer")
    -- >   -- lookupGroupInfo "outer"   → Just ("Outer Group", Nothing)
    -- >   ==> Just ["Outer Group", "Inner A"]
    groupPath :: Text -> Maybe [Text]
    groupPath declName = do
      groupName <- lookupGroupMembership declName doxy
      (title, mParent) <- lookupGroupInfo groupName doxy
      pure $ buildPath [title] mParent

    -- | Accumulate group titles from leaf to root, prepending each parent.
    buildPath :: [Text] -> Maybe Text -> [Text]
    buildPath acc Nothing = acc
    buildPath acc (Just parentName) =
      case lookupGroupInfo parentName doxy of
        Just (parentTitle, grandparent) ->
          buildPath (parentTitle : acc) grandparent
        Nothing -> acc

{-------------------------------------------------------------------------------
  Public entry point
-------------------------------------------------------------------------------}

-- | Resolve exports for a declaration list using precomputed export tags.
--
-- The export list is assembled in three stages:
--
--  1. __Tag__ each export item by looking up the declaration's Haskell name
--     in 'ExportTags'.  A missing key yields 'Derived'.
--  2. __Assign paths__ to 'Derived' items by inheriting the most-recent
--     'Grouped' path.  Items appearing before any 'Grouped' get no path
--     and are hoisted alongside ungrouped items.
--  3. __Build the tree__: items with paths are folded into a recursive
--     'ExportEntry' tree where consecutive items sharing a path prefix
--     are nested under the same 'ExportSection'.  Depth is implicit in
--     the tree structure.
--
resolveExports :: ExportTags -> [SDecl] -> [ExportEntry]
resolveExports tags decls =
    let assigned             = assignPaths (concatMap (taggedExports tags) decls)
        (pathless, withPath) = partition (isNothing . fst) assigned
        pathlessEntries      = [ ExportEntry item | (_, item) <- pathless ]
        treeEntries          = buildTree [ (p, item) | (Just p, item) <- withPath ]
    in  pathlessEntries ++ treeEntries

{-------------------------------------------------------------------------------
  Tag pipeline
-------------------------------------------------------------------------------}

-- | Pair each export item produced by a declaration with its tag.
--
-- A single 'SDecl' can produce multiple export items (e.g. a record type
-- exports both @TypeName(..)@ and its pattern synonyms).  All items derived
-- from one declaration share the declaration's tag.
taggedExports :: ExportTags -> SDecl -> [(ExportGroupTag, ExportItem)]
taggedExports tags decl =
    let tag = case sdeclExportedName decl of
          Just n  -> Map.findWithDefault Derived n tags
          Nothing -> Derived
    in [ (tag, item) | item <- resolveDeclExports decl ]

-- | Replace 'Derived' tags with the most-recent 'Grouped' path.
--
-- 'Ungrouped' items always get @Nothing@; 'Grouped' items get their own
-- path.  'Derived' items inherit the @Just _@ from the closest preceding
-- 'Grouped'; if none has occurred yet, they get @Nothing@ (and end up
-- hoisted with the ungrouped items).
assignPaths
  :: [(ExportGroupTag, ExportItem)]
  -> [(Maybe [Text], ExportItem)]
assignPaths = go Nothing
  where
    go _   []                         = []
    go cur ((Ungrouped,   i) : rest)  = (Nothing, i) : go cur  rest
    go _   ((Grouped p,   i) : rest)  = (Just p, i)  : go (Just p) rest
    go cur ((Derived,     i) : rest)  = (cur, i)     : go cur  rest

{-------------------------------------------------------------------------------
  Tree assembly
-------------------------------------------------------------------------------}

-- | Build a hierarchical export tree from items in source order.
--
-- Each item carries the remaining path beneath the current recursion point:
-- an empty path means the item belongs at this level; a non-empty path
-- starts a (or continues an existing) sub-section under its head segment.
-- Consecutive items sharing the same head segment are grouped under one
-- 'ExportSection' and the recursion descends one level after stripping it.
buildTree :: [([Text], ExportItem)] -> [ExportEntry]
buildTree []                      = []
buildTree (([], item) : rest)     = ExportEntry item : buildTree rest
buildTree items@((seg : _, _) : _) =
    let (here, after) = span (startsWith seg) items
        children      = buildTree (map (first (drop 1)) here)
    in  ExportSection [HsDoc.TextContent seg] children : buildTree after
  where
    startsWith :: Text -> ([Text], a) -> Bool
    startsWith s (s' : _, _) = s == s'
    startsWith _ _           = False

{-------------------------------------------------------------------------------
  Declaration introspection
-------------------------------------------------------------------------------}

-- | Extract the user-facing Haskell name from a declaration.
--
-- Type-constructor and pattern-synonym names are always exported.  Term-level
-- names ('DForeignImport', 'DBinding') may be internal helpers, in which case
-- 'Nothing' is returned.  Instances and deriving-instances return 'Nothing'.
sdeclExportedName :: SDecl -> Maybe Text
sdeclExportedName = \case
    DTypSyn typSyn               -> Just typSyn.name.text
    DRecord record               -> Just record.typ.text
    DNewtype newtyp              -> Just newtyp.name.text
    DEmptyData empty             -> Just empty.name.text
    DForeignImport foreignImport -> termText foreignImport.name
    DBinding binding             -> termText binding.name
    DPatternSynonym patSyn       -> Just patSyn.name.text
    DCompletePragma _            -> Nothing
    DInst _                      -> Nothing
    DDerivingInstance _          -> Nothing
  where
    termText :: Hs.TermName -> Maybe Text
    termText = \case
      Hs.ExportedName n -> Just n.text
      Hs.InternalName _ -> Nothing
