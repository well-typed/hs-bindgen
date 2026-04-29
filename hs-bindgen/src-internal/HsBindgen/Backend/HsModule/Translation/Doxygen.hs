-- | Doxygen-driven export resolution
--
-- Builds a hierarchical 'ExportEntry' tree from a flat list of 'SDecl' values
-- and a 'GroupSections' map.  Section nesting is intrinsic to the tree:
-- depth corresponds to recursion level, so the pretty-printer never has to
-- carry an explicit depth field.
module HsBindgen.Backend.HsModule.Translation.Doxygen (
    GroupSections
  , resolveExports
  ) where

import Data.List (partition)
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.HsModule.Translation (ExportEntry (..), ExportItem,
                                               defaultResolveExports,
                                               resolveDeclExports)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Frontend.AST.Decl (DeclInfo (..))
import HsBindgen.Frontend.Naming (CDeclName (..), DeclId (..), DeclIdPair (..))
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  GroupSections
-------------------------------------------------------------------------------}

-- | Mapping from declaration names to their Doxygen group title path.
--
-- Built by @computeGroupSections@ in "HsBindgen" from Doxygen XML data and
-- the final C declarations.  Keyed by both C names and Haskell names because
-- the two consumer-side lookup functions cover different 'SDecl'
-- constructors:
--
--  * __Haskell-name keys__ are needed for 'DForeignImport', 'DBinding', and
--    'DPatternSynonym', which have no 'Origin.Decl' and therefore no
--    extractable C name (see 'sdeclOriginCName').  For these constructors,
--    'sdeclExportedName' is the only lookup path.
--
--  * __C-name keys__ are needed for backend-generated declarations whose
--    Haskell name differs from the originating C name (e.g. @_Aux@
--    newtypes).  Their Haskell name is not in the map, but
--    'sdeclOriginCName' extracts the C name, which is.
--
-- Values are group title paths from root to leaf, e.g.:
--
-- > fromList
-- >   [ ("config_t",  ["Core Data Types"])
-- >   , ("Config_t",  ["Core Data Types"])
-- >   , ("inner_typ", ["Outer Group", "Inner A"])
-- >   , ("Inner_typ", ["Outer Group", "Inner A"])
-- >   ]
--
-- An empty map means no group information is available, in which case the
-- export list is produced without any section headers.
type GroupSections = Map Text [Text]

{-------------------------------------------------------------------------------
  Tagging
-------------------------------------------------------------------------------}

-- | How a declaration relates to doxygen group sections
--
-- Tagging is a two-step lookup (see 'taggedExports'):
--
--  1. Try the Haskell export name in 'GroupSections'.
--  2. If that misses, try the C origin name ('sdeclOriginCName').
--  3. If both miss, distinguish top-level C declarations ('Ungrouped') from
--     anonymous\/nested\/derived ones ('Derived').
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

{-------------------------------------------------------------------------------
  Public entry point
-------------------------------------------------------------------------------}

-- | Resolve exports for a declaration list using Doxygen group information.
--
-- When the 'GroupSections' map is empty, this is equivalent to
-- 'defaultResolveExports' (a flat list with no section headers).
--
-- When non-empty, the export list is assembled in three stages:
--
--  1. __Tag__ each declaration item as 'Ungrouped', 'Derived', or
--     'Grouped' (see 'ExportGroupTag').
--  2. __Assign paths__ to derived items by inheriting the most-recent
--     'Grouped' path.  Items appearing before any 'Grouped' get no path
--     and are hoisted alongside ungrouped items.
--  3. __Build the tree__: items with paths are folded into a recursive
--     'ExportEntry' tree where consecutive items sharing a path prefix
--     are nested under the same 'ExportSection'.  Depth is implicit in
--     the tree structure.
--
-- Example: for a header with an ungrouped @api_version_t@ and a
-- @\@defgroup core "Core"@ containing @config_t@ plus an @enum color@:
--
-- > resolveExports groups decls
-- > ==> [ ExportEntry (ExportTypeAll "Api_version_t")
-- >      , ExportSection [TextContent "Core"]
-- >          [ ExportEntry (ExportTypeAll "Config_t")
-- >          , ExportEntry (ExportTypeAll "Color")
-- >          , ExportEntry (ExportPattern "COLOR_RED")  -- derived, inherits
-- >          ]
-- >      ]
resolveExports :: GroupSections -> [SDecl] -> [ExportEntry]
resolveExports groups decls
  | Map.null groups = defaultResolveExports decls
  | otherwise =
      let assigned             = assignPaths (concatMap (taggedExports groups) decls)
          (pathless, withPath) = partition (isNothing . fst) assigned
          pathlessEntries      = [ ExportEntry item | (_, item) <- pathless ]
          treeEntries          = buildTree [ (p, item) | (Just p, item) <- withPath ]
      in  pathlessEntries ++ treeEntries

{-------------------------------------------------------------------------------
  Tag pipeline
-------------------------------------------------------------------------------}

-- | Classify a declaration and pair each of its export items with a tag.
--
-- A single 'SDecl' can produce multiple export items (e.g. a record type
-- exports both @TypeName(..)@ and its pattern synonyms).
taggedExports :: GroupSections -> SDecl -> [(ExportGroupTag, ExportItem)]
taggedExports groups decl = [ (tag, item) | item <- resolveDeclExports decl ]
  where
    lookupGroup n = Map.lookup n groups

    tag :: ExportGroupTag
    tag = case sdeclExportedName decl >>= lookupGroup of
      Just path -> Grouped path
      Nothing -> case sdeclOriginCName decl >>= lookupGroup of
        Just path -> Grouped path
        Nothing
          | sdeclIsTopLevel decl -> Ungrouped
          | otherwise            -> Derived

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

-- | Is this a non-nested C declaration?
--
-- 'True' when @declEnclosing = Nothing@ — i.e. the declaration is not
-- inside another struct\/union.  Only 'Origin.Decl'-carrying constructors
-- can be top-level; the rest ('DForeignImport', 'DBinding', etc.) return
-- 'False'.
sdeclIsTopLevel :: SDecl -> Bool
sdeclIsTopLevel = \case
    DTypSyn typSyn      -> isTopLevel typSyn.origin
    DRecord record      -> isTopLevel record.origin
    DNewtype newtyp     -> isTopLevel newtyp.origin
    DEmptyData empty    -> isTopLevel empty.origin
    DForeignImport _    -> False
    DBinding _          -> False
    DPatternSynonym _   -> False
    DInst _             -> False
    DDerivingInstance _ -> False
  where
    isTopLevel :: Origin.Decl a -> Bool
    isTopLevel (Origin.Decl DeclInfo{declEnclosing = enc} _ _) =
      isNothing enc

-- | Extract the C name from the declaration's 'Origin.Decl', if present.
--
-- Available for 'DTypSyn', 'DRecord', 'DNewtype', 'DEmptyData'.  Returns
-- 'Nothing' for the rest ('DForeignImport', 'DBinding', 'DPatternSynonym',
-- etc.) because they carry different origin types without a 'C.DeclInfo'.
--
-- Used as the fallback in 'taggedExports' when the Haskell export name
-- misses in 'GroupSections' — e.g. @Event_callback_t_Aux@ is not a key,
-- but its C origin name @event_callback_t@ is.
sdeclOriginCName :: SDecl -> Maybe Text
sdeclOriginCName = \case
    DTypSyn typSyn      -> Just $ originCName typSyn.origin
    DRecord record      -> Just $ originCName record.origin
    DNewtype newtyp     -> Just $ originCName newtyp.origin
    DEmptyData empty    -> Just $ originCName empty.origin
    DForeignImport _    -> Nothing
    DBinding _          -> Nothing
    DPatternSynonym _   -> Nothing
    DInst _             -> Nothing
    DDerivingInstance _ -> Nothing
  where
    originCName :: Origin.Decl a -> Text
    originCName (Origin.Decl DeclInfo{id = declIdPair} _ _) =
      let DeclIdPair{cName = DeclId{name = CDeclName{text = t}}} = declIdPair
      in t

-- | Extract the user-facing Haskell name from a declaration.
--
-- Returns 'Nothing' for internal names, instances, and deriving instances.
-- This is the primary lookup key in 'taggedExports'.
sdeclExportedName :: SDecl -> Maybe Text
sdeclExportedName = \case
    DTypSyn typSyn               -> exportedName typSyn.name
    DRecord record               -> exportedName record.typ
    DNewtype newtyp              -> exportedName newtyp.name
    DEmptyData empty             -> exportedName empty.name
    DForeignImport foreignImport -> exportedName foreignImport.name
    DBinding binding             -> exportedName binding.name
    DPatternSynonym patSyn       -> exportedName patSyn.name
    DInst _                      -> Nothing
    DDerivingInstance _          -> Nothing
  where
    exportedName :: Hs.Name ns -> Maybe Text
    exportedName n = case n of
      Hs.ExportedName _ -> Just (Hs.getName n)
      Hs.InternalName _ -> Nothing
