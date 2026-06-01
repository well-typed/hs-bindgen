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
  , resolveExports
  ) where

import Data.List (partition)
import Data.Map.Strict qualified as Map

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.HsModule.Translation (ExportEntry (..), ExportItem,
                                               resolveDeclExports)
import HsBindgen.Backend.SHs.AST
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

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
