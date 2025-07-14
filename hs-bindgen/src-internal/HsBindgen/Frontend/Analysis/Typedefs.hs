-- | Analyse typedefs
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
-- > import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
module HsBindgen.Frontend.Analysis.Typedefs (
    TypedefAnalysis(..)
  , fromDecls
  ) where

import Data.Map.Strict qualified as Map

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Analysis.UseDeclGraph (Usage(..), ValOrRef(..))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.Parse.Type.PrelimDeclId
import HsBindgen.Frontend.Pass.ResolveBindingSpec.IsPass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass (HandleTypedefs)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Analyzed typedefs
--
-- == Context
--
-- The relevant passes of the frontend, in order, are
--
-- * Assign names to anonymous declarations
--   (so that binding specifications can refer to them)
-- * Resolve external and prescriptive binding specifications
--   (so that they can override which typedefs we squash and which we don't)
-- * Handle typedefs
--   (assisted by the analysis in this module)
--
-- == Motivation
--
-- The goal of this analysis is two-fold:
--
-- 1. Avoid name clashes.
--
--    C uses a different namespace for "Tags" and for "Ordinary" names. This
--    means that the following code is perfectly fine:
--
--    > typedef struct game_state *game_state;
--
--    It is however important that we do not use the same name (\"Game_state\")
--    in Haskel for both @struct game_state@ and the (typedef) @game_state@.
--
-- 2. Avoid unnecessary newtypes in Haskell.
--
--    Many typedefs around structs (or unions/enums) are there for syntactic
--    convenience only:
--
--    > typedef struct foo {..} foo;
--
--    In this case it is not necessary to create two separate Haskell types
--    for @struct foo@ and (the typedef @foo@; indeed, doing so would result in
--    less usable bindings.
--
-- These two points are related: if we don't generate two separate types, there
-- cannot be any name clashes.
--
-- == Approach
--
-- We do not generate a separate Haskell type for the C @typedef@ when the
-- typedef wraps the struct /directly/, such as
--
-- > struct foo {..};
-- > typedef struct foo foo_t;
--
-- or
--
-- > typedef struct foo { .. } foo_t;
--
-- or
--
-- > struct foo; // opaque
-- > typedef struct foo foo_t;
--
-- but /not/
--
-- > struct foo {..};
-- > typedef struct foo *foo_t; // intervening pointer
--
-- Provided that one of the following two conditions is satisfied:
--
-- * The struct and the typedef have the same name, or
-- * The /only/ use of the struct is in the typedef (or recursively in the
--   definition of the struct itself).
--
-- This leaves one possible name clash: a typedef which is a pointer to a struct
-- of the same name:
--
-- > typedef struct foo {} *foo;
--
-- There is no obvious solution here. If the struct would have been anonymous
--
-- > typedef struct {} *foo;
--
-- we would have assigned the name @foo_Deref@; for consistency we do the same
-- thing also in the case of this name clash.
data TypedefAnalysis = TypedefAnalysis {
      -- | Declarations (structs, unions, or enums) that need to be renamed
      rename :: Map C.Name (C.Name, C.NameOrigin)

      -- | Typedefs that need to be squashed
      --
      -- We record what use sites of the typedef should be replaced with.
    , squash :: Map C.Name (C.Type HandleTypedefs)
    }
  deriving stock (Show, Eq)

instance Semigroup TypedefAnalysis where
  a <> b = TypedefAnalysis {
        rename = combine rename
      , squash = combine squash
      }
    where
      combine :: (TypedefAnalysis -> Map C.Name a) -> Map C.Name a
      combine f =
          Map.unionWith
            (panicPure "TypedefAnalysis: unexpected overlap")
            (f a)
            (f b)

instance Monoid TypedefAnalysis where
  mempty = TypedefAnalysis {
        rename = Map.empty
      , squash = Map.empty
      }

{-------------------------------------------------------------------------------
  Analysis proper
-------------------------------------------------------------------------------}

fromDecls :: DeclUseGraph -> [C.Decl ResolveBindingSpec] -> TypedefAnalysis
fromDecls declUseGraph = mconcat . map aux
  where
    aux :: C.Decl ResolveBindingSpec -> TypedefAnalysis
    aux C.Decl{declInfo, declKind} =
        case declKind of
          C.DeclTypedef typedef ->
            analyseTypedef declUseGraph (C.declId declInfo) typedef
          _otherwise ->
            mempty

analyseTypedef ::
     DeclUseGraph
  -> C.Name
  -> C.Typedef ResolveBindingSpec
  -> TypedefAnalysis
analyseTypedef declUseGraph typedefName typedef =
    go ByValue $ C.typedefType typedef
  where
    go :: ValOrRef -> C.Type ResolveBindingSpec -> TypedefAnalysis
    go valOrRef ty | Just taggedType <- toTaggedType ty =
        typedefOfTagged typedefName valOrRef taggedType $
          getUseSites (origQualId taggedType)
    go _ (C.TypePointer ty) =
        go ByRef ty
    go _ _otherType =
        mempty

    -- Get use sites, except any self-references
    getUseSites :: QualPrelimDeclId -> [(QualPrelimDeclId, Usage)]
    getUseSites qid =
        let allUseSites = DeclUseGraph.getUseSites declUseGraph qid
        in filter (not . isSelfReference) allUseSites
      where
        isSelfReference :: (QualPrelimDeclId, Usage) -> Bool
        isSelfReference (qid', _usage) = qid == qid'

-- | Typedef of some tagged datatype
typedefOfTagged ::
     C.Name                      -- ^ Name of the typedef
  -> ValOrRef                    -- ^ Does the typedef wrap the datatype directly?
  -> TaggedType                  -- ^ Tagged datatype
  -> [(QualPrelimDeclId, Usage)] -- ^ All use sites of the struct
  -> TypedefAnalysis
typedefOfTagged typedefName valOrRef taggedType@TaggedType{..} useSites
    -- Struct and typedef same name, no intervening pointers
  | ByValue <- valOrRef, typedefName == taggedName
  = mempty{
        squash = Map.singleton typedefName (fromTaggedType taggedType)
      }

    -- Struct and typedef same name, with intervening pointers
  | ByRef <- valOrRef, typedefName == taggedName
  = let newName   = typedefName <> "_Deref"
        newOrigin = updateOrigin taggedName taggedOrigin
    in mempty{
           rename = Map.singleton taggedName (newName, newOrigin)
         }

    -- Single use site of the struct
    -- (which must therefore necessarily be this very struct)
    --
    -- In this case we want to use the name of the typedef rather than the
    -- name of the struct, as normally the typedef is the main type and intended
    -- to be used throughout the code.
  | ByValue <- valOrRef, [_] <- useSites
  = let newName   = typedefName
        newOrigin = updateOrigin taggedName taggedOrigin
        newTagged = TaggedType{
                        taggedName   = newName
                      , taggedOrigin = newOrigin
                      , taggedKind
                      }
    in mempty{
           squash = Map.singleton typedefName (fromTaggedType newTagged)
         , rename = Map.singleton taggedName (newName, newOrigin)
         }

    -- if there are multiple uses, and the names don't match, don't do anything
  | otherwise
  = mempty

-- | Update origin information for renamed tagged datatype
--
-- If we rename a datatype with a name which was /already/ not original, we
-- leave the origin information unchanged.
updateOrigin :: C.Name -> C.NameOrigin -> C.NameOrigin
updateOrigin oldName = \case
    C.NameOriginInSource           -> C.NameOriginRenamedFrom oldName
    C.NameOriginGenerated   anonId -> C.NameOriginGenerated   anonId
    C.NameOriginRenamedFrom orig   -> C.NameOriginRenamedFrom orig

{-------------------------------------------------------------------------------
  Internal auxiliary: tagged types

  TODO: Should we use this elsewhere also?
-------------------------------------------------------------------------------}

data TaggedType = TaggedType {
      taggedKind   :: TaggedKind
    , taggedName   :: C.Name
    , taggedOrigin :: C.NameOrigin
    }

data TaggedKind = Struct | Union | Enum

toTaggedType :: C.Type ResolveBindingSpec -> Maybe TaggedType
toTaggedType = \case
    C.TypeStruct name origin -> Just $ TaggedType Struct name origin
    C.TypeUnion  name origin -> Just $ TaggedType Union  name origin
    C.TypeEnum   name origin -> Just $ TaggedType Enum   name origin
    _otherwise               -> Nothing

fromTaggedType :: TaggedType -> C.Type HandleTypedefs
fromTaggedType TaggedType{..} =
    case taggedKind of
      Struct -> C.TypeStruct taggedName taggedOrigin
      Union  -> C.TypeUnion  taggedName taggedOrigin
      Enum   -> C.TypeEnum   taggedName taggedOrigin

taggedNameKind :: TaggedKind -> C.NameKind
taggedNameKind = \case
    Struct -> C.NameKindStruct
    Union  -> C.NameKindUnion
    Enum   -> C.NameKindEnum

origQualId :: TaggedType -> QualPrelimDeclId
origQualId TaggedType{..} =
    QualPrelimDeclId
      (origPrelimDeclId taggedName taggedOrigin)
      (taggedNameKind taggedKind)

{-------------------------------------------------------------------------------
  Internal auxiliary: restore original names

  This allows us to look up the type in the 'DeclUseGraph'.
  TODO: Maybe this should live somewhere more general?
-------------------------------------------------------------------------------}

origPrelimDeclId :: C.Name -> C.NameOrigin -> PrelimDeclId
origPrelimDeclId name = \case
    C.NameOriginInSource           -> PrelimDeclIdNamed name
    C.NameOriginRenamedFrom orig   -> PrelimDeclIdNamed orig
    C.NameOriginGenerated   anonId -> PrelimDeclIdAnon  anonId
