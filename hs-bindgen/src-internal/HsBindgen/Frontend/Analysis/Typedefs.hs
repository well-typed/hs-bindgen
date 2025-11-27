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
import HsBindgen.Frontend.Analysis.UseDeclGraph (Usage (..), ValOrRef (..))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Select.IsPass (Select)
import HsBindgen.Imports

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
      rename :: Map C.Name (C.DeclId Select)

      -- | Typedefs that need to be squashed
      --
      -- We record what use sites of the typedef should be replaced with.
    , squash :: Map C.Name (C.Type Select)
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

fromDecls :: DeclUseGraph -> [C.Decl Select] -> TypedefAnalysis
fromDecls declUseGraph = mconcat . map aux
  where
    aux :: C.Decl Select -> TypedefAnalysis
    aux C.Decl{declInfo, declKind} =
        case declKind of
          C.DeclTypedef typedef ->
            analyseTypedef declUseGraph (C.declId declInfo) typedef
          _otherwise ->
            mempty

analyseTypedef ::
     DeclUseGraph
  -> C.DeclId Select
  -> C.Typedef Select
  -> TypedefAnalysis
analyseTypedef declUseGraph uid typedef =
    go ByValue $ C.typedefType typedef
  where
    go :: ValOrRef -> C.Type Select -> TypedefAnalysis
    go valOrRef ty | Just taggedTypeId <- toTaggedTypeId ty =
        typedefOfTagged (C.declIdName uid) valOrRef taggedTypeId $
          getUseSites (taggedTypeIdToQualPrelimDeclId taggedTypeId)
    go _ (C.TypePointer ty) =
        go ByRef ty
    go _ _otherType =
        mempty
-- typedef (struct { };) abc_syn
    -- Get use sites, except any self-references
    getUseSites :: C.QualPrelimDeclId -> [(C.QualPrelimDeclId, Usage)]
    getUseSites qualPrelimDeclId =
       DeclUseGraph.getUseSitesNoSelfReferences declUseGraph qualPrelimDeclId

-- | Typedef of some tagged datatype
typedefOfTagged ::
     C.Name                      -- ^ Name of the typedef
  -> ValOrRef                    -- ^ Does the typedef wrap the datatype directly?
  -> TaggedTypeId                -- ^ Tagged type information
  -> [(C.QualPrelimDeclId, Usage)] -- ^ All use sites of the struct
  -> TypedefAnalysis
typedefOfTagged typedefName valOrRef taggedType@TaggedTypeId{..} useSites
    -- Struct and typedef same name, no intervening pointers
  | ByValue <- valOrRef, typedefName == taggedTypeIdName taggedType
  = mempty{
        squash = Map.singleton typedefName (fromTaggedTypeId taggedType)
      }

    -- Struct and typedef same name, with intervening pointers
  | ByRef <- valOrRef, typedefName == taggedTypeIdName taggedType
  = let newDeclId = C.DeclIdNamed C.NamedDeclId{
            name   = typedefName <> "_Deref"
          , origin = updateOrigin taggedTypeDeclId
          }
    in mempty{
           rename = Map.singleton (taggedTypeIdName taggedType) newDeclId
         }

    -- Single use site of the struct
    -- (which must therefore necessarily be this very struct)
    --
    -- In this case we want to use the name of the typedef rather than the
    -- name of the struct, as normally the typedef is the main type and intended
    -- to be used throughout the code.
  | ByValue <- valOrRef, [_] <- useSites
  = let newDeclId = C.DeclIdNamed C.NamedDeclId{
            name   = typedefName
          , origin = updateOrigin taggedTypeDeclId
          }
        newTagged = TaggedTypeId{
            taggedTypeDeclId = newDeclId
          , taggedTypeIdKind
          }
    in mempty{
           squash = Map.singleton typedefName (fromTaggedTypeId newTagged)
         , rename = Map.singleton (taggedTypeIdName taggedType) newDeclId
         }

    -- if there are multiple uses, and the names don't match, don't do anything
  | otherwise
  = mempty
  where

-- | Update origin information for renamed tagged datatype
--
-- If we rename a datatype with a name which was /already/ not original, we
-- leave the origin information unchanged.
updateOrigin :: C.DeclId Select -> C.NameOrigin
updateOrigin (C.DeclIdBuiltin _name) =
    -- TODO: Ideally we'd have a separate pass to deal with builtin types,
    -- and strengthen the types in such as a way that we don't have to deal with
    -- this case here.
    -- See also <https://github.com/well-typed/hs-bindgen/issues/1266>
    panicPure "Unexpected builtin"
updateOrigin (C.DeclIdNamed old) =
    case old.origin of
      C.NameOriginInSource           -> C.NameOriginRenamedFrom old.name
      C.NameOriginGenerated   anonId -> C.NameOriginGenerated   anonId
      C.NameOriginRenamedFrom orig   -> C.NameOriginRenamedFrom orig

{-------------------------------------------------------------------------------
  TaggedTypeId

  This is only used within the context of this module.
-------------------------------------------------------------------------------}

-- | Tagged type
--
-- This is nearly identical to 'C.QualDeclId', except that we use 'C.TagKind'
-- rather than 'C.NameKind' (in other words, we rule out 'C.NameKindOrdinary').
data TaggedTypeId = TaggedTypeId {
      taggedTypeDeclId :: C.DeclId Select
    , taggedTypeIdKind :: C.TagKind
    }
  deriving stock (Eq, Generic, Ord, Show)

taggedTypeIdName :: TaggedTypeId -> C.Name
taggedTypeIdName = C.declIdName . taggedTypeDeclId

taggedTypeIdToQualPrelimDeclId :: TaggedTypeId -> C.QualPrelimDeclId
taggedTypeIdToQualPrelimDeclId (TaggedTypeId declId tk) =
    C.qualDeclIdToQualPrelimDeclId $ C.QualDeclId{
        qualDeclId     = declId
      , qualDeclIdKind = C.NameKindTagged tk
      }

toTaggedTypeId :: C.Type Select -> Maybe TaggedTypeId
toTaggedTypeId = \case
    C.TypeStruct declId -> Just $ TaggedTypeId declId C.TagKindStruct
    C.TypeUnion  declId -> Just $ TaggedTypeId declId C.TagKindUnion
    C.TypeEnum   declId -> Just $ TaggedTypeId declId C.TagKindEnum
    _otherwise          -> Nothing

fromTaggedTypeId :: TaggedTypeId -> C.Type Select
fromTaggedTypeId (TaggedTypeId declId kind) =
    case kind of
      C.TagKindStruct -> C.TypeStruct declId
      C.TagKindUnion  -> C.TypeUnion  declId
      C.TagKindEnum   -> C.TypeEnum   declId
