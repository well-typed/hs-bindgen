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
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleTypedefs.IsPass
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
      rename :: Map (C.DeclId Select) (C.DeclId HandleTypedefs)

      -- | Typedefs that need to be squashed
      --
      -- We record what use sites of the typedef should be replaced with.
    , squash :: Map (C.DeclId Select) (C.DeclId HandleTypedefs)
    }
  deriving stock (Show, Eq)

instance Semigroup TypedefAnalysis where
  a <> b = TypedefAnalysis {
        rename = combine rename
      , squash = combine squash
      }
    where
      combine :: Ord k => (TypedefAnalysis -> Map k a) -> Map k a
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
analyseTypedef declUseGraph typedefId typedef =
   case taggedPayload typedefType of
     Nothing      -> mempty
     Just payload -> typedefOfDecl typedefId payload $
                       getUseSites payload.origDeclId
  where
    C.Typedef{typedefType, typedefAnn = NoAnn} = typedef

    getUseSites :: C.PrelimDeclId -> [(C.PrelimDeclId, Usage)]
    getUseSites qualPrelimDeclId =
       DeclUseGraph.getUseSitesNoSelfReferences declUseGraph qualPrelimDeclId

typedefOfDecl ::
     C.DeclId Select             -- ^ Typedef ID
  -> TaggedPayload               -- ^ Payload
  -> [(C.PrelimDeclId, Usage)]   -- ^ Use sites of the payload
  -> TypedefAnalysis
typedefOfDecl typedefId payload useSites
  | shouldSquash
  = let newId :: C.DeclId HandleTypedefs
        newId = C.DeclId{
            name       = typedefId.name
          , nameKind   = payload.declId.nameKind
          , origDeclId = typedefId.origDeclId
          , haskellId  = ()
          }
    in mempty{
        squash = Map.singleton typedefId      newId
      , rename = Map.singleton payload.declId newId
      }

  | shouldRename
  = let newId :: C.DeclId HandleTypedefs
        newId = C.DeclId{
            name       = typedefId.name <> "_Deref"
          , nameKind   = payload.declId.nameKind
          , origDeclId = payload.declId.origDeclId
          , haskellId  = ()
          }
    in mempty{
        rename = Map.singleton payload.declId newId
      }

  | otherwise
  = mempty
  where
    shouldSquash, shouldRename :: Bool
    shouldSquash = and [
          payload.valOrRef == ByValue
        , or [ typedefId.name == payload.declId.name
             , length useSites == 1
             ]
        ]
    shouldRename = and [
          payload.valOrRef == ByRef
        , typedefId.name == payload.declId.name
        ]

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

data TaggedPayload = TaggedPayload{
      valOrRef   :: ValOrRef
    , declId     :: C.DeclId Select
    , tagKind    :: C.TagKind
    , origDeclId :: C.PrelimDeclId
    }

-- | Tagged declaration (struct, union, enum) wrapped by this typedef, if any
taggedPayload :: C.Type Select -> Maybe TaggedPayload
taggedPayload = go ByValue
  where
    go :: ValOrRef -> C.Type Select -> Maybe TaggedPayload
    go valOrRef = \case
        C.TypeRef declId -> do
          -- We only need to worry about typedefs around tagged declarations,
          -- because only then there is a chance of name clashes (in Haskell we
          -- do not distinguish between the tagged and ordinary namespaces).
          tagKind <-
            case declId.nameKind of
              C.NameKindTagged kind -> Just kind
              C.NameKindOrdinary    -> Nothing
          -- Auxiliary declarations are not introduced until @HandleTypedefs@
          origDeclId <-
            case declId.origDeclId of
              C.OrigDeclId orig    -> Just orig
              C.AuxForDecl _parent -> panicPure "Unexpected aux decl"
          return $ TaggedPayload{
              valOrRef
            , declId
            , tagKind
            , origDeclId
            }
        C.TypePointer ty ->
          go ByRef ty
        _otherwise ->
          Nothing
