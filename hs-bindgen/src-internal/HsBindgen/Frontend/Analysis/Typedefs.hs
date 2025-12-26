{-# LANGUAGE NoFieldSelectors  #-}
{-# LANGUAGE NoRecordWildCards #-}

-- | Analyse typedefs
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
-- > import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
module HsBindgen.Frontend.Analysis.Typedefs (
    TypedefAnalysis(..)
  , Conclusion(..)
  , Rename(..)
  , fromDecls
  ) where

import Data.Map.Strict qualified as Map

import Clang.HighLevel.Types

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Select.IsPass (Select)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Typedef analysis
--
-- See 'Conclusion' for detailed discussion.
data TypedefAnalysis = TypedefAnalysis{
      map :: Map DeclId Conclusion
    }
  deriving stock (Show)

instance Semigroup TypedefAnalysis where
  a <> b = TypedefAnalysis{
        map = Map.unionWithKey unexpectedOverlap a.map b.map
      }
    where
      unexpectedOverlap :: DeclId -> Conclusion -> Conclusion -> Conclusion
      unexpectedOverlap declId conclusion1 conclusion2 =
          panicPure $ concat [
              "Unexpected overlap for "
            , show declId
            , ": "
            , show (conclusion1, conclusion2)
            ]

instance Monoid TypedefAnalysis where
  mempty = TypedefAnalysis{
        map = Map.empty
      }

-- | What should we do with a particular declaration?
--
-- NOTE: The /absence/ of a conclusion in the 'TypedefAnalysis' indicates that
-- we should leave the declaration as-is.
data Conclusion =
    -- | Squash typedef
    --
    -- Squashing means removing the declaration of the typedef from the list of
    -- declarations altogether, so that we do not generate a Haskell datatype
    -- for it. We squash typedefs if
    --
    -- 1. We have a typedef around a declaration of the same name
    --
    --    > typedef struct foo { .. } foo;
    --
    --    This helps avoid name clashes in Haskell.
    --
    -- 2. The typedef is the /only/ reference to the underlying decl:
    --
    --    > typedef struct foo { .. } foo_t;
    --
    --    In this case @foo_t@ is just a syntactic convenience for @struct foo@,
    --    rather than indicating some kind of semantic difference. Since the
    --    @struct@ prefix is /anyway/ not present in Haskell, that syntactic
    --    convenience is irrelevant there and we squash this typedef also.
    --
    -- The name mangler must be instructed to use the name of the typedef for
    -- the struct; this after all is how C code will most likely refer to this
    -- struct. /If/ the only two cases for squashing are the ones listed above,
    -- this would automatically take care of use sites: assigning the name of
    -- the typedef to the struct (in Haskell) would make no difference if they
    -- have the same name, and would only affect the typedef itself if the names
    -- are different, because in that case that is the /only/ use of the struct.
    -- However, if in the future we make squashing configurable, and for example
    -- make it possible to squash @foo_t@ in
    --
    -- > typedef struct foo { .. } foo_t;
    -- > void f(struct foo x);
    --
    -- then use sites would also need to be handled explicitly; see also
    -- <https://github.com/well-typed/hs-bindgen/issues/1356>.
    Squash SingleLoc DeclId

    -- | Rename the Haskell type corresponding to this C type
    --
    -- C distinguishes between the \"tagged\" namespace and thet \"ordinary\"
    -- namespace, so that there is no name clash in
    --
    -- > typedef struct foo { .. } * foo;
    --
    -- In Haskell these live in the /same/ namespace, and so we cannot assign
    -- the same name to both of these types. We also cannot /squash/ the typedef
    -- in this case, because @struct foo@ and (the typedef) @foo@ are
    -- /different/ types. We must therefore instruct the name mangler to use
    -- a different name for one of these two types, we add a suffix.
    --
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/1432>
    -- These suffixes could result in name clashes.
  | Rename Rename
  deriving stock (Show)

data Rename =
    AddSuffix Hs.Identifier
  | UseNameOf DeclId
  deriving stock (Show)

conclude :: DeclId -> Conclusion -> TypedefAnalysis
conclude declId conclusion = TypedefAnalysis $ Map.singleton declId conclusion

{-------------------------------------------------------------------------------
  Analysis proper
-------------------------------------------------------------------------------}

fromDecls :: DeclUseGraph -> [C.Decl Select] -> TypedefAnalysis
fromDecls declUseGraph = mconcat . map aux
  where
     aux :: C.Decl Select -> TypedefAnalysis
     aux decl =
         case decl.declKind of
           C.DeclTypedef typedef ->
             analyseTypedef declUseGraph decl.declInfo typedef
           _otherwise ->
             mempty

analyseTypedef ::
      DeclUseGraph
  -> C.DeclInfo Select
  -> C.Typedef Select
  -> TypedefAnalysis
analyseTypedef declUseGraph typedefInfo typedef =
    case taggedPayload typedefType of
      Nothing      -> mempty
      Just payload ->
        typedefOfTagged typedefInfo payload $
          DeclUseGraph.getUseSitesNoSelfReferences declUseGraph payload.declId
  where
    C.Typedef{typedefType, typedefAnn = NoAnn} = typedef

-- | Typedef around tagged payload
typedefOfTagged ::
     C.DeclInfo Select    -- ^ Typedef info
  -> TaggedPayload        -- ^ Payload
  -> [(DeclId, usage)]  -- ^ Use sites of the payload
  -> TypedefAnalysis
typedefOfTagged typedefInfo payload useSites
  | shouldSquash
  = mconcat [
        conclude typedefInfo.declId $ Squash typedefInfo.declLoc payload.declId
      , conclude payload.declId $ Rename (UseNameOf typedefInfo.declId)
      ]

    -- TODO <https://github.com/well-typed/hs-bindgen/issues/1427>
    -- Use "_Aux" instead.
  | shouldRename
  = conclude payload.declId $ Rename (AddSuffix "_Deref")

  | otherwise
  = mempty
  where
    shouldSquash, shouldRename :: Bool
    shouldSquash = and [
          payload.isDirect
        , or [ typedefInfo.declId.name.text == payload.declId.name.text
             , length useSites == 1
             ]
        ]
    shouldRename = and [
          not payload.isDirect
        , typedefInfo.declId.name.text == payload.declId.name.text
        ]

{-------------------------------------------------------------------------------
  Internal auxiliary: typedefs around "tagged" decls (structs, unions, enums)

  We only need to worry about typedefs around tagged declarations, because only
  then there is a chance of name clashes (in Haskell we do not distinguish
  between the tagged and ordinary namespaces).
-------------------------------------------------------------------------------}

data TaggedPayload = TaggedPayload{
      isDirect :: Bool
    , declId   :: DeclId
    }

-- | Tagged declaration (struct, union, enum) wrapped by this typedef, if any
taggedPayload :: C.Type Select -> Maybe TaggedPayload
taggedPayload = go True
  where
    go :: Bool -> C.Type Select -> Maybe TaggedPayload
    go direct = \case
        C.TypeRef declId     -> typeRef direct declId
        C.TypePointers _n ty -> go False ty
        _otherwise ->
          -- TODO: <https://github.com/well-typed/hs-bindgen/issues/1445>
          -- This is wrong. This deals with
          --
          -- > typedef struct {..} * foo;
          --
          -- but not, for exmaple
          --
          -- > typedef struct {..} foo[10];
          Nothing

    typeRef :: Bool -> DeclId -> Maybe TaggedPayload
    typeRef isDirect declId = do
        void $ C.checkIsTagged declId.name.kind
        return TaggedPayload{isDirect, declId}
