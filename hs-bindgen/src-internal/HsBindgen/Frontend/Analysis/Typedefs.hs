-- | Analyse typedefs
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Analysis.Typedefs (TypedefAnalysis)
-- > import HsBindgen.Frontend.Analysis.Typedefs qualified as TypedefAnalysis
module HsBindgen.Frontend.Analysis.Typedefs (
    TypedefAnalysis(..)
  , Conclusion(..)
  , Squash(..)
  , fromDecls
  ) where

import Data.Map.Strict qualified as Map

import Clang.HighLevel.Types

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclUseGraph (DeclUseGraph)
import HsBindgen.Frontend.Analysis.DeclUseGraph qualified as DeclUseGraph
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass (ResolveBindingSpecs)
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Typedef analysis
--
-- See 'Conclusion' for detailed discussion.
data TypedefAnalysis = TypedefAnalysis{
      map :: Map C.DeclId Conclusion
    }
  deriving stock (Show)

instance Semigroup TypedefAnalysis where
  a <> b = TypedefAnalysis{
        map = Map.unionWithKey unexpectedOverlap a.map b.map
      }
    where
      unexpectedOverlap :: C.DeclId -> Conclusion -> Conclusion -> Conclusion
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

data Squash = SquashTypedef {
      typedefLoc :: SingleLoc
    , targetId   :: C.DeclId
    }
  deriving stock (Eq, Show, Generic)

-- | What should we do with a particular declaration?
--
-- NOTE: The /absence/ of a conclusion in the t'TypedefAnalysis' indicates that
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
    Squash Squash

    -- | Instruct the name mangler to add a suffix to the Haskell type
    --   corresponding to this C type
    --
    -- C distinguishes between the \"tagged\" namespace and the \"ordinary\"
    -- namespace, so that there is no name clash in
    --
    -- > typedef struct foo { .. } * foo;
    --
    -- In Haskell these live in the /same/ namespace, and so we cannot assign
    -- the same name to both of these types. We also cannot /squash/ the typedef
    -- in this case, because @struct foo@ and (the typedef) @foo@ are
    -- /different/ types. We must therefore instruct the name mangler to use
    -- a different name for one of these two types; we add a suffix.
  | AddSuffix Text

    -- | Instruct the name mangler to use the name of another C declaration
    --
    -- Useful in conjunction with 'HsBindgen.Frontend.Analysis.Typedefs.Squash'. That is, the C typedef is squashed,
    -- and the squash target is assigned the name of the typedef with
    -- 'UseNameOf'.
  | UseNameOf C.DeclId
  deriving stock (Show)

conclude :: C.DeclId -> Conclusion -> TypedefAnalysis
conclude declId conclusion = TypedefAnalysis $ Map.singleton declId conclusion

{-------------------------------------------------------------------------------
  Analysis proper
-------------------------------------------------------------------------------}

fromDecls :: DeclUseGraph -> [C.Decl l ResolveBindingSpecs] -> TypedefAnalysis
fromDecls declUseGraph = mconcat . map aux
  where
     aux :: C.Decl l ResolveBindingSpecs -> TypedefAnalysis
     aux decl =
         case decl.kind of
           C.DeclTypedef typedef ->
             analyseTypedef declUseGraph decl.info typedef
           _otherwise ->
             mempty

analyseTypedef ::
      DeclUseGraph
  -> C.DeclInfo ResolveBindingSpecs
  -> C.Typedef ResolveBindingSpecs
  -> TypedefAnalysis
analyseTypedef declUseGraph typedefInfo typedef =
    case directPayload of
      -- The typedef is a direct alias of a tagged type: a candidate for
      -- squashing.
      Just payload
        | shouldSquash payload
        -> mconcat [
               conclude typedefInfo.id $ Squash $ SquashTypedef {
                   typedefLoc = typedefInfo.loc
                 , targetId   = payload.id
                 }
             , conclude payload.id $ UseNameOf typedefInfo.id
             ]

      -- Otherwise, suffix every tagged type that is referenced /within/ the
      -- typedef's own type and that would mangle to the same Haskell name as the
      -- typedef itself.
      --
      -- This is a purely /local/ decision: it depends only on this typedef's
      -- name and its own type structure, never on which other declarations
      -- happen to be in scope. We can only resolve a name clash here when the
      -- clashing tagged type is syntactically referenced by the typedef; clashes
      -- with unrelated declarations of the same name are detected (and reported)
      -- by the collision check in "HsBindgen.Frontend.Pass.MangleNames".
      _otherwise ->
        foldMap suffixClashingPayload clashingPayloads
  where
    typedefName :: Text
    typedefName = typedefInfo.id.name.text

    -- All tagged types referenced anywhere within the typedef's type.
    payloads :: [TaggedPayload]
    payloads = taggedPayloads typedef.typ

    -- The tagged type the typedef is a direct alias of (the head of the type,
    -- reached without any indirection), if any.
    directPayload :: Maybe TaggedPayload
    directPayload = case [ p | p <- payloads, p.isDirect ] of
        p : _      -> Just p
        _otherwise -> Nothing

    -- Tagged types referenced /through indirection/ within the typedef's type
    -- whose name clashes with the typedef itself.
    --
    -- A single typedef can reference the same tagged type more than once (e.g. a
    -- function-pointer typedef with several arguments of that type), so we
    -- deduplicate by 'C.DeclId': each clashing tag must yield exactly one
    -- 'AddSuffix' conclusion, otherwise the 'TypedefAnalysis' '<>' would panic on
    -- the duplicate key.
    clashingPayloads :: [TaggedPayload]
    clashingPayloads = Map.elems $ Map.fromList [
          (p.id, p)
        | p <- payloads
        , not p.isDirect
        , p.id.name.text == typedefName
        ]

    shouldSquash :: TaggedPayload -> Bool
    shouldSquash payload = or [
          payload.id.name.text == typedefName
        , length useSites == 1
        ]
      where
        useSites :: [(C.DeclId, C.ValOrRef)]
        useSites = DeclUseGraph.getUseSitesNoSelfReferences declUseGraph payload.id

    suffixClashingPayload :: TaggedPayload -> TypedefAnalysis
    suffixClashingPayload payload =
        conclude payload.id $ AddSuffix $ tagSuffix payload.id.name.kind

-- | The (deterministic) suffix used to disambiguate a tagged type from a
-- same-named typedef.
--
-- We suffix the /tagged/ type (and not the typedef) because C code refers to a
-- typedef by its bare name, but to a tagged type via its @struct@\/@union@\/
-- @enum@ keyword; the suffix mirrors that keyword.
tagSuffix :: C.NameKind -> Text
tagSuffix = \case
    C.NameKindTagged C.TagKindStruct -> "_struct"
    C.NameKindTagged C.TagKindUnion  -> "_union"
    C.NameKindTagged C.TagKindEnum   -> "_enum"
    -- Only tagged types are ever suffixed (see 'taggedPayloads'), so this is
    -- unreachable; we panic rather than invent a plausible-but-wrong suffix.
    kind -> panicPure $ "tagSuffix: unexpected non-tagged name kind " ++ show kind

{-------------------------------------------------------------------------------
  Internal auxiliary: typedefs around "tagged" decls (structs, unions, enums)

  We only need to worry about typedefs around tagged declarations, because only
  then there is a chance of name clashes (in Haskell we do not distinguish
  between the tagged and ordinary namespaces).
-------------------------------------------------------------------------------}

data TaggedPayload = TaggedPayload{
      isDirect :: Bool
    , id       :: C.DeclId
    }

-- | All tagged declarations (struct, union, enum) referenced within a type.
--
-- The 'isDirect' flag is 'True' only for a tagged type that the typedef is a
-- direct alias of, i.e. one reached without going through any indirection
-- (pointer, array, function, block). A qualifier (such as @const@) is
-- transparent: it changes neither the Haskell representation nor the mangled
-- name, so a tagged type underneath it stays direct.
--
-- Any layer of indirection means the tagged type and the typedef are
-- /different/ types that nonetheless mangle to the same Haskell name, so the
-- tagged type must be given a suffix. This covers
--
-- > typedef struct {..} * foo;
-- > typedef struct {..} foo[10];
-- > typedef struct foo (*foo)(struct bar arg);
-- > typedef const struct foo * foo;
--
-- See <https://github.com/well-typed/hs-bindgen/issues/1445>. Note that we
-- recurse into /function/ types (both the result and the argument types), so a
-- tagged type referenced there is detected as well.
taggedPayloads :: C.Type ResolveBindingSpecs -> [TaggedPayload]
taggedPayloads = go True
  where
    go :: Bool -> C.Type ResolveBindingSpecs -> [TaggedPayload]
    go direct = \case
        C.TypeRef declId         -> typeRef direct declId
        C.TypeEnum ref           -> typeRef direct ref.name

        -- A qualifier (e.g. @const@) is transparent: it changes neither the
        -- Haskell representation nor the mangled name, so the tagged type
        -- underneath is reached with the /same/ directness.
        C.TypeQual _qual ty      -> go direct ty

        -- Indirection: the tagged type underneath is a /different/ type that
        -- merely mangles to the same name, so it is no longer direct.
        C.TypePointers _n ty     -> go False ty
        C.TypeConstArray _n ty   -> go False ty
        C.TypeIncompleteArray ty -> go False ty
        C.TypeBlock ty           -> go False ty
        C.TypeFun args res       ->
          concatMap (go False . (.typ)) args ++ go False res

        -- No tagged type to find.
        C.TypePrim{}             -> []
        C.TypeComplex{}          -> []
        C.TypeVoid               -> []

        -- A macro type and another typedef each have their own name; the tagged
        -- type they may wrap is not /syntactically/ referenced by this typedef,
        -- so resolving such a clash is out of scope for this local analysis.
        C.TypeMacro{}            -> []
        C.TypeTypedef{}          -> []
        C.TypeExtBinding{}       -> []

    typeRef :: Bool -> C.DeclId -> [TaggedPayload]
    typeRef isDirect declId =
        case C.checkIsTagged declId.name.kind of
          Nothing -> []
          Just _  -> [TaggedPayload{isDirect = isDirect, id = declId}]
