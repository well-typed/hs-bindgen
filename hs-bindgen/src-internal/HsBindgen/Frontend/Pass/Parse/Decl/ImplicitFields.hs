-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Frontend.Pass.Parse.Decl.ImplicitFields as IFields
module HsBindgen.Frontend.Pass.Parse.Decl.ImplicitFields (
    -- * Inputs
    Inputs
  , inputEmpty
  , inputField
  , inputDecl
    -- * Outputs
  , Outputs (..)
    -- * Top-level
  , EnclosingObject(..)
  , MakeImplicitField
  , withImplicitFields
  ) where

import Control.Exception (try)
import Control.Monad.Except (ExceptT (..), MonadError (throwError), liftEither,
                             runExceptT)
import Data.Either (partitionEithers)
import Data.Foldable (minimumBy)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import GHC.Records (HasField (getField))

import Clang.HighLevel.Types (SingleLoc)
import Clang.LowLevel.Core (CXType, CallFailed, clang_Type_getOffsetOf)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Deps (depsOfDecl, depsOfField)
import HsBindgen.Frontend.AST.Type (ValOrRef (..))
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming (CScopedName (CScopedName, text))
import HsBindgen.Frontend.Pass (IsPass (ScopedName))
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse, ReparseInfo (..))
import HsBindgen.Frontend.Pass.Parse.IsPass qualified as Origin (ExplicitFieldOrigin (..),
                                                                 FieldOrigin (..),
                                                                 ImplicitFieldOrigin (..))
import HsBindgen.Frontend.Pass.Parse.Msg (ParseImplicitFieldsMsg (..))
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId (Named))
import HsBindgen.Imports (Bifunctor (bimap), MonadIO (..), NonEmpty, Text)

{-------------------------------------------------------------------------------
  Inputs
-------------------------------------------------------------------------------}

-- | A list of nested struct\/union declarations and field declarations
newtype Inputs field = Inputs {
    nestedDecls :: [Either (C.Decl Parse) (field Parse)]
  }
  deriving newtype (Semigroup, Monoid)

inputEmpty :: Inputs field
inputEmpty = Inputs []

inputField :: field Parse -> Inputs field
inputField x = Inputs [Right x]

inputDecl :: C.Decl Parse -> Inputs field
inputDecl x = Inputs [Left x]

{-------------------------------------------------------------------------------
  Outputs
-------------------------------------------------------------------------------}

data Outputs field =
    -- | An exception occurred
    OutputFail {
        exception :: ParseImplicitFieldsMsg
      }
    -- | All explicit and implicit fields
  | OutputSuccess {
        fields    :: [field Parse]
      }

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Detect implicit fields and generate entries in the C AST for them
--
-- PRECONDITION: the inputs must include all nested struct\/union\/field
-- declarations that are present in the C source. If any failed to parse and are
-- not included, then it is unsafe to use 'withImplicitFields'.
--
-- Unnamed bit-fields, used to specify padding, are already filtered out.
--
-- === Algorithm description
--
-- Implicit fields for nested anonymous structs and unions are not reported by
-- @libclang@, so we detect them instead.
--
-- Given an enclosing object struct\/union $E$, and a nested anonymous
-- struct\/union $A$, the goal is to compute the offset of the (hidden) implicit
-- field of $E$ that reference $A$. The key observation is that the offset to
-- that implicit field is equal to the offset to any of $A$'s fields (let's say
-- $F$), subtracted by the offset to $F$ with respect to $A$.
--
-- Let's walk through an example to make this more concrete. We assume that
-- @int@s are 4 bytes.
--
-- > struct E {
-- >   int x;
-- >   struct A {
-- >     int : 3;
-- >     int y;
-- >   };
-- > };
--
-- The offsets that are reported by @libclang@ are as follows:
--
-- * @offsetOf(E, x) = 0@
-- * @offsetOf(A, y) = 4@
-- * @offsetOf(E, y) = 12@
--
-- We can ask @libclang@ for the offset of $y$ with respect to $E$ because $y$
-- is considered an indirect field of $E$. It can be accessed as if it were a
-- true field of $E$ itself. We can not ask for the offset of $A$ with respect
-- to $E$, unfortunately, so we have to compute it ourselves instead.
--
-- * @offsetOf(E, A) = offsetOf(E, y) - offsetOf(A, y) = 12 - 4 = 8@
--
-- The compiler implementation is not allowed to include padding before the
-- first field of a struct\/union, but it is free to include padding between
-- fields. For that reason, we could not have used the /size/ of individual
-- fields to compute @offsetOf(E, A)@. In the example, $x$ has a size of $4$
-- bytes, while the offset to $A$ is $8$ bytes because the "compiler" has
-- inserted $4$ padding bytes between the $x$ and $A$. In practice, there will
-- probably be no padding in this case, but there could be and we have to design
-- the algorithm with that assumption in mind.
--
-- Using the computed offset, we generate a so-called implicit field and we
-- include it as a struct\/union field in the C AST. Haskell bindings are
-- generated for such implicit fields like for any other explicit field.
--
-- The implicit field detection algorithm is the same for any nesting of structs
-- or unions, in any order, even recursively.
--
-- The implicit field detection algorithm does rely on one condition: the
-- anonymous nested struct or union should have at least one named field. In
-- other words, the anonymous nested struct\/union should be "non-empty". A
-- struct\/union with only unnamed bit-fields, used to specify padding, is also
-- considered empty. A warning-level trace message will be emitted if these
-- conditions are not met.
--
-- Anonymous nested structs/unions have no name, but they need one for our Haskell
-- bindings, so they are named after their first field. Informally, the former will
-- be transformed to the latter:
--
-- > struct S {
-- >   struct {
-- >     int x;
-- >   };
-- > };
--
-- > struct S {
-- >   struct {
-- >     int x;
-- >   } x;
-- > };
--
withImplicitFields ::
     forall m field. (
       MonadIO m
     , MakeImplicitField field
     , HasField "typ" (field Parse) (C.Type Parse)
     )
  => EnclosingObject
  -> Inputs field
  -> m (Outputs field)
withImplicitFields encObj inputs = do
    resultsE <- runM $ mapM getImplicitField' classifications.candidates
    case resultsE of
      Left e -> pure OutputFail {
          exception = e
        }
      Right implicitFields -> pure OutputSuccess {
          fields =
              fmap (.numberee)
            $ sortOn (.number)
            $ classifications.explicitFields ++ implicitFields
        }
  where
    classifications = classifyInputs inputs

    getImplicitField' ::
        Numbered (C.Decl Parse)
      -> M m (Numbered (field Parse))
    getImplicitField' decl =
        Numbered decl.number <$> getImplicitField encObj decl.numberee

{-------------------------------------------------------------------------------
  Inputs classification
-------------------------------------------------------------------------------}

data Classification field = Classification {
    -- | Parsed explicit fields
    explicitFields :: [Numbered (field Parse)]
    -- | Candidates for implicit fields
  , candidates     :: [Numbered (C.Decl Parse)]
  }

-- | Declarations are numbered so that we can sort them in the order that they
-- were parsed
data Numbered a = Numbered {
    number   :: Int
  , numberee :: a
  }
  deriving stock (Functor, Foldable, Traversable)

classifyInputs ::
     forall field. HasField "typ" (field Parse) (C.Type Parse)
  => Inputs field
  -> Classification field
classifyInputs inputs = Classification {
      explicitFields = explicitFields
    , candidates = candidates
    }
  where
    -- | Number all the declarations so that we can re-sort them at the end of
    -- the algorithm
    membersNumbered ::
      [Numbered (Either (C.Decl Parse) (field Parse))]
    membersNumbered = zipWith Numbered [0..] inputs.nestedDecls
    nestedDecls :: [Numbered (C.Decl Parse)]
    explicitFields :: [Numbered (field Parse)]
    (nestedDecls, explicitFields) = partitionEithers $ fmap numberedIn membersNumbered

    -- | A struct\/union declaration requires an implicit field if the
    -- declaration is anonymous
    candidates :: [Numbered (C.Decl Parse)]
    candidates = [
          decl
        | let decls = fmap (.numberee) nestedDecls
              fields = fmap (.numberee) explicitFields
        , decl <- nestedDecls
        , isAnonymous decl.numberee fields decls
        ]

numberedIn :: Numbered (Either a b) -> Either (Numbered a) (Numbered b)
numberedIn x = bimap (Numbered x.number) (Numbered x.number) x.numberee

-- | Check if a target declaration is anonymous with respect to an enclosing
-- object.
--
-- We achieve this by checking whether the target declaration is referenced by
-- any of the directly declared fields of the enclosing object, or is referenced
-- by fields of any recursively nested struct and union declarations including
-- implicit fields that we generated previously.
isAnonymous ::
     HasField "typ" (field Parse) (C.Type Parse)
     -- | Target declaration
  => C.Decl Parse
     -- | Fields that are declared directly in the enclosing object
  -> [field Parse]
     -- | Struct and union declarations (recursively) nested in the enclosing
     -- object
  -> [C.Decl Parse] -> Bool
isAnonymous decl fields decls
  | Named{} <- decl.info.id
  = False
  | (ByValue, decl.info.id) `elem` deps
  = False
  | (ByRef, decl.info.id) `elem` deps
  = False
  | otherwise
  = True
  where
    deps = concatMap (\d -> depsOfDecl d.kind) decls ++
           concatMap depsOfField fields

{-------------------------------------------------------------------------------
  Implicit field
-------------------------------------------------------------------------------}

-- | Generate an implicit field for an anonymous object
getImplicitField ::
     forall m field. (
      MonadIO m
    , MakeImplicitField field
    )
     -- | The enclosing object
  => EnclosingObject
     -- | An anonymous object nested in the enclosing object
  -> C.Decl Parse
  -> M m (field Parse)
getImplicitField encObj decl = do
    targetsNE <- liftEither $ checkNonEmpty targets
    offsets <- mapM offsetOf' targetsNE
    -- The offset to the implicit field is equal to the offset to any of the
    -- nested object's fields, subtracted by the offset of that same field with
    -- respect to the nested object.
    --
    -- We do not assume that the fields are ordered in any way, hence we use
    -- 'minimumBy' to determine which /named/ field is the first. We need to
    -- know which field is first so that we can use that field's name as the
    -- implicit field's name as well.
    let (target, offset) = minimumBy (\x y -> compare (snd x) (snd y)) offsets
        offset' = offset - target.fieldOffset
    makeImplicitFieldM
      decl.info.loc
      (mkName target)
      (C.TypeRef decl.info.id)
      offset'
      (mkOrigin target)
  where
    targets :: [Target]
    targets = case decl.kind of
        C.DeclStruct struct -> map getOffsetOfTarget struct.fields
        C.DeclUnion  union  -> map getOffsetOfTarget union.fields
        _                   -> []

    checkNonEmpty :: [Target] -> Either ParseImplicitFieldsMsg (NonEmpty Target)
    checkNonEmpty xs = case NonEmpty.nonEmpty xs of
        Nothing -> Left UnsupportedEmptyAnon
        Just ys -> Right ys

    offsetOf' :: Target -> M m (Target, FieldOffset)
    offsetOf' target = do
        offset <- offsetOf encObj target.originName
        pure (target, offset)

    mkOrigin ::
         Target
      -> Origin.ImplicitFieldOrigin
    mkOrigin target = Origin.ImplicitFieldOrigin encObj.typ (CScopedName target.originName.text)

    mkName :: Target -> ScopedName Parse
    mkName target = CScopedName target.fieldName.text

-- | When the field is implicit and we want to ask for its offset using its
-- name, then we should ask for the offset to an explicit field of the
-- referenced anonymous object instead.
--
-- Implicit fields are generated by @hs-bindgen@ and therefore not present in
-- the header file. As such, when 'offsetOf' is used to ask @libclang@ what the
-- offset of a field is with respect to an enclosing object, then we have to be
-- careful with implicit field names. @libclang@ won't recognise the implicit
-- field names.
--
getOffsetOfTarget ::
     IsField field
  => field Parse
  -> Target
getOffsetOfTarget (Field -> field) =
    Target {
        fieldName = FieldName field.info.name.text
      , originName = FieldName $ case snd field.ann of
          Origin.ExplicitParsed Origin.ExplicitFieldOrigin
            -> field.info.name.text
          Origin.ImplicitGenerated origin
            -> origin.field.text
      , fieldOffset = FieldOffset $ field.offset
      }

data Target = Target {
      fieldName  :: FieldName
    , originName :: FieldName
    , fieldOffset :: FieldOffset
    }

{-------------------------------------------------------------------------------
  Field
-------------------------------------------------------------------------------}

class ( HasField "ann" (Field field) (ReparseInfo, Origin.FieldOrigin)
      , HasField "info" (Field field) (C.FieldInfo Parse)
      , HasField "width" (Field field) (Maybe Int)
      , HasField "offset" (Field field) Int
      )
   => IsField field

instance IsField C.StructField
instance IsField C.UnionField

newtype Field field = Field { unwrap :: field Parse }

instance HasField "ann" (Field C.StructField) (ReparseInfo, Origin.FieldOrigin) where
  getField x = getField @"ann" x.unwrap

instance HasField "info" (Field C.StructField) (C.FieldInfo Parse) where
  getField x = getField @"info" x.unwrap

instance HasField "width" (Field C.StructField) (Maybe Int) where
  getField x = getField @"width" x.unwrap

instance HasField "offset" (Field C.StructField) Int where
  getField x = getField @"offset" x.unwrap

instance HasField "ann" (Field C.UnionField) (ReparseInfo, Origin.FieldOrigin) where
  getField x = getField @"ann" x.unwrap

instance HasField "info" (Field C.UnionField) (C.FieldInfo Parse) where
  getField x = getField @"info" x.unwrap

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>
-- Once bit-fields are supported in union fields, then we can implement this
-- instance properly
instance HasField "width" (Field C.UnionField) (Maybe Int) where
  getField _ = Nothing

-- offsets for union fields are always 0
instance HasField "offset" (Field C.UnionField) Int where
  getField _ = 0

{-------------------------------------------------------------------------------
  Field offset
-------------------------------------------------------------------------------}

newtype EnclosingObject = EnclosingObject { typ :: CXType }
  deriving stock Show

newtype FieldName = FieldName { text :: Text }
  deriving stock (Show, Eq, Ord)

newtype FieldOffset = FieldOffset { int :: Int }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num)

-- | Get the offset of a named field with respect to an enclosing object.
--
-- If the named field is *not* an (indirect) field of the enclosing object, then
-- an 'UnexpectedClangOffsetOfException' exception is returned. Otherwise, a
-- 'FieldOffset' is returned.
offsetOf ::
     MonadIO m
  => EnclosingObject
  -> FieldName
  -> M m FieldOffset
offsetOf encObj name = do
    offsetE <- liftIO $ try @CallFailed $ clang_Type_getOffsetOf encObj.typ fieldName
    case offsetE of
      Left e
        -> throwError (UnexpectedClangOffsetOfException name.text (show e))
      Right offset
        -> pure (FieldOffset (fromIntegral offset))
  where
    fieldName = Text.unpack name.text


{-------------------------------------------------------------------------------
  MakeImplicitField
-------------------------------------------------------------------------------}

-- | Create an implicit field monadically
makeImplicitFieldM ::
     (Monad m, MakeImplicitField field)
  => SingleLoc                  -- ^ Field location
  -> ScopedName Parse           -- ^ Field name
  -> C.Type Parse               -- ^ Field type
  -> FieldOffset                -- ^ Field offset
  -> Origin.ImplicitFieldOrigin -- ^ Field origin
  -> M m (field Parse)
makeImplicitFieldM loc name typ off orig = do
    let field = makeImplicitField loc name typ off orig
    case field of
      Left e -> throwError e
      Right x -> pure x

-- | A common interface to creating an implicit field for a struct or union
-- object.
class MakeImplicitField field where
  -- | Create an implicit field
  makeImplicitField ::
       SingleLoc                  -- ^ Field location
    -> ScopedName Parse           -- ^ Field name
    -> C.Type Parse               -- ^ Field type
    -> FieldOffset                -- ^ Field offset
    -> Origin.ImplicitFieldOrigin -- ^ Field origin
    -> Either ParseImplicitFieldsMsg (field Parse)

instance MakeImplicitField C.StructField where
  makeImplicitField loc name typ off orig =
      Right C.StructField {
          info = C.FieldInfo {
              loc = loc
            , name = name
            , comment = Nothing
            }
        , typ = typ
        , offset = off.int
        , width = Nothing
        , ann = (ReparseNotNeeded, Origin.ImplicitGenerated orig)
        }

instance MakeImplicitField C.UnionField where
  makeImplicitField loc name typ off orig
    | off.int /= 0
    = Left (UnexpectedNonZeroFieldOffset off.int)
    | otherwise
    = Right C.UnionField {
          info = C.FieldInfo {
              loc = loc
            , name = name
            , comment = Nothing
            }
        , typ = typ
        , ann = (ReparseNotNeeded, Origin.ImplicitGenerated orig)
        }

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

-- | A monad @m@ with exceptions of type 'ParseImplicitFieldsMsg'
newtype M m a = M (ExceptT ParseImplicitFieldsMsg m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError ParseImplicitFieldsMsg)

runM :: M m a -> m (Either ParseImplicitFieldsMsg a)
runM (M m) = runExceptT m
