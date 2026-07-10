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

import Clang.LowLevel.Core (CXType, CallFailed, clang_Type_getOffsetOf)

import HsBindgen.Frontend.Analysis.Deps (depsOfExplicitField, depsOfStruct,
                                         depsOfUnion)
import HsBindgen.Frontend.Pass.Parse.IsPass (IsAnon (isAnon), Parse)
import HsBindgen.Frontend.Pass.Parse.IsPass qualified as Origin (FieldOrigin (..))
import HsBindgen.Frontend.Pass.Parse.Msg (ParseImplicitFieldsMsg (..))
import HsBindgen.Imports (Bifunctor (bimap), MonadIO (..), NonEmpty, forM)
import HsBindgen.IR.C qualified as C
import HsBindgen.IR.Pass (PassScopedName (ScopedName))

{-------------------------------------------------------------------------------
  Inputs
-------------------------------------------------------------------------------}

-- | A list of nested struct\/union declarations and field declarations
newtype Inputs l = Inputs {
    nestedDecls :: [Either (C.Decl l Parse) (C.ExplicitField Parse)]
  }
  deriving newtype (Semigroup, Monoid)

inputEmpty :: Inputs l
inputEmpty = Inputs []

inputField :: C.ExplicitField Parse -> Inputs l
inputField x = Inputs [Right x]

inputDecl :: C.Decl l Parse -> Inputs l
inputDecl x = Inputs [Left x]

{-------------------------------------------------------------------------------
  Outputs
-------------------------------------------------------------------------------}

data Outputs =
    -- | An exception occurred
    OutputFail {
        exception :: ParseImplicitFieldsMsg
      }
    -- | All explicit and implicit fields
  | OutputSuccess {
        fields    :: [C.Field Parse]
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
-- Anonymous nested structs/unions have no name, but they need one for our
-- Haskell bindings, so they are named after their first field. We prefix the
-- field name with "anon'" to highlight that the field is created for an
-- anonymous struct/union. The tick ensures that this name can not clash with
-- any existing C fields because ticks are not allowed in C identifiers. See
-- issue #2064 for more information about this "anon'" prefix.
--
-- <https://github.com/well-typed/hs-bindgen/issues/2064>
--
-- Informally, the former will be transformed to the latter:
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
-- >   } anon'x;
-- > };
--
withImplicitFields ::
     forall m l. (
       MonadIO m
     )
  => EnclosingObject
  -> Inputs l
  -> m Outputs
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
            $ fmap (fmap C.FieldExplicit) classifications.explicitFields ++ implicitFields
        }
  where
    classifications = classifyInputs inputs

    getImplicitField' ::
        Numbered (C.Decl l Parse)
      -> M m (Numbered (C.Field Parse))
    getImplicitField' decl =
        Numbered decl.number <$> getImplicitField encObj decl.numberee

{-------------------------------------------------------------------------------
  Inputs classification
-------------------------------------------------------------------------------}

data Classification l = Classification {
    -- | Parsed explicit fields
    explicitFields :: [Numbered (C.ExplicitField Parse)]
    -- | Candidates for implicit fields
  , candidates     :: [Numbered (C.Decl l Parse)]
  }

-- | Declarations are numbered so that we can sort them in the order that they
-- were parsed
data Numbered a = Numbered {
    number   :: Int
  , numberee :: a
  }
  deriving stock (Functor, Foldable, Traversable)

classifyInputs :: forall l. Inputs l -> Classification l
classifyInputs inputs = Classification {
      explicitFields = explicitFields
    , candidates = candidates
    }
  where
    -- | Number all the declarations so that we can re-sort them at the end of
    -- the algorithm
    membersNumbered ::
      [Numbered (Either (C.Decl l Parse) (C.ExplicitField Parse))]
    membersNumbered = zipWith Numbered [0..] inputs.nestedDecls
    nestedDecls :: [Numbered (C.Decl l Parse)]
    explicitFields :: [Numbered (C.ExplicitField Parse)]
    (nestedDecls, explicitFields) = partitionEithers $ fmap numberedIn membersNumbered

    -- | A struct\/union declaration requires an implicit field if the
    -- declaration is anonymous, and if we have not already created an implicit
    -- field for that declaration
    candidates :: [Numbered (C.Decl l Parse)]
    candidates = [
          decl
        | let decls = fmap (.numberee) nestedDecls
              fields = fmap (.numberee) explicitFields
        , decl <- nestedDecls
        , isAnonymous decl.numberee
        , not (isReferenced decl.numberee fields decls)
        ]

numberedIn :: Numbered (Either a b) -> Either (Numbered a) (Numbered b)
numberedIn x = bimap (Numbered x.number) (Numbered x.number) x.numberee

-- | Check if a target declaration is anonymous with respect to an enclosing
-- object.
isAnonymous :: C.Decl l Parse -> Bool
isAnonymous decl = case decl.kind of
    C.DeclStruct struct -> struct.ann.isAnon
    C.DeclUnion  union  -> union.ann.isAnon
    _                   -> False

-- | Check if a target declaration is referenced by any fields, nested structs,
-- or nested unions.
--
-- Note: the nested structs and unions should include implicit fields.
isReferenced ::
     C.Decl l Parse
     -- | Fields that are declared directly in the enclosing object
  -> [C.ExplicitField Parse]
     -- | Struct and union declarations (recursively) nested in the enclosing
     -- object
  -> [C.Decl l Parse]
  -> Bool
isReferenced decl fields decls =
       decl.info.id `elem` map fst (concatMap depsOfExplicitField fields)
    || decl.info.id `elem` map fst (concatMap depsOfStructOrUnion decls)
  where
    depsOfStructOrUnion d = case d.kind of
        C.DeclStruct struct -> depsOfStruct struct
        C.DeclUnion  union  -> depsOfUnion  union
        _ -> []

{-------------------------------------------------------------------------------
  Implicit field
-------------------------------------------------------------------------------}

-- | Generate an implicit field for an anonymous object
getImplicitField ::
     forall m l. (
       MonadIO m
     )
     -- | The enclosing object
  => EnclosingObject
     -- | An anonymous object nested in the enclosing object
  -> C.Decl l Parse
  -> M m (C.Field Parse)
getImplicitField encObj decl = do
    indFields <- getIndirectFields encObj decl
    indFieldsNE <- liftEither $ checkNonEmpty indFields
    -- The offset to the implicit field is equal to the offset to any of the
    -- nested object's fields, subtracted by the offset of that same field with
    -- respect to the nested object.
    --
    -- We do not assume that the fields are ordered in any way, hence we use
    -- 'minimumBy' to determine which /named/ field is the first. We need to
    -- know which field is first so that we can use that field's name as the
    -- implicit field's name as well.
    let (origField, indField) = minimumBy (\x y -> compare (snd x).offset (snd y).offset) indFieldsNE
        offset' = indField.offset - origField.offset

    let implicitField = makeImplicitField origField indField offset'

    pure (C.FieldImplicit implicitField)
  where
    checkNonEmpty :: [a] -> Either ParseImplicitFieldsMsg (NonEmpty a)
    checkNonEmpty xs = case NonEmpty.nonEmpty xs of
        Nothing -> Left UnsupportedEmptyAnon
        Just ys -> Right ys

    makeImplicitField ::
         C.Field Parse
      -> IndirectField Parse
      -> Int
      -> C.ImplicitField Parse
    makeImplicitField origField indField offset =
        C.ImplicitField {
            info = C.FieldInfo {
                loc = decl.info.loc
              , name = C.ScopedName ("anon'" <> indField.info.name.text)
              , comment = ()
              }
          , typRef = C.AnonRef decl.info.id
          , offset = offset
          , ann = Origin.FieldOrigin (getOrigin origField)
          }

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
getOrigin :: C.Field Parse -> ScopedName Parse
getOrigin = C.elimField (.info.name) (.ann.field)

{-------------------------------------------------------------------------------
  Indirect field
-------------------------------------------------------------------------------}

getIndirectFields ::
     forall m l. (
       MonadIO m
     )
     -- | The enclosing object
  => EnclosingObject
     -- | An anonymous object nested in the enclosing object
  -> C.Decl l Parse
  -> M m [(C.Field Parse, IndirectField Parse)]
getIndirectFields encObj decl = forM fields $ \field ->
    (field,) <$> getIndirectField encObj decl field
  where
    fields :: [C.Field Parse]
    fields = case decl.kind of
        C.DeclStruct struct -> struct.fields
        C.DeclUnion  union  -> union.fields
        _ -> []

getIndirectField ::
     forall m l. (
       MonadIO m
     )
     -- | The enclosing object
  => EnclosingObject
     -- | An anonymous object nested in the enclosing object
  -> C.Decl l Parse
     -- | A field of the anonymous object
  -> C.Field Parse
  -> M m (IndirectField Parse)
getIndirectField encObj _decl field = do
    offsetOuter <- offsetOf encObj (getOrigin field)
    pure $ mkIndirectField offsetOuter
  where
    mkIndirectField :: Int -> IndirectField Parse
    mkIndirectField offset = IndirectField {
          info = field.info
        , typ = field.typ
        , offset = offset
        , width = field.width
        , origin = Origin.FieldOrigin (getOrigin field)
        }

data IndirectField p = IndirectField {
      info :: C.FieldInfo p
    , typ :: C.Type p
    , offset :: Int
    , width  :: Maybe Int
    , origin :: Origin.FieldOrigin
    }

{-------------------------------------------------------------------------------
  Field offset
-------------------------------------------------------------------------------}

newtype EnclosingObject = EnclosingObject { typ :: CXType }
  deriving stock Show

-- | Get the offset of a named field with respect to an enclosing object.
--
-- If the named field is *not* an (indirect) field of the enclosing object, then
-- an 'UnexpectedClangOffsetOfException' exception is returned. Otherwise, a
-- t'FieldOffset' is returned.
offsetOf ::
     MonadIO m
  => EnclosingObject
  -> ScopedName Parse
  -> M m Int
offsetOf encObj name = do
    offsetE <- liftIO $ try @CallFailed $ clang_Type_getOffsetOf encObj.typ fieldName
    case offsetE of
      Left e
        -> throwError (UnexpectedClangOffsetOfException name.text (show e))
      Right offset
        -> pure (fromIntegral offset)
  where
    fieldName = Text.unpack name.text

{-------------------------------------------------------------------------------
  Monad
-------------------------------------------------------------------------------}

-- | A monad @m@ with exceptions of type 'ParseImplicitFieldsMsg'
newtype M m a = M (ExceptT ParseImplicitFieldsMsg m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError ParseImplicitFieldsMsg)

runM :: M m a -> m (Either ParseImplicitFieldsMsg a)
runM (M m) = runExceptT m
