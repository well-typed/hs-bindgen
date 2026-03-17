-- | Parse functions related to struct and union members
module HsBindgen.Frontend.Pass.Parse.Decl.Members (
    ParseMembersResult (..)
  , parseStructMembersWith
  , parseUnionMembersWith
  ) where

import Data.Bifunctor (Bifunctor (first))
import Data.Either (partitionEithers)
import Data.List qualified as List
import GHC.Records (HasField)

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (Fold, Next, foldContinueWith, foldRecurseWith,
                              simpleFold)
import Clang.LowLevel.Core (CXCursor, CXCursorKind (CXCursor_FieldDecl),
                            clang_getCursorKind)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Deps (depsOfField)
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Parse.Context (ParseCtx)
import HsBindgen.Frontend.Pass.Parse.Decl.Field (structFieldDecl,
                                                 unionFieldDecl)
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl)
import HsBindgen.Frontend.Pass.Parse.Msg (DelayedParseMsg (ParseUnsupportedImplicitFields))
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.Result (ParseResult,
                                             getParseResultEitherDecl,
                                             parseSucceed)

-- NOTE: this is a copy of 'HsBindgen.Frontend.Pass.Parse.Decl.Parser' for
-- internal use to prevent cyclic module dependencies
type Parser = CXCursor -> ParseDecl (Next ParseDecl [ParseResult Parse])

-- | The result of parsing the members of a struct\/union
data ParseMembersResult field = ParseMembersResult {
      -- | Nested object declarations (i.e., structs and unions)
      declMembers  :: [ParseResult Parse]
      -- | Field declarations
      --
      -- Returns 'Left' if any implicit fields were detected in the
      -- struct\/union. Returns 'Right' otherwise.
    , fieldMembers :: Either DelayedParseMsg [field Parse]
    }

-- | Parse the members of a struct
parseStructMembersWith ::
     ParseCtx
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
      -- | How to continue with the result of parsing members
  -> (ParseMembersResult C.StructField -> ParseDecl a)
  -> ParseDecl (Next ParseDecl a)
parseStructMembersWith ctx parseObject = parseMembersWith ctx structFieldDecl parseObject

-- | Parse the members of a union
parseUnionMembersWith ::
     ParseCtx
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
      -- | How to continue with the result of parsing members
  -> (ParseMembersResult C.UnionField -> ParseDecl a)
  -> ParseDecl (Next ParseDecl a)
parseUnionMembersWith ctx parseObject = parseMembersWith ctx unionFieldDecl parseObject

-- | Parse all members of a struct\/union
parseMembersWith ::
     HasField "typ" (field Parse) (C.Type Parse)
  => ParseCtx
     -- | How to parse a field declaration
  -> (ParseCtx -> CXCursor -> ParseDecl (field Parse))
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
     -- | How to continue with the result of parsing members
  -> (ParseMembersResult field -> ParseDecl a)
  -> ParseDecl (Next ParseDecl a)
parseMembersWith ctx parseField parseObject k =
    foldRecurseWith (parseMember ctx parseField parseObject) $ \xs -> do
      let (otherRs, fields)   = first concat $ partitionEithers xs
          (fails, successes) = partitionEithers $
                                  map getParseResultEitherDecl otherRs
          -- Separate out nested declarations from regular struct\/union fields
          --
          -- Local declarations inside structs\/unions that are not used by any
          -- fields result in implicit fields. Unfortunately, @libclang@ does
          -- not make these visible
          -- <https://github.com/llvm/llvm-project/issues/122257>. This matters,
          -- because we need the offsets of these implicit fields. For now we
          -- therefore only try to detect the situation and report an error when
          -- it happens. Hopefully this is anyway very rare.
          (used, unused) = detectImplicitFields successes fields
      if null unused then
        k ParseMembersResult {
              declMembers  = fails ++ map parseSucceed used
            , fieldMembers = Right fields
            }
      -- If the struct has implicit fields, don't generate anything.
      else
        k ParseMembersResult {
              declMembers  = fails
            , fieldMembers = Left ParseUnsupportedImplicitFields
            }

-- | Parse a single member of a struct\/union
parseMember ::
     ParseCtx
     -- | How to parse a field declaration
  -> (ParseCtx -> CXCursor -> ParseDecl (field Parse))
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
  -> Fold ParseDecl (Either [ParseResult Parse] (field Parse))
parseMember ctx parseField parseObject = simpleFold $ \curr -> do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        field <- parseField ctx curr
        -- Field declarations can have struct\/union declarations as children in
        -- the clang AST; however, those are duplicates of declarations that
        -- appear elsewhere, so here we choose not to recurse.
        foldContinueWith $ Right field
      _otherwise -> do
        fmap Left <$> parseObject ctx curr

-- | Detect implicit fields inside a struct\/union
--
-- Implicit fields arise from structs\/unions that are declared inside an outer
-- struct\/union, but without an explicit reference from any of the fields in
-- that outer struct\/union. Something like this:
--
-- > struct outer {
-- >   struct inner {
-- >     int x;
-- >     int y;
-- >   };
-- >   int z;
-- > };
--
-- We cannot support implicit fields due to a limitation of clang
-- (<https://github.com/well-typed/hs-bindgen/issues/659>), but we should at
-- least detect when they are used and issue an error.
--
-- This function partitions local declarations into those that are referenced by
-- some field ("regular declarations"), and those that are not (that is, the
-- implicit fields). Doing this correctly is a little tricky, because clang
-- reports /all/ nested declarations at once. For example, in
--
-- > struct outer {
-- >   struct {
-- >     int x1_1;
-- >     struct {
-- >       int x1_2_1;
-- >     } x1_2;
-- >   } x1;
-- >   int x2;
-- > };
--
-- there are no implicit fields, but we see both nested structs at once (inside
-- the outermost struct), and so we need to check if there is a reference to the
-- inner struct from /any/ nested field, not just fields of the outermost
-- struct.
detectImplicitFields ::
     HasField "typ" (field Parse) (C.Type Parse)
  => [C.Decl Parse]
     -- ^ Nested declarations inside a struct\/union
  -> [field Parse]
     -- ^ Fields of the (outer) struct\/union
  -> ([C.Decl Parse], [C.Decl Parse])
detectImplicitFields nestedDecls outerFields =
    List.partition declIsUsed nestedDecls
  where
    nestedFields :: [Either (C.StructField Parse) (C.UnionField Parse)]
    nestedFields = flip concatMap nestedDecls $ \decl ->
        case decl.kind of
          C.DeclStruct struct -> map Left  struct.fields
          C.DeclUnion union   -> map Right union.fields
          _otherwise          -> []

    fieldDeps :: [PrelimDeclId]
    fieldDeps = map snd $ concat [
          concatMap depsOfField outerFields
        , concatMap (either depsOfField depsOfField) nestedFields
        ]

    declIsUsed :: C.Decl Parse -> Bool
    declIsUsed decl = decl.info.id `elem` fieldDeps
