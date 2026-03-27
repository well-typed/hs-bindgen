-- | Parse functions related to struct and union members
module HsBindgen.Frontend.Pass.Parse.Decl.Members (
    ParseMembersResult (..)
  , parseStructMembersWith
  , parseUnionMembersWith
  ) where

import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NE
import GHC.Records (HasField)

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (Fold, FoldException (exception), Next,
                              foldContinueWith, foldRecurseWith, foldTry)
import Clang.LowLevel.Core (CXCursor, CXCursorKind (CXCursor_FieldDecl), CXType,
                            clang_getCursorKind)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass.Parse.Context (ExceptionInCtx (exception),
                                              ParseCtx)
import HsBindgen.Frontend.Pass.Parse.Decl.Field (structFieldDecl,
                                                 unionFieldDecl)
import HsBindgen.Frontend.Pass.Parse.Decl.ImplicitFields qualified as IFields
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl)
import HsBindgen.Frontend.Pass.Parse.Msg (DelayedParseMsg (ParseImplicitFieldFailed, ParseNestedDeclsFailed))
import HsBindgen.Frontend.Pass.Parse.Result (ParseResult,
                                             getParseResultEitherDecl)

-- NOTE: this is a copy of 'HsBindgen.Frontend.Pass.Parse.Decl.Parser' for
-- internal use to prevent cyclic module dependencies
type Parser = CXCursor -> ParseDecl (Next ParseDecl [ParseResult Parse])

-- | The result of parsing the members of a struct\/union
data ParseMembersResult field = ParseMembersResult {
      -- | Nested object declarations (i.e., structs and unions)
      declMembers  :: [ParseResult Parse]
      -- | Field declarations
      --
      -- Returns 'Left' if any nested struct\/union declarations or field
      -- declarations failed to be parsed. Returns 'Right' otherwise.
    , fieldMembers :: Either DelayedParseMsg [field Parse]
    }

deriving stock instance Show (field Parse) => Show (ParseMembersResult field)

-- | Parse the members of a struct
parseStructMembersWith ::
     CXType
     -- ^ Type of the enclosing object
  -> ParseCtx
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
      -- | How to continue with the result of parsing members
  -> (ParseMembersResult C.StructField -> ParseDecl a)
  -> ParseDecl (Next ParseDecl a)
parseStructMembersWith ty ctx parseObject k =
    parseMembersWith ty ctx structFieldDecl parseObject k

-- | Parse the members of a union
parseUnionMembersWith ::
     CXType
     -- ^ Type of the enclosing object
  -> ParseCtx
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
      -- | How to continue with the result of parsing members
  -> (ParseMembersResult C.UnionField -> ParseDecl a)
  -> ParseDecl (Next ParseDecl a)
parseUnionMembersWith ty ctx parseObject k =
    parseMembersWith ty ctx unionFieldDecl parseObject k

-- | Parse all members of a struct\/union
parseMembersWith ::
     ( IFields.MakeImplicitField field
     , HasField "typ" (field Parse) (C.Type Parse)
     )
     -- | Type of the enclosing object
  => CXType
  -> ParseCtx
     -- | How to parse a field declaration
  -> (ParseCtx -> CXCursor -> ParseDecl (field Parse))
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
     -- | How to continue with the result of parsing members
  -> (ParseMembersResult field -> ParseDecl a)
  -> ParseDecl (Next ParseDecl a)
parseMembersWith ty ctx parseField parseObject k =
    foldRecurseWith (parseMember ctx parseField parseObject) $ \xs -> do
      let (foldExceptions, allDecls, fails, successes) = partitionParseMemberResults xs
          -- Always return all nested declarations, regardless of their parse
          -- status. The @Select@ pass wil handle deselecting declarations if
          -- necessary.
          declMembers = allDecls
      if
        -- If any nested declarations failed to parse, then we can't parse the
        -- current declaration for fear of missing (implicit) fields.
        | not (null fails)
        -> k ParseMembersResult {
                declMembers = declMembers
              , fieldMembers = Left ParseNestedDeclsFailed
              }
        -- If any exceptions occurred during folding, only return the first one
        | Just foldExceptions' <- NE.nonEmpty foldExceptions
        -> k ParseMembersResult {
                declMembers = declMembers
              , fieldMembers = Left (NE.head foldExceptions').exception.exception
              }
        -- Local declarations inside structs\/unions that are not used by any
        -- fields result in implicit fields. Unfortunately, @libclang@ does
        -- not make these visible
        -- <https://github.com/llvm/llvm-project/issues/122257>. This matters,
        -- because we need the offsets of these implicit fields. For now we
        -- therefore only try to detect the situation and report an error when
        -- it happens. Hopefully this is anyway very rare.
        --
        -- Implicit fields might exist for fields of nested anonymous
        -- structs\/unions that failed to parse. At this point, we have already
        -- checked that there are no failed parses. So all nested anonymous
        -- structs\/unions were parsed successfully, and we can properly detect
        -- implicit fields using 'IFields.withImplicitFields'.
        | otherwise
        -> do -- From the explicit members, derive implicit fields and include them in
              -- the list of members
              members <- IFields.withImplicitFields (IFields.EnclosingObject ty) successes
              k $ case members of
                -- Some implicit fields were not successfully detected. It is unsafe
                -- to return an incomplete list of implicit and explicit fields, so we
                -- return a failure message instead.
                IFields.OutputFail exc ->
                  ParseMembersResult {
                      declMembers = declMembers
                    , fieldMembers = Left (ParseImplicitFieldFailed exc)
                    }
                IFields.OutputSuccess fields ->
                  ParseMembersResult {
                      declMembers = declMembers
                    , fieldMembers = Right fields
                    }

-- | The result of parsing a single member of a struct\/union
data ParseMemberResult field =
    ParseMemberResultFoldException (FoldException (ExceptionInCtx DelayedParseMsg))
  | ParseMemberResultDecls [ParseResult Parse]
  | ParseMemberResultField (field Parse)

partitionParseMemberResults ::
     [ParseMemberResult field]
  -> ( [FoldException (ExceptionInCtx DelayedParseMsg)]
     , [ParseResult Parse]  -- ^ All parse results
     , [ParseResult Parse]  -- ^ Only parse failures
     , IFields.Inputs field -- ^ Only parse successes
     )
partitionParseMemberResults = mconcat . fmap f
  where
    f = \case
          ParseMemberResultFoldException e ->
                ([e], [], [], IFields.inputEmpty)
          ParseMemberResultDecls xs ->
            let (fails, successes) = partitionEithers $ map getParseResultEitherDecl xs
            in  ( []
                , xs
                , fails
                , mconcat $ map IFields.inputDecl successes
                )
          ParseMemberResultField x ->
                ([], [], [], IFields.inputField x)

-- | Parse a single member of a struct\/union
parseMember ::
     ParseCtx
     -- | How to parse a field declaration
  -> (ParseCtx -> CXCursor -> ParseDecl (field Parse))
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser)
  -> Fold ParseDecl (ParseMemberResult field)
parseMember ctx parseField parseObject =
    fmap flatten $
    foldTry $ \curr -> do
      kind <- fromSimpleEnum <$> clang_getCursorKind curr
      case kind of
        Right CXCursor_FieldDecl -> do
          field <- parseField ctx curr
          -- Field declarations can have struct\/union declarations as children in
          -- the clang AST; however, those are duplicates of declarations that
          -- appear elsewhere, so here we choose not to recurse.
          foldContinueWith $ ParseMemberResultField field
        _otherwise -> do
          fmap ParseMemberResultDecls <$> parseObject ctx curr
  where
    flatten ::
         Either (FoldException (ExceptionInCtx DelayedParseMsg)) (ParseMemberResult field)
      -> ParseMemberResult field
    flatten = \case
        Left e -> ParseMemberResultFoldException e
        Right x -> x
