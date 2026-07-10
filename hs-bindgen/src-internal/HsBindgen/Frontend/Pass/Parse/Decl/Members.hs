-- | Parse functions related to struct and union members
module HsBindgen.Frontend.Pass.Parse.Decl.Members (
    ParseMembersResult (..)
  , parseMembersWith
  ) where

import Data.Either (partitionEithers)
import Data.List.NonEmpty qualified as NE

import Clang.Enum.Simple (fromSimpleEnum)
import Clang.HighLevel.Types (Fold, FoldException (exception), Next,
                              foldContinueWith, foldRecurseWith, foldTry)
import Clang.LowLevel.Core (CXCursor, CXCursorKind (CXCursor_FieldDecl), CXType,
                            clang_getCursorKind)

import HsBindgen.Frontend.Pass.Parse.Context (ExceptionInCtx (exception),
                                              ParseCtx)
import HsBindgen.Frontend.Pass.Parse.Decl.Field (explicitFieldDecl)
import HsBindgen.Frontend.Pass.Parse.Decl.ImplicitFields qualified as IFields
import HsBindgen.Frontend.Pass.Parse.IsPass (Parse)
import HsBindgen.Frontend.Pass.Parse.Monad.Decl (ParseDecl)
import HsBindgen.Frontend.Pass.Parse.Msg (DelayedParseMsg (ParseImplicitFieldFailed, ParseNestedDeclsFailed))
import HsBindgen.Frontend.Pass.Parse.Result (ParseResult,
                                             getParseResultEitherDecl)
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Type qualified as Macro

-- NOTE: this is a copy of 'HsBindgen.Frontend.Pass.Parse.Decl.Parser' for
-- internal use to prevent cyclic module dependencies
type Parser l = CXCursor -> ParseDecl (Next ParseDecl [ParseResult l Parse])

-- | The result of parsing the members of a struct\/union
data ParseMembersResult l = ParseMembersResult {
      -- | Nested object declarations (i.e., structs and unions)
      declMembers  :: [ParseResult l Parse]
      -- | Field declarations
      --
      -- Returns 'Left' if any nested struct\/union declarations or field
      -- declarations failed to be parsed. Returns 'Right' otherwise.
    , fieldMembers :: Either DelayedParseMsg [C.Field Parse]
    }

deriving stock instance (Macro.HasTypes l)
  => Show (ParseMembersResult l)

-- | Parse all members of a struct\/union
parseMembersWith ::
     -- | Type of the enclosing object
     CXType
  -> ParseCtx
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser l)
     -- | How to continue with the result of parsing members
  -> (ParseMembersResult l -> ParseDecl a)
  -> ParseDecl (Next ParseDecl a)
parseMembersWith ty ctx parseObject k =
    foldRecurseWith (parseMember ctx parseObject) $ \xs -> do
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
data ParseMemberResult l =
    ParseMemberResultFoldException (FoldException (ExceptionInCtx DelayedParseMsg))
  | ParseMemberResultDecls [ParseResult l Parse]
  | ParseMemberResultField (C.ExplicitField Parse)

partitionParseMemberResults ::
     [ParseMemberResult l]
  -> ( [FoldException (ExceptionInCtx DelayedParseMsg)]
     , [ParseResult l Parse]  -- ^ All parse results
     , [ParseResult l Parse]  -- ^ Only parse failures
     , IFields.Inputs l -- ^ Only parse successes
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
     -- | How to parse a non-field declaration (e.g., a union or struct
     -- declaration)
  -> (ParseCtx -> Parser l)
  -> Fold ParseDecl (ParseMemberResult l)
parseMember ctx parseObject =
    fmap flatten $
    foldTry $ \curr -> do
      kind <- fromSimpleEnum <$> clang_getCursorKind curr
      case kind of
        Right CXCursor_FieldDecl -> do
          field <- explicitFieldDecl ctx curr
          -- Field declarations can have struct\/union declarations as children in
          -- the clang AST; however, those are duplicates of declarations that
          -- appear elsewhere, so here we choose not to recurse.
          foldContinueWith $ ParseMemberResultField field
        _otherwise -> do
          fmap ParseMemberResultDecls <$> parseObject ctx curr
  where
    flatten ::
         Either (FoldException (ExceptionInCtx DelayedParseMsg)) (ParseMemberResult l)
      -> ParseMemberResult l
    flatten = \case
        Left e -> ParseMemberResultFoldException e
        Right x -> x
