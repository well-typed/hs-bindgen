-- | Fold declarations
module HsBindgen.Frontend.Pass.Parse.Decl (topLevelDecl) where

import Control.Exception (Exception (..), SomeException)
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Text qualified as Text
import Foreign.C (CInt)

import C.Expr.Parse qualified as CExpr.DSL

import Clang.CStandard
import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Context
import HsBindgen.Frontend.Pass.Parse.Decl.Field (getFieldInfo)
import HsBindgen.Frontend.Pass.Parse.Decl.Macro (getReparseInfo)
import HsBindgen.Frontend.Pass.Parse.Decl.Members (ParseMembersResult (declMembers, fieldMembers),
                                                   parseStructMembersWith,
                                                   parseUnionMembersWith)
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad.Decl
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.Parse.Type
import HsBindgen.Frontend.RootHeader (HashIncludeArg)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | Top-level declaration
--
-- We only attach an exception handler for top-level declarations: if something
-- goes wrong with a nested declaration, we want to skip the entire outer
-- declaration.
topLevelDecl :: Fold ParseDecl (CXSourceLocation, [ParseResult Parse])
topLevelDecl = foldWithHandler handleParseExceptions parseDeclTopLevel
  where
    handleParseExceptions ::
         CXCursor
      -> SomeException
      -> ParseDecl (Maybe (CXSourceLocation, [ParseResult Parse]))
    handleParseExceptions curr err
      | Just e <- fromException @(ExceptionInCtx DelayedParseMsg) err = do
          loc <- clang_getCursorLocation curr
          Just . (loc,) <$> parseFailNoInfo e.ctx e.exception curr
      | otherwise = liftIO $ throwIO err

{-------------------------------------------------------------------------------
  Functions for each kind of declaration
-------------------------------------------------------------------------------}

type Parser = CXCursor -> ParseDecl (Next ParseDecl [ParseResult Parse])

-- | Parse declarations with available parse context
parseDeclNested :: [C.EnclosingRef Parse] -> ParseCtx -> Parser
parseDeclNested enclosing ctx = parseDecl' enclosing (Just ctx)

-- | Parse a top level declaration (parse context not yet available)
parseDeclTopLevel :: CXCursor -> ParseDecl (Next ParseDecl (CXSourceLocation, [ParseResult Parse]))
parseDeclTopLevel curr = do
    -- By getting and attaching the location to all parse results, we
    -- potentially get the location twice. We could store the 'CXSourceLocation'
    -- next to all parse results and use it to retrieve the single location
    -- stored in the 'DeclInfo'.
    loc       <- clang_getCursorLocation curr
    nextDecls <- parseDecl' [] Nothing curr
    -- We cannot directly thread in macros here because the 'Functor' instance
    -- of 'Next' drops them when we have `Continue Nothing`. That is, there is a
    -- subtle difference between `foldContinue`, and `foldContinueWith []`. This
    -- also leads to the following edge case:
    --
    -- Forward declarations ('DefinitionElsewhere') produce 'Continue Nothing',
    -- so macros before a forward declaration end up being threaded after the
    -- forward declaration, and before the next, actual declaration instead.
    -- This is sub-optimal, but does not lead to incorrect behavior. Should the
    -- next declaration be the definition of the forward declaration the macro
    -- still appears before the definition.
    pure $ (loc,) <$> nextDecls

-- | Auxiliary function; use 'parseDeclNested' or 'parseDeclTopLevel'
parseDecl' :: HasCallStack => [C.EnclosingRef Parse] -> Maybe ParseCtx -> Parser
parseDecl' enclosing mCtx = withCursorKindNoCtx $ \case
      -- Ordinary kinds that we parse
      Right CXCursor_FunctionDecl    -> parseDeclWith enclosing (push CNameKindOrdinary NotRequiredForScoping) functionDecl
      Right CXCursor_VarDecl         -> parseDeclWith enclosing (push CNameKindOrdinary NotRequiredForScoping) varDecl
      Right CXCursor_TypedefDecl     -> parseDeclWith enclosing (push CNameKindOrdinary RequiredForScoping)    typedefDecl
      Right CXCursor_MacroDefinition -> parseDeclWith enclosing (push CNameKindMacro    NotRequiredForScoping) macroDefinition

      -- Tagged kinds that we parse
      Right CXCursor_StructDecl -> parseDeclWith enclosing (push (CNameKindTagged CTagKindStruct) NotRequiredForScoping) structDecl
      Right CXCursor_UnionDecl  -> parseDeclWith enclosing (push (CNameKindTagged CTagKindUnion)  NotRequiredForScoping) unionDecl
      Right CXCursor_EnumDecl   -> parseDeclWith enclosing (push (CNameKindTagged CTagKindEnum)   NotRequiredForScoping) enumDecl

      -- Process macro expansions independent of any select predicates
      Right CXCursor_MacroExpansion -> macroExpansion

      -- Kinds that we skip over
      Right CXCursor_AlignedAttr        -> \_curr -> foldContinue
      Right CXCursor_InclusionDirective -> \_curr -> foldContinue
      Right CXCursor_PackedAttr         -> \_curr -> foldContinue
      Right CXCursor_UnexposedAttr      -> \_curr -> foldContinue
      Right CXCursor_UnexposedDecl      -> \_curr -> foldContinue
      -- @visibility@ attributes. The visibility itself the value could be
      -- obtained using 'getCursorVisibility'.
      Right CXCursor_VisibilityAttr     -> \_curr -> foldContinue

      -- Report error for declarations we don't recognize
      eKind -> case mCtx of
        Nothing  ->
          -- Without a 'ParseCtx', we cannot attach a possible error to the
          -- declaration, and so we must panic.
          unavoidablePanicUnrecognizedKind eKind
        Just ctx ->
          failUnrecognizedKind ctx eKind >=> foldContinueWith
  where
    push :: CNameKind -> RequiredForScoping -> ParseCtx
    push kind scoping =
      let ctx = DeclCtx kind scoping
      in  case mCtx of
            Nothing     -> mkCtx   ctx
            Just ctxOld -> pushCtx ctx ctxOld

    -- We use a custom 'withCursorKind' function here, because the parse context
    -- is not always available.
    withCursorKindNoCtx :: (Either CInt CXCursorKind -> Parser) -> Parser
    withCursorKindNoCtx k = \curr -> do
        mKind <- fromSimpleEnum <$> clang_getCursorKind curr
        k mKind curr

    -- Unavoidably panic (!) on an unrecognized cursor kind
    --
    -- Only use this function if there is no way to assemble a @ParseResult@.
    --
    -- See 'failUnrecognizedKind'.
    unavoidablePanicUnrecognizedKind ::
      MonadIO m => Either CInt CXCursorKind -> CXCursor -> m b
    unavoidablePanicUnrecognizedKind eKind curr = do
        loc <- HighLevel.clang_getCursorLocation' curr
        case eKind of
          Left i ->
            panicIO $ concat [
                "Unrecognized CXCursorKind "
              , show i
              , " at "
              , show loc
              ]
          Right kind -> do
            spelling <- clang_getCursorKindSpelling (simpleEnum kind)
            panicIO $ concat [
                "Unknown cursor of kind "
              , show kind
              , " ("
              , Text.unpack spelling
              , ") at "
              , show loc
              ]

-- | Parse declaration
--
-- NOTE: We currently skip all built-ins. The only built-ins that can even
-- /have/ an associated declaration at all are macros. However, since we cannot
-- get the list of tokens for built-in macros, we would anyway need to
-- special-case them. For now we skip /all/ builtins.
parseDeclWith ::
     [C.EnclosingRef Parse]
  -> ParseCtx
  -> ([C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser)
  -> Parser
parseDeclWith enclosing ctx parser curr = do
    mBuiltin <- PrelimDeclId.checkIsBuiltin curr
    case mBuiltin of
      Just _name -> foldContinue
      Nothing    -> withDeclInfo enclosing ctx parseExplicitDecl curr
  where
    parseExplicitDecl :: C.DeclInfo Parse -> Parser
    parseExplicitDecl info = \_curr ->
      if | C.Unavailable <- info.availability ->
             foldContinueWith [
                 parseDoNotAttempt info DeclarationUnavailable
               ]
         | otherwise ->
             parser enclosing ctx info curr

-- | Macros
macroDefinition ::
  HasCallStack => [C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser
macroDefinition _enclosing _ctx info = \curr -> do
    cxLoc  <- clang_getCursorLocation curr
    tokens <- getMacroTokens curr
    cStd   <- getCStandard
    let result = mkResult cStd tokens
    -- We only store the macro location and definition in the parser state. We
    -- consolidate macros and non-macros at top-level because newer versions of
    -- Clang allow us to determine the sequence number.
    recordMacroDefinitionAt cxLoc result
    foldContinue
  where
    mkResult :: ClangCStandard -> [Token TokenSpelling] -> ParseResult Parse
    mkResult cStd tokens =
        case parseMacroTokens cStd info.id tokens of
          Right parsed -> parseSucceed C.Decl{
              info = info
            , kind = C.DeclMacro parsed
            , ann  = NoAnn
            }
          Left msg -> ParseResult{
              id             = info.id
            , loc            = info.loc
            , classification = ParseResultFailure msg
            }

    getMacroTokens :: CXCursor -> ParseDecl [Token TokenSpelling]
    getMacroTokens curr' = do
        unit'  <- getTranslationUnit
        range  <- HighLevel.clang_getCursorExtent curr'
        HighLevel.clang_tokenize unit' (multiLocExpansion <$> range)

-- | Parse an struct declaration
--
-- Visibility attributes are ignored on structs, since as far as we can tell
-- they do not affect the Haskell bindings.
structDecl :: [C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser
structDecl enclosing ctx info = \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      Definition -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        isAnon    <- clang_Cursor_isAnonymousRecordDecl curr

        let mkStruct :: [C.StructField Parse] -> C.Decl Parse
            mkStruct allFields = C.Decl {
                  info = info
                , ann  = NoAnn
                , kind = C.DeclStruct C.Struct{
                      sizeof    = fromIntegral sizeof
                    , alignment = fromIntegral alignment
                    , fields    = filter isField regularFields
                    , flam      = mFlam
                    , ann       = IsAnon isAnon
                    }
                }
              where
                (regularFields, mFlam) = partitionFields allFields

            enclosing' :: [C.EnclosingRef Parse]
            enclosing' = C.EnclosingRef info.id : enclosing

        -- Recursively parse all members of the struct. These members include
        -- field declarations and nested struct/union declarations.
        parseStructMembersWith ty ctx (parseDeclNested enclosing') $ \membersResult ->
          -- The parse results of nested struct/union declarations are returned
          -- regardless of the parse status of field declarations.
          --
          -- NOTE: the parse results *must* be returned here. The
          -- 'withImplicitFields' algorithm relies on it.
          (membersResult.declMembers ++) <$>
            -- If we failed to parse any of the field declarations, then we will
            -- not return a struct object, because it will have missing fields
            -- that are therefore inaccessible in Haskell.
            case membersResult.fieldMembers of
              Left failMsg ->
                parseFail ctx info.id info.loc failMsg
              Right fields ->
                pure [parseSucceed (mkStruct fields)]

      DefinitionUnavailable ->
        let decl :: C.Decl Parse
            decl = C.Decl{
                info = info
              , kind = C.DeclOpaque
              , ann  = NoAnn
              }
        in  foldContinueWith [parseSucceed decl]
      DefinitionElsewhere _ ->
        foldContinue
  where
    -- Split off FLAM, if any
    partitionFields ::
         [C.StructField Parse]
      -> ([C.StructField Parse], Maybe (C.StructField Parse))
    partitionFields = go []
      where
        go ::
             [C.StructField Parse]
          -> [C.StructField Parse]
          -> ([C.StructField Parse], Maybe (C.StructField Parse))
        go acc []     = (reverse acc, Nothing)
        go acc (f:fs) = case f.typ of
                          C.TypeIncompleteArray ty ->
                            let f' = f & #typ .~ ty
                            in (reverse acc ++ fs, Just f')
                          _otherwise->
                            go (f:acc) fs

    -- An unnamed bit-field is used to specify padding, using a specified
    -- padding width or zero to instruct the compiler to not pack any more
    -- fields into the current storage unit.  This predicate is used to filter
    -- out such bit-fields.
    isField :: C.StructField Parse -> Bool
    isField field = not $ Text.null field.info.name.text && isJust field.width

-- | Parse a union declaration
--
-- Visibility attributes are ignored on unions, since as far as we can tell they
-- do not affect the Haskell bindings.
unionDecl :: [C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser
unionDecl enclosing ctx info = \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      Definition -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        isAnon    <- clang_Cursor_isAnonymousRecordDecl curr

        let mkUnion :: [C.UnionField Parse] -> C.Decl Parse
            mkUnion fields = C.Decl{
                  info = info
                , ann  = NoAnn
                , kind = C.DeclUnion C.Union{
                             sizeof    = fromIntegral sizeof
                           , alignment = fromIntegral alignment
                           , fields    = filter isField fields
                           , ann       = IsAnon isAnon
                           }
                }
            enclosing' :: [C.EnclosingRef Parse]
            enclosing' = C.EnclosingRef info.id : enclosing

        -- Recursively parse all members of the union. These members include
        -- field declarations and nested struct/union declarations.
        parseUnionMembersWith ty ctx (parseDeclNested enclosing') $ \membersResult ->
          -- The parse results of nested struct/union declarations are returned
          -- regardless of the parse status of field declarations.
          --
          -- NOTE: the parse results *must* be returned here. The
          -- 'withImplicitFields' algorithm relies on it.
          (membersResult.declMembers ++) <$>
            -- If we failed to parse any of the field declarations, then we will
            -- not return a union object, because it will have missing fields
            -- that are therefore inaccessible in Haskell.
            case membersResult.fieldMembers of
              Left failMsg ->
                parseFail ctx info.id info.loc failMsg
              Right fields ->
                pure [parseSucceed (mkUnion fields)]

      DefinitionUnavailable -> do
        let decl :: C.Decl Parse
            decl = C.Decl{
                info = info
              , kind = C.DeclOpaque
              , ann  = NoAnn
              }
        foldContinueWith [parseSucceed decl]
      DefinitionElsewhere _ ->
        foldContinue
  where
    -- An unnamed bit-field is used to specify padding, using a specified
    -- padding width or zero to instruct the compiler to not pack any more
    -- fields into the current storage unit.  This predicate is used to filter
    -- out such bit-fields.
    isField :: C.UnionField Parse -> Bool
    isField field = not $ Text.null field.info.name.text

typedefDecl :: [C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser
typedefDecl _enclosing ctx info = \curr -> do
    typedefType <-
      fromCXType ctx =<< clang_getTypedefDeclUnderlyingType curr

    declKind <-
      case typedefType of
        C.TypeVoid ->
          -- We regard
          --
          -- > typedef void foo
          --
          -- as the declaration of an opaque type.
          return C.DeclOpaque
        _otherwise -> do
          typedefAnn <- getReparseInfo curr
          return $ C.DeclTypedef C.Typedef{
              typ = typedefType
            , ann = typedefAnn
            }

    let decl :: C.Decl Parse
        decl = C.Decl{
            info = info
          , kind = declKind
          , ann  = NoAnn
          }
    foldContinueWith [parseSucceed decl]

macroExpansion :: Parser
macroExpansion = \curr -> do
    range <- fmap multiLocExpansion <$> HighLevel.clang_getCursorExtent curr
    let loc = range.rangeStart
    unit <- getTranslationUnit
    tokens <- HighLevel.clang_tokenize unit range
    case getMacroName tokens of
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1554>
      --
      -- Attach failed macroName message to declaration.
      Nothing -> traceImmediateGlobal ParseMacroExpansionNoMacroName
      Just macroName -> recordMacroExpansionAt loc macroName
    foldContinue
  where
    getMacroName :: [Token TokenSpelling] -> Maybe Text
    getMacroName []    = Nothing
    -- The spelling of function macros includes the function parameters. For
    -- example, when expanding
    --
    -- #define ID(X) (X)
    -- void foo(int ID(z));
    --
    -- the pretty printed tokens at expansion location are "ID ( z )" instead of
    -- just the macro name "ID". We believe that in `hs-bindgen` it is enough to
    -- identify macros by their first token only, because the name of macro
    -- functions in `hs-bindgen` also corresponds to this first token. That is,
    -- in `hs-bindgen`,
    --
    -- #define ID
    -- #define ID(X) (X)
    --
    -- are conflicting (macro) declarations.
    getMacroName (x:_) =
      let macroName = getTokenSpelling $ tokenSpelling x
      in  if Text.null macroName; then
            Nothing
          else
            Just macroName

-- | Parse an enum declaration
--
-- Visibility attributes are ignored on enums, since as far as we can tell they
-- do not affect the Haskell bindings.
enumDecl :: [C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser
enumDecl _enclosing ctx info = \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      Definition -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        ety       <- fromCXType ctx =<< clang_getEnumDeclIntegerType curr

        -- The underlying type of a C enum is always an integer type, so
        -- clang_getEnumDeclIntegerType only returns TypePrim (e.g. unsigned
        -- int) or TypeTypedef wrapping one (e.g. uint8_t). The fallback to
        -- Signed is conservative and should be unreachable in practice.
        --
        let enumSign :: C.PrimSign
            enumSign = go ety
              where
                go (C.TypePrim pt)      = C.primTypeSign pt
                go (C.TypeTypedef ref)  = go ref.underlying
                go _                    = C.Signed

        let parseConstant ::
              Fold ParseDecl (Either DelayedParseMsg (C.EnumConstant Parse))
            parseConstant = simpleFold $ \curr' -> do
                mKind <- fromSimpleEnum <$> clang_getCursorKind curr'
                case mKind of
                  Right CXCursor_EnumConstantDecl ->
                    fmap Right <$> enumConstantDecl enumSign curr'
                  Right CXCursor_PackedAttr ->
                    foldContinue
                  -- @visibility@ attributes. The visibility itself the value can be
                  -- obtained using 'getCursorVisibility'.
                  Right CXCursor_VisibilityAttr -> foldContinue
                  unexpectedKind -> foldContinueWith
                    (Left $ ParseUnexpectedCursorKind unexpectedKind)

        let mkEnum ::
                 [Either DelayedParseMsg (C.EnumConstant Parse)]
              -> ParseDecl [ParseResult Parse]
            mkEnum eConstants = case partitionEithers eConstants of
              ([], constants) -> pure $ (:[]) $ parseSucceed $ C.Decl{
                  info = info
                , ann  = NoAnn
                , kind = C.DeclEnum C.Enum{
                             typ       = ety
                           , sizeof    = fromIntegral sizeof
                           , alignment = fromIntegral alignment
                           , constants = constants
                           , ann       = NoAnn
                           }
                }
              -- If there are errors, report the first one.
              (msg:_, _) -> parseFail ctx info.id info.loc msg

        foldRecurseWith parseConstant mkEnum
      DefinitionUnavailable -> do
        let decl :: C.Decl Parse
            decl = C.Decl{
                info = info
              , kind = C.DeclOpaque
              , ann  = NoAnn
              }
        foldContinueWith [parseSucceed decl]
      DefinitionElsewhere _ ->
        foldContinue

enumConstantDecl ::
     C.PrimSign
  -> CXCursor
  -> ParseDecl (Next ParseDecl (C.EnumConstant Parse))
enumConstantDecl sign = \curr -> do
    enumConstantInfo  <- getFieldInfo curr
    enumConstantValue <- case sign of
      C.Unsigned -> toInteger <$> clang_getEnumConstantDeclUnsignedValue curr
      C.Signed   -> toInteger <$> clang_getEnumConstantDeclValue curr
    foldContinueWith C.EnumConstant {
        info  = enumConstantInfo
      , value = enumConstantValue
      }

functionDecl :: [C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser
functionDecl enclosing ctx info =
    withCursorVisibility ctx info $ \visibility ->
      withCursorLinkage ctx info $ \linkage ->
        aux visibility linkage
  where
    aux :: Visibility -> Linkage -> Parser
    aux visibility linkage curr = do
      declCls    <- HighLevel.classifyDeclaration curr
      typ        <- fromCXType ctx =<< clang_getCursorType curr
      guardTypeFunction curr typ >>= \case
        Left rs -> foldContinueWith rs
        Right (functionArgs, functionRes) -> do
          functionAnn <- getReparseInfo curr
          let mkDecl :: C.FunctionPurity -> C.Decl Parse
              mkDecl purity = C.Decl{
                  info = info
                , ann  = NoAnn
                , kind = C.DeclFunction C.Function {
                             args  = functionArgs
                           , res   = functionRes
                           , attrs = C.FunctionAttributes purity
                           , ann   = functionAnn
                           }
                }

          case declCls of
            -- The header contains a definition elsewhere, but it is not the
            -- declaration that the cursor is currently pointing to. Skip this
            -- declaration.
            DefinitionElsewhere _->
              foldContinue
            _ -> foldRecurseWith nestedDecl $ \nestedDecls -> do
              let declsAndAttrs = concat nestedDecls
                  (parseRs, attrs) = partitionEithers declsAndAttrs
                  (fails, decls)   = partitionEithers $
                                       map getParseResultEitherDecl parseRs
                  purity = C.decideFunctionPurity attrs
                  (anonDecls, otherDecls) = partitionAnonDecls decls

              -- This declaration may act as a definition.
              let isDefn = declCls == Definition

              (fails ++) <$>
                if not (null anonDecls) then do
                  parseFail ctx info.id info.loc ParseUnsupportedAnonInSignature
                else
                  let nonPublicVisibility = [
                          ParseNonPublicVisibility
                        | visibilityCanCauseErrors visibility linkage isDefn
                        ]
                      potentialDuplicate = [
                          ParsePotentialDuplicateSymbol (visibility == PublicVisibility)
                        | isDefn && linkage == ExternalLinkage
                        ]
                      msgs = nonPublicVisibility ++ potentialDuplicate
                      funDeclResult = parseSucceedWith msgs (mkDecl purity)
                  in pure $ map parseSucceed otherDecls ++ [funDeclResult]

    guardTypeFunction ::
         CXCursor
      -> C.Type Parse
      -> ParseDecl (
            Either
              [ParseResult Parse]
              ([C.FunctionArg Parse], C.Type Parse)
          )
    guardTypeFunction curr ty =
        case ty of
          C.TypeFun args res -> do
            args' <- forM (zip args [0 :: Int ..]) $ \(argCType, i) -> do
              argCursor <- clang_Cursor_getArgument curr i

              argName <- clang_getCursorDisplayName argCursor
              let mbArgName =
                    if Text.null argName
                       then Nothing
                       else Just (CScopedName argName)

              return C.FunctionArg {
                  name = mbArgName
                , argTyp = argCType
                }
            pure $ Right (args', res)
          C.TypeTypedef{} ->
            Left <$>
              (parseFail ctx info.id info.loc ParseFunctionOfTypeTypedef)
          otherType ->
            Left <$>
              (parseFail ctx info.id info.loc $
                 ParseExpectedFunctionType $ show otherType)

    -- Look for (unsupported) declarations inside function parameters, and for
    -- function attributes. Function attributes are returned separately, so that
    -- we can pair them with the parent function.
    nestedDecl :: Fold ParseDecl [Either (ParseResult Parse) C.FunctionPurity]
    nestedDecl = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        let enclosing' :: [C.EnclosingRef Parse]
            enclosing' = C.EnclosingRef info.id : enclosing
        case kind of
          -- 'ParmDecl' sometimes appear in nested in the AST
          Right CXCursor_ParmDecl ->
            foldRecurseWith nestedDecl (return . concat)

          -- Nested declarations
          Right CXCursor_StructDecl -> fmap (fmap Left) <$> parseDeclNested enclosing' ctx curr
          Right CXCursor_UnionDecl  -> fmap (fmap Left) <$> parseDeclNested enclosing' ctx curr

          -- Harmless
          Right CXCursor_TypeRef        -> foldContinue
          Right CXCursor_IntegerLiteral -> foldContinue
          Right CXCursor_UnexposedAttr  -> foldContinue

          -- @const@ and @pure@ function attributes.
          Right CXCursor_ConstAttr -> foldContinueWith $ [Right C.HaskellPureFunction]
          Right CXCursor_PureAttr  -> foldContinueWith $ [Right C.CPureFunction]

          -- @visibility@ attributes. The visibility itself the value is
          -- obtained using 'getCursorVisibility'.
          Right CXCursor_VisibilityAttr -> foldContinue

          -- Attributes we (probably?) want to ignore
          Right CXCursor_WarnUnusedResultAttr -> foldContinue

          -- Function body
          Right CXCursor_CompoundStmt -> foldContinue

          -- We are not interested in assembler labels.
          Right CXCursor_AsmLabelAttr -> foldContinue

          -- Fail on anything else. We could instead use 'foldContinue' here,
          -- but this is safer.
          otherKind -> do
            failures <-
              parseFail ctx info.id info.loc
                (ParseUnexpectedCursorKind otherKind)
            foldContinueWith $ map Left failures

-- | Global variable declaration
varDecl :: [C.EnclosingRef Parse] -> ParseCtx -> C.DeclInfo Parse -> Parser
varDecl enclosing ctx info = do
    withCursorVisibility ctx info $ \visibility ->
      withCursorLinkage ctx info $ \linkage ->
        aux visibility linkage
  where
    aux :: Visibility -> Linkage -> Parser
    aux visibility linkage curr = do
      declCls    <- HighLevel.classifyDeclaration curr
      typ        <- fromCXType ctx =<< clang_getCursorType curr
      cls        <- classifyVarDecl curr
      let mkDecl :: C.DeclKind Parse -> C.Decl Parse
          mkDecl kind = C.Decl{
              info = info
            , kind = kind
            , ann  = NoAnn
            }

      case declCls of
        -- The header contains a definition elsewhere, but it is not the
        -- declaration that the cursor is currently pointing to. Skip this
        -- declaration.
        DefinitionElsewhere _->
          foldContinue
        _ -> foldRecurseWith nestedDecl $ \nestedRs -> do
          let
            (fails, nestedDecls)    = partitionEithers $
                                        map getParseResultEitherDecl $
                                          concat nestedRs
            (anonDecls, otherDecls) = partitionAnonDecls nestedDecls

          -- This declaration may act as a definition even if it has no
          -- initialiser.
          isTentative <- HighLevel.classifyTentativeDefinition curr
          let isDefn = declCls == Definition
                     || (isTentative && declCls == DefinitionUnavailable)

          (fails ++) <$>
            let nonPublicVisibility = [
                    ParseNonPublicVisibility
                  | visibilityCanCauseErrors visibility linkage isDefn
                  ]
                potentialDuplicate = [
                    ParsePotentialDuplicateSymbol (visibility == PublicVisibility)
                  | isDefn && linkage == ExternalLinkage
                  ]
                msgs = nonPublicVisibility ++ potentialDuplicate
             in case cls of
                  VarGlobal IsExtern
                    | not (null anonDecls) -> do
                      parseFail ctx info.id info.loc ParseUnsupportedAnonInExtern
                  VarGlobal _ -> do
                    globalAnn <- getReparseInfo curr
                    pure $ (map parseSucceed (anonDecls ++ otherDecls) ++) $
                      singleton $ parseSucceedWith msgs $
                        mkDecl $ C.DeclGlobal C.Global{
                            typ = typ
                          , ann = globalAnn
                          }
                  VarThreadLocal ->
                    parseFail ctx info.id info.loc ParseUnsupportedTLS
                  VarUnsupported storage ->
                    parseFail ctx info.id info.loc $ ParseUnknownStorageClass storage
    -- Look for nested declarations inside the global variable type
    nestedDecl :: Fold ParseDecl [ParseResult Parse]
    nestedDecl =
        let enclosing' = C.EnclosingRef info.id : enclosing
        in simpleFold $ withCursorKind ctx $ \case
          -- Reference to previously declared type can safely be skipped
          CXCursor_TypeRef -> skip

          -- Nested /new/ declarations
          CXCursor_StructDecl -> parseDeclNested enclosing' ctx
          CXCursor_UnionDecl  -> parseDeclNested enclosing' ctx
          CXCursor_EnumDecl   -> parseDeclNested enclosing' ctx

          -- Initializers
          --
          -- It's a bit annoying that we have to explicitly enumerate them, but
          -- a catch-all may result in us ignoring nodes that we shouldn't.
          --
          -- The order here roughly matches the order of 'CXCursor'.
          -- <https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013>
          CXCursor_IntegerLiteral      -> skip
          CXCursor_FloatingLiteral     -> skip
          CXCursor_ImaginaryLiteral    -> skip
          CXCursor_StringLiteral       -> skip
          CXCursor_ParenExpr           -> skip
          CXCursor_UnaryOperator       -> skip
          CXCursor_BinaryOperator      -> skip
          CXCursor_ConditionalOperator -> skip
          CXCursor_CStyleCastExpr      -> skip
          CXCursor_InitListExpr        -> skip
          CXCursor_CXXBoolLiteralExpr  -> skip -- Since C23

          -- Some initializers are \"unexposed\".
          --
          -- Not sure exactly when clang decides to expose an expression and
          -- when it does not, but it seems it does this when constructing
          -- /pointers/ to values, such as
          --
          -- > char* x = "hi";              // CXCursor_StringLiteral?
          -- > int*  y = (int []){2, 4, 6}; // CXCursor_CompoundLiteralExpr?
          --
          -- String literals /are/ exposed when declaring an array:
          --
          -- > char z = "hi";
          --
          -- The only other example I'm currently aware of is characters
          -- ('CXCursor_CharacterLiteral').
          CXCursor_UnexposedExpr -> skip
          CXCursor_DeclRefExpr   -> skip

          -- @visibility@ attributes, where the value is obtained using
          -- @clang_getCursorVisibility@.
          CXCursor_VisibilityAttr -> skip

          -- We are not interested in assembler labels.
          CXCursor_AsmLabelAttr -> skip

          -- Function types
          CXCursor_ParmDecl -> skip

          -- Fail on anything we don't recognize
          otherKind -> \_curr -> do
            failures <-
              parseFail ctx info.id info.loc
                (ParseUnexpectedCursorKind $ Right otherKind)
            foldContinueWith failures

    skip :: MonadIO m => b -> m (Next m a)
    skip = const foldContinue

{-------------------------------------------------------------------------------
  Utility: dispatching based on the cursor kind
-------------------------------------------------------------------------------}

-- | Fail safely due to an unrecognized cursor kind
--
-- Assemble a parse failure
failUnrecognizedKind ::
  ParseCtx -> Either CInt CXCursorKind -> CXCursor -> ParseDecl [ParseResult Parse]
failUnrecognizedKind ctx eKind curr =
    let msg = ParseUnexpectedCursorKind eKind
    in  parseFailNoInfo ctx msg curr

-- | Obtain cursor kind and run continuation
--
-- Only run continuation if the cursor kind can be obtain, otherwise fail with a
-- @ParseFailure@.
withCursorKind ::
     ParseCtx
  -> (CXCursorKind -> Parser)
  -> Parser
withCursorKind ctx k = \curr -> do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    case mKind of
      Right kind -> k kind curr
      Left  i    -> failUnrecognizedKind ctx (Left i) curr >>= foldContinueWith

{-------------------------------------------------------------------------------
  Info that we collect for all declarations
-------------------------------------------------------------------------------}

-- | Parse with declaration info
--
-- The continuation is only called when the declaration info can be determined.
--
-- Must not be called on built-ins.
withDeclInfo ::
  [C.EnclosingRef Parse] -> ParseCtx -> (C.DeclInfo Parse -> Parser) -> Parser
withDeclInfo enclosing ctx k = \curr -> do
    declId          <- PrelimDeclId.atCursor curr ctx.inner.kind
    declLoc         <- HighLevel.clang_getCursorLocation' curr
    (withHeaderInfo ctx declId declLoc $ \headerInfo ->
      withAvailability ctx declId declLoc $ \availability curr' -> do
        let info :: C.DeclInfo Parse
            info = C.DeclInfo{
                  loc          = declLoc
                , id           = declId
                  -- We initialize the sequence number to 'Nothing', and
                  -- populate it when consolidating macros with non-macros in
                  -- "HsBindgen.Frontend.Pass.Parse".
                , seqNr        = Nothing
                , headerInfo   = headerInfo
                , availability = availability
                , comment      = ()
                , enclosing    = enclosing
                }
        k info curr') curr

-- | Continue with availability
--
-- The continuation is only called when the availability can be determined.
withAvailability ::
     ParseCtx
  -> PrelimDeclId
  -> SingleLoc
  -> (C.Availability -> Parser)
  -> Parser
withAvailability ctx declId declLoc k = \curr -> do
    sAvailability  <- clang_getCursorAvailability curr
    let mAvailability :: Maybe C.Availability
        mAvailability = fmap toAvailability $ fromSimple $ sAvailability
    case mAvailability of
      Nothing -> do
        failures <-
          parseFail ctx declId declLoc $
            ParseUnknownCursorAvailability sAvailability
        foldContinueWith failures
      Just availability -> k availability curr
  where
    fromSimple :: IsSimpleEnum a => SimpleEnum a -> Maybe a
    fromSimple x = either (const Nothing) Just $ fromSimpleEnum x

    toAvailability :: CXAvailabilityKind -> C.Availability
    toAvailability = \case
      CXAvailability_Available     -> C.Available
      CXAvailability_Deprecated    -> C.Deprecated
      CXAvailability_NotAvailable  -> C.Unavailable
      CXAvailability_NotAccessible -> C.Unavailable

-- | Continue with header information
--
-- The continuation is only called when the header information can be determined.
withHeaderInfo ::
     ParseCtx
  -> PrelimDeclId
  -> SingleLoc
  -> (C.HeaderInfo -> Parser)
  -> Parser
withHeaderInfo ctx declId declLoc k = \curr -> do
  eRes <- evalGetMainHeadersAndInclude (singleLocPath declLoc)
  case eRes of
    Left err -> do
      failures <- parseFail ctx declId declLoc err
      foldContinueWith failures
    Right res ->
      k (uncurry aux res) curr
  where
    aux :: NonEmpty HashIncludeArg -> IncludeGraph.Include -> C.HeaderInfo
    aux mainHeaders include = C.HeaderInfo{
        mainHeaders     = mainHeaders
      , includeArg      = IncludeGraph.getIncludeArg      include
      , includeMacroArg = IncludeGraph.getIncludeMacroArg include
      }

-- | The linkage of a linker symbol determines whether or not a linker symbol is
-- visible to the linker outside the translation unit it is defined in.
--
-- See the section on Visibility in the manual for more details.
data Linkage =
    InternalLinkage
  | NoLinkage
  | ExternalLinkage
  deriving stock (Show, Eq, Generic)

-- | Retrieve the linkage of the entity that the cursor is currently pointing
-- to.
--
-- Only call continuation if linkage can be retrieved.
withCursorLinkage ::
  ParseCtx -> C.DeclInfo Parse -> (Linkage -> Parser) -> Parser
withCursorLinkage ctx info k = \curr -> do
    simpleLinkage   <- clang_getCursorLinkage curr
    case fromSimpleLinkage simpleLinkage of
      Left err -> parseFail ctx info.id info.loc err >>= foldContinueWith
      Right l  -> k l curr
  where
    fromSimpleLinkage :: SimpleEnum CXLinkageKind -> Either DelayedParseMsg Linkage
    fromSimpleLinkage simpleLinkage =
      case fromSimpleEnum simpleLinkage of
        Right linkage' -> case linkage' of
          CXLinkage_Invalid ->
            Left ParseInvalidLinkage
          CXLinkage_NoLinkage ->
            Right NoLinkage
          CXLinkage_Internal ->
            Right InternalLinkage
          CXLinkage_UniqueExternal ->
            Left $ ParseUnsupportedLinkage "C++ specific" linkage'
          CXLinkage_External ->
            Right ExternalLinkage
        Left x -> do
          Left $ ParseUnexpectedLinkage (Left x)

-- | The visibility of a linker symbol determines whether or not a linker symbol
-- is visible to the linker outside of the shared object that it is defined in.
--
-- See the section on Visibility in the manual for more details.
data Visibility =
    PublicVisibility
  | NonPublicVisibility
  deriving stock (Show, Eq, Generic)

-- | Retrieve the visibility of the entity that the cursor is currently pointing
-- to.
--
-- Only call continuation if cursor visibility can be retrieved.
withCursorVisibility :: ParseCtx -> C.DeclInfo Parse -> (Visibility -> Parser) -> Parser
withCursorVisibility ctx info k = \curr -> do
    simpleVisibility  <- clang_getCursorVisibility curr
    case fromSimpleVisibility simpleVisibility of
      Left err -> parseFail ctx info.id info.loc err >>= foldContinueWith
      Right v  -> k v curr
  where
    fromSimpleVisibility :: SimpleEnum CXVisibilityKind -> Either DelayedParseMsg Visibility
    fromSimpleVisibility simpleVisibility =
      case fromSimpleEnum simpleVisibility of
        -- See https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gaf92fafb489ab66529aceab51818994cb
        Right vis' -> case vis' of
          -- Despite the name, /default/ always means public.
          CXVisibility_Default ->
            Right PublicVisibility
          CXVisibility_Hidden ->
            Right NonPublicVisibility
          -- This visibility is rarely useful in practice. For binding generation,
          -- we treat it as non-public visibility.
          CXVisibility_Protected ->
            Right NonPublicVisibility
          CXVisibility_Invalid ->
            Left $ ParseInvalidVisibility
        Left x ->
          Left $ ParseUnexpectedVisibility (Left x)


{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Partition declarations into anonymous and non-anonymous
--
-- We are only interested in the name of the declaration /itself/; if a named
-- declaration /contains/ anonymous declarations, that's perfectly fine.
partitionAnonDecls :: [C.Decl Parse] -> ([C.Decl Parse], [C.Decl Parse])
partitionAnonDecls =
    List.partition $ \decl -> declIdIsAnon decl.info.id
  where
    declIdIsAnon :: PrelimDeclId -> Bool
    declIdIsAnon PrelimDeclId.Anon{} = True
    declIdIsAnon _otherwise          = False

-- | Parse macro tokens
--
-- We use @c-expr-dsl@ to parse the macro tokens into a 'CExpr.DSL.Macro'.
-- No type environment is needed for this step; type resolution and
-- expression typechecking happen later in 'TypecheckMacros'.
parseMacroTokens ::
     ClangCStandard
  -> PrelimDeclId
  -> [Token TokenSpelling]
  -> Either DelayedParseMsg ParsedMacro
parseMacroTokens cStd name = \case
    []      -> Left $ ParseMacroEmpty name []
    [token] -> Left $ ParseMacroEmpty name [token]
    tokens  ->
      case CExpr.DSL.runParser (CExpr.DSL.parseMacro cStd) tokens of
        Right macro ->
          Right $ ParsedMacro macro
        Left errParse ->
          Left $ ParseMacroErrorParse errParse

-- | Whether a global variable has @extern@ storage class
--
-- This is only used locally during parsing to reject extern declarations with
-- anonymous types; it is not propagated into the AST.
data IsExtern = IsExtern | IsNotExtern
  deriving stock (Show)

data VarClassification =
    -- | Global variable (mutable or const)
    --
    -- > extern int simpleGlobal;
    -- > extern const int globalConstant;
    -- > static const int staticConst = 123;
    --
    -- NOTE: @static@ can be useful to be able to specify the /value/ of the
    -- constant in the header file (perhaps so that the compiler can inline it).
    -- Without @const@, @static@ results in a mutable variable local to any C
    -- file that includes the header, invisible to the C API. Arguably this does
    -- not make much sense, but it does occur in real-world code (e.g. device
    -- driver headers), so we accept it nonetheless.
    --
    -- TODO <https://github.com/well-typed/hs-bindgen/issues/829>
    -- We could in principle expose the /value/ of the constant, if we know it.
    VarGlobal IsExtern

    -- | Thread local variables
    --
    -- We don't currently support thread-local variables.
    -- <https://github.com/well-typed/hs-bindgen/issues/828>
    --
    -- This is a special case of 'VarUnsupported', for better error reporting.
  | VarThreadLocal

    -- | Unsupported storage class
  | VarUnsupported (SimpleEnum CX_StorageClass)
  deriving stock (Show)

classifyVarDecl :: MonadIO m => CXCursor -> m VarClassification
classifyVarDecl = \curr -> do
    tls <- clang_getCursorTLSKind curr
    case fromSimpleEnum tls of
      Right CXTLS_None -> do
        storage <- clang_Cursor_getStorageClass curr
        case fromSimpleEnum storage of
          Right CX_SC_Extern  -> return $ VarGlobal IsExtern
          Right CX_SC_None    -> return $ VarGlobal IsNotExtern
          Right CX_SC_Static  -> return $ VarGlobal IsNotExtern
          _otherwise -> return $ VarUnsupported storage
      _otherwise ->
        return VarThreadLocal

-- | Check if a function declaration or global variable declaration has a
-- problematic case of non-public visibility.
--
-- See the section on Visibility in the manual for more details.
visibilityCanCauseErrors ::
     Visibility
  -> Linkage
  -> Bool
     -- ^ Whether the declaration acts as a definition
     --
     -- Tentative definitions can also act as definitions if there are no full
     -- definitions in scope.
  -> Bool
visibilityCanCauseErrors NonPublicVisibility ExternalLinkage False = True
visibilityCanCauseErrors _ _ _ = False
