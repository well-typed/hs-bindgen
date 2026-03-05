-- | Fold declarations
module HsBindgen.Frontend.Pass.Parse.Decl (topLevelDecl) where

import Control.Exception (Exception (..), SomeException)
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Text qualified as Text
import Foreign.C (CInt)

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Context
import HsBindgen.Frontend.Pass.Parse.Decl.Monad
import HsBindgen.Frontend.Pass.Parse.IsPass
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
topLevelDecl :: Fold ParseDecl [ParseResult Parse]
topLevelDecl = foldWithHandler handleParseExceptions (parseDecl' Nothing)
  where
    handleParseExceptions ::
         CXCursor
      -> SomeException
      -> ParseDecl (Maybe [ParseResult Parse])
    handleParseExceptions curr err
      | Just e <- fromException @(ExceptionInCtx ParseMsg) err = do
          Just <$> parseFailNoInfo e.ctx e.exception curr
      | otherwise = liftIO $ throwIO err

{-------------------------------------------------------------------------------
  Functions for each kind of declaration
-------------------------------------------------------------------------------}

type Parser = CXCursor -> ParseDecl (Next ParseDecl [ParseResult Parse])

-- | Parse declarations with available parse context
parseDecl :: ParseCtx -> Parser
parseDecl = parseDecl' . Just

-- | Parse declarations, possibly without a parse context
parseDecl' :: HasCallStack => Maybe ParseCtx -> Parser
parseDecl' mCtx = withCursorKind' $ \case
      -- Ordinary kinds that we parse
      CXCursor_FunctionDecl    -> parseDeclWith (push CNameKindOrdinary NotRequiredForScoping) functionDecl
      CXCursor_VarDecl         -> parseDeclWith (push CNameKindOrdinary NotRequiredForScoping) varDecl
      CXCursor_TypedefDecl     -> parseDeclWith (push CNameKindOrdinary RequiredForScoping)    typedefDecl
      CXCursor_MacroDefinition -> parseDeclWith (push CNameKindMacro    NotRequiredForScoping) macroDefinition

      -- Tagged kinds that we parse
      CXCursor_StructDecl -> parseDeclWith (push (CNameKindTagged CTagKindStruct) NotRequiredForScoping) structDecl
      CXCursor_UnionDecl  -> parseDeclWith (push (CNameKindTagged CTagKindUnion)  NotRequiredForScoping) unionDecl
      CXCursor_EnumDecl   -> parseDeclWith (push (CNameKindTagged CTagKindEnum)   NotRequiredForScoping) enumDecl

      -- Process macro expansions independent of any select predicates
      CXCursor_MacroExpansion -> macroExpansion

      -- Kinds that we skip over
      CXCursor_AlignedAttr        -> \_curr -> foldContinue
      CXCursor_InclusionDirective -> \_curr -> foldContinue
      CXCursor_PackedAttr         -> \_curr -> foldContinue
      CXCursor_UnexposedAttr      -> \_curr -> foldContinue
      CXCursor_UnexposedDecl      -> \_curr -> foldContinue

      -- Report error for declarations we don't recognize
      kind -> case mCtx of
        Nothing  ->
          unavoidablePanicUnrecognizedKind (Right kind)
        Just ctx ->
          failUnrecognizedKind ctx (Right kind) >=> foldContinueWith
  where
    push :: CNameKind -> RequiredForScoping -> ParseCtx
    push kind scoping =
      let ctx = DeclCtx kind scoping
      in  case mCtx of
            Nothing     -> mkCtx   ctx
            Just ctxOld -> pushCtx ctx ctxOld

    withCursorKind' :: (CXCursorKind -> Parser) -> Parser
    withCursorKind' = case mCtx of
      Nothing  -> unsafeWithCursorKind
      Just ctx -> withCursorKind ctx

-- | Parse declaration
--
-- NOTE: We currently skip all built-ins. The only built-ins that can even
-- /have/ an associated declaration at all are macros. However, since we cannot
-- get the list of tokens for built-in macros, we would anyway need to
-- special-case them. For now we skip /all/ builtins.
parseDeclWith :: ParseCtx -> (ParseCtx -> C.DeclInfo Parse -> Parser) -> Parser
parseDeclWith ctx parser curr = do
    mBuiltin <- PrelimDeclId.checkIsBuiltin curr
    case mBuiltin of
      Just _name -> foldContinue
      Nothing    -> withDeclInfo ctx parseExplicitDecl curr
  where
    parseExplicitDecl :: C.DeclInfo Parse -> Parser
    parseExplicitDecl info = \_curr ->
      if | C.Unavailable <- info.availability ->
             foldContinueWith [
                 parseDoNotAttempt info DeclarationUnavailable
               ]
         | RequiredForScoping <- ctx.inner.scoping ->
             parser ctx info curr
         | otherwise -> do
             matched <- evalPredicate info
             if matched then
               parser ctx info curr
             else
               foldContinueWith [
                 parseDoNotAttempt info ParsePredicateNotMatched
               ]

-- | Macros
--
-- In this phase, we return macro declarations simply as a list of tokens. We
-- will parse them later (after sorting all declarations in the file).
macroDefinition :: HasCallStack => ParseCtx -> C.DeclInfo Parse -> Parser
macroDefinition _ctx info = \curr -> do
    decl <- mkDecl <$> getUnparsedMacro curr
    foldContinueWith [parseSucceed decl]
  where
    mkDecl :: UnparsedMacro -> C.Decl Parse
    mkDecl body = C.Decl{
          info = info
        , kind = C.DeclMacro body
        , ann  = NoAnn
        }

    getUnparsedMacro :: CXCursor -> ParseDecl UnparsedMacro
    getUnparsedMacro curr = do
        unit <- getTranslationUnit
        range  <- HighLevel.clang_getCursorExtent curr
        tokens <- HighLevel.clang_tokenize unit (multiLocExpansion <$> range)
        return $ UnparsedMacro tokens

structDecl :: ParseCtx -> C.DeclInfo Parse -> Parser
structDecl ctx info = \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      Definition -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty

        let mkStruct :: [C.StructField Parse] -> C.Decl Parse
            mkStruct allFields = C.Decl {
                  info = info
                , ann  = NoAnn
                , kind = C.DeclStruct C.Struct{
                             sizeof    = fromIntegral sizeof
                           , alignment = fromIntegral alignment
                           , fields    = regularFields
                           , flam      = mFlam
                           , ann       = NoAnn
                           }
                }
              where
                (regularFields, mFlam) = partitionFields allFields

        -- Separate out nested declarations from regular struct fields
        --
        -- Local declarations inside structs that are not used by any fields
        -- result in implicit fields. Unfortunately, @libclang@ does not make
        -- these visible <https://github.com/llvm/llvm-project/issues/122257>.
        -- This matters, because we need the offsets of these implicit fields.
        -- For now we therefore only try to detect the situation and report an
        -- error when it happens. Hopefully this is anyway very rare.
        let partitionChildren ::
                 [C.Decl Parse] -> [C.StructField Parse]
              -> ParseDecl (Maybe [C.Decl Parse])
            partitionChildren otherDecls fields
              | null unused = pure $ Just used
              | otherwise   = pure Nothing
              where
                used, unused :: [C.Decl Parse]
                (used, unused) = detectStructImplicitFields otherDecls fields

        foldRecurseWith (declOrFieldDecl ctx $ structFieldDecl ctx) $ \xs -> do
          let (otherRs, fields)   = first concat $ partitionEithers xs
              (fails, otherDecls) = partitionEithers $
                                      map getParseResultEitherDecl otherRs
          mPartitioned <- partitionChildren otherDecls fields
          (fails ++) <$> case mPartitioned of
            Just decls ->
              pure $ map parseSucceed $ decls ++ [mkStruct fields]
            Nothing -> do
              -- If the struct has implicit fields, don't generate anything.
              parseFail ctx info.id info.loc $ Delayed ParseUnsupportedImplicitFields

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

unionDecl :: ParseCtx -> C.DeclInfo Parse -> Parser
unionDecl ctx info = \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      Definition -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty

        let mkUnion :: [C.UnionField Parse] -> C.Decl Parse
            mkUnion fields = C.Decl{
                  info = info
                , ann  = NoAnn
                , kind = C.DeclUnion C.Union{
                             sizeof    = fromIntegral sizeof
                           , alignment = fromIntegral alignment
                           , fields    = fields
                           , ann       = NoAnn
                           }
                }

        -- Separate out nested declarations from regular struct fields
        --
        -- Local declarations inside unions that are not used by any fields
        -- result in implicit fields. Unfortunately, @libclang@ does not make
        -- these visible <https://github.com/llvm/llvm-project/issues/122257>.
        -- We could in principle support it but currently we don't. See #682.
        -- For now we only try to detect the situation and report an error when
        -- it happens. Hopefully this is anyway very rare.
        let partitionChildren ::
                 [C.Decl Parse] -> [C.UnionField Parse]
              -> ParseDecl (Maybe [C.Decl Parse])
            partitionChildren otherDecls fields
              | null unused = pure $ Just used
              | otherwise   = pure Nothing
              where
                used, unused :: [C.Decl Parse]
                (used, unused) = detectUnionImplicitFields otherDecls fields

        foldRecurseWith (declOrFieldDecl ctx $ unionFieldDecl ctx) $ \xs -> do
          let (otherRs, fields)   = first concat $ partitionEithers xs
              (fails, otherDecls) = partitionEithers $
                                      map getParseResultEitherDecl otherRs
          mPartitioned <- partitionChildren otherDecls fields
          (fails ++) <$> case mPartitioned of
            Just decls ->
              pure $ map parseSucceed $ decls ++ [mkUnion fields]
            Nothing -> do
              -- If the union has implicit fields, don't generate anything.
              parseFail ctx info.id info.loc $ Delayed ParseUnsupportedImplicitFields
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

declOrFieldDecl ::
     ParseCtx
  -> (CXCursor -> ParseDecl (a Parse))
  -> Fold ParseDecl (Either [ParseResult Parse] (a Parse))
declOrFieldDecl ctx fieldDecl = simpleFold $ \curr -> do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        field <- fieldDecl curr
        -- Field declarations can have struct declarations as children in the
        -- clang AST; however, those are duplicates of declarations that appear
        -- elsewhere, so here we choose not to recurse.
        foldContinueWith $ Right field
      _otherwise -> do
        fmap Left <$> parseDecl ctx curr

structFieldDecl :: ParseCtx -> CXCursor -> ParseDecl (C.StructField Parse)
structFieldDecl ctx = \curr -> do
    structFieldInfo   <- getFieldInfo curr
    structFieldType   <- fromCXType ctx =<< clang_getCursorType curr
    structFieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField curr
    structFieldAnn    <- getReparseInfo curr
    structFieldWidth  <- structWidth curr
    pure C.StructField{
        info   = structFieldInfo
      , typ    = structFieldType
      , offset = structFieldOffset
      , width  = structFieldWidth
      , ann    = structFieldAnn
      }

structWidth :: CXCursor -> ParseDecl (Maybe Int)
structWidth = \curr -> do
    isBitField <- clang_Cursor_isBitField curr
    if isBitField
      then Just . fromIntegral <$> clang_getFieldDeclBitWidth curr
      else return Nothing

unionFieldDecl :: ParseCtx -> CXCursor -> ParseDecl (C.UnionField Parse)
unionFieldDecl ctx = \curr -> do
    unionFieldInfo <- getFieldInfo curr
    unionFieldType <- fromCXType ctx =<< clang_getCursorType curr
    unionFieldAnn  <- getReparseInfo curr
    pure C.UnionField{
        info = unionFieldInfo
      , typ  = unionFieldType
      , ann  = unionFieldAnn
      }

typedefDecl :: ParseCtx -> C.DeclInfo Parse -> Parser
typedefDecl ctx info = \curr -> do
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
    loc <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    recordMacroExpansionAt loc
    foldContinue

enumDecl :: ParseCtx -> C.DeclInfo Parse -> Parser
enumDecl ctx info = \curr -> do
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
              Fold ParseDecl (Either ImmediateParseMsg (C.EnumConstant Parse))
            parseConstant = simpleFold $ \curr' -> do
                mKind <- fromSimpleEnum <$> clang_getCursorKind curr'
                case mKind of
                  Right CXCursor_EnumConstantDecl ->
                    fmap Right <$> enumConstantDecl enumSign curr'
                  Right CXCursor_PackedAttr ->
                    foldContinue
                  unexpectedKind -> foldContinueWith
                    (Left $ ParseUnexpectedCursorKind unexpectedKind)

        let mkEnum ::
                 [Either ImmediateParseMsg (C.EnumConstant Parse)]
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
              (msg:_, _) -> parseFail ctx info.id info.loc (Immediate msg)

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

functionDecl :: ParseCtx -> C.DeclInfo Parse -> Parser
functionDecl ctx info =
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
                  parseFail ctx info.id info.loc $ Delayed ParseUnsupportedAnonInSignature
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
              (parseFail ctx info.id info.loc $
                 Delayed ParseFunctionOfTypeTypedef)
          otherType ->
            Left <$>
              (parseFail ctx info.id info.loc $
                 Immediate $ ParseExpectedFunctionType $ show otherType)

    -- Look for (unsupported) declarations inside function parameters, and for
    -- function attributes. Function attributes are returned separately, so that
    -- we can pair them with the parent function.
    nestedDecl :: Fold ParseDecl [Either (ParseResult Parse) C.FunctionPurity]
    nestedDecl = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          -- 'ParmDecl' sometimes appear in nested in the AST
          Right CXCursor_ParmDecl ->
            foldRecurseWith nestedDecl (return . concat)

          -- Nested declarations
          Right CXCursor_StructDecl -> fmap (fmap Left) <$> parseDecl ctx curr
          Right CXCursor_UnionDecl  -> fmap (fmap Left) <$> parseDecl ctx curr

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
                (Immediate $ ParseUnexpectedCursorKind otherKind)
            foldContinueWith $ map Left failures

-- | Global variable declaration
varDecl :: ParseCtx -> C.DeclInfo Parse -> Parser
varDecl ctx info = do
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

      -- TODO <https://github.com/well-typed/hs-bindgen/issues/831>
      -- We should support macro types in globals.

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
                      parseFail ctx info.id info.loc $ Delayed ParseUnsupportedAnonInExtern
                  VarGlobal _ ->
                    pure $ (map parseSucceed (anonDecls ++ otherDecls) ++) $
                      singleton $ parseSucceedWith msgs (mkDecl $ C.DeclGlobal typ)
                  VarThreadLocal ->
                    parseFail ctx info.id info.loc $ Delayed ParseUnsupportedTLS
                  VarUnsupported storage ->
                    parseFail ctx info.id info.loc $ Delayed $ ParseUnknownStorageClass storage
    -- Look for nested declarations inside the global variable type
    nestedDecl :: Fold ParseDecl [ParseResult Parse]
    nestedDecl = simpleFold $ withCursorKind ctx $ \case
          -- Reference to previously declared type can safely be skipped
          CXCursor_TypeRef -> skip

          -- Nested /new/ declarations
          CXCursor_StructDecl -> parseDecl ctx
          CXCursor_UnionDecl  -> parseDecl ctx
          CXCursor_EnumDecl   -> parseDecl ctx

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
                (Immediate $ ParseUnexpectedCursorKind $ Right otherKind)
            foldContinueWith failures

    skip :: MonadIO m => b -> m (Next m a)
    skip = const foldContinue

{-------------------------------------------------------------------------------
  Utility: dispatching based on the cursor kind
-------------------------------------------------------------------------------}

-- | Unavoidably panic (!) on an unrecognized cursor kind
--
-- Only use this function if there is no way to assemble a 'ParseResult'.
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

-- | Fail safely due to an unrecognized cursor kind
--
-- Assemble a parse failure
failUnrecognizedKind ::
  ParseCtx -> Either CInt CXCursorKind -> CXCursor -> ParseDecl [ParseResult Parse]
failUnrecognizedKind ctx eKind curr =
    let msg = Immediate (ParseUnexpectedCursorKind eKind)
    in  parseFailNoInfo ctx msg curr

-- | Obtain cursor kind and run continuation
--
-- Without a 'ParseCtx', we cannot attach a possible error to the declaration,
-- and so we must panic.
--
-- Use 'withCursorKind' instead, if possible.
unsafeWithCursorKind ::
     (CXCursorKind -> Parser)
  -> Parser
unsafeWithCursorKind k = \curr -> do
    mKind <- fromSimpleEnum <$> clang_getCursorKind curr
    case mKind of
      Right kind -> k kind curr
      Left  i    -> unavoidablePanicUnrecognizedKind (Left i) curr

-- | Obtain cursor kind and run continuation
--
-- Only run continuation if the cursor kind can be obtain, otherwise fail with a
-- 'ParseFailure'.
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
withDeclInfo :: ParseCtx -> (C.DeclInfo Parse -> Parser) -> Parser
withDeclInfo ctx k = \curr -> do
    declId          <- PrelimDeclId.atCursor curr ctx.inner.kind
    declLoc         <- HighLevel.clang_getCursorLocation' curr
    (withHeaderInfo ctx declId declLoc $ \headerInfo ->
      withAvailability ctx declId declLoc $ \availability curr' -> do
        declComment    <- fmap parseCommentReferences <$> CDoc.clang_getComment curr
        let info :: C.DeclInfo Parse
            info = C.DeclInfo{
                  id           = declId
                , loc          = declLoc
                , headerInfo   = headerInfo
                , availability = availability
                , comment      = declComment
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
          parseFail ctx declId declLoc $ Immediate $
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
      failures <- parseFail ctx declId declLoc $ Immediate err
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

getFieldInfo :: CXCursor -> ParseDecl (C.FieldInfo Parse)
getFieldInfo = \curr -> do
    fieldLoc     <- HighLevel.clang_getCursorLocation' curr
    fieldName    <- CScopedName <$> clang_getCursorDisplayName curr
    fieldComment <- fmap parseCommentReferences <$> CDoc.clang_getComment curr

    return C.FieldInfo {
        loc     = fieldLoc
      , name    = fieldName
      , comment = fieldComment
      }

getReparseInfo :: CXCursor -> ParseDecl ReparseInfo
getReparseInfo = \curr -> do
    extent <- fmap multiLocExpansion <$> HighLevel.clang_getCursorExtent curr
    hasMacroExpansion <- checkHasMacroExpansion extent
    if hasMacroExpansion then do
      unit <- getTranslationUnit
      ReparseNeeded <$> HighLevel.clang_tokenize unit extent
    else
      return ReparseNotNeeded

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

parseCommentReferences :: CDoc.Comment Text -> C.Comment Parse
parseCommentReferences comment = C.Comment (fmap auxRefs comment)
  where
    auxRefs :: Text -> C.CommentRef Parse
    auxRefs ref = C.CommentRef ref Nothing

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

-- | Detect implicit fields inside a struct
--
-- Implicit fields arise from structs that are declared inside an outer struct,
-- but without an explicit reference from any of the fields in that outer
-- struct. Something like this:
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
detectStructImplicitFields ::
     [C.Decl Parse]
     -- ^ Nested declarations inside a struct
  -> [C.StructField Parse]
     -- ^ Fields of the (outer) struct
  -> ([C.Decl Parse], [C.Decl Parse])
detectStructImplicitFields nestedDecls outerFields =
    List.partition declIsUsed nestedDecls
  where
    allFields :: [Either (C.StructField Parse) (C.UnionField Parse)]
    allFields = map Left outerFields ++ concatMap nestedFields nestedDecls

    nestedFields :: C.Decl Parse -> [Either (C.StructField Parse) (C.UnionField Parse)]
    nestedFields decl =
        case decl.kind of
          C.DeclStruct struct -> map Left  struct.fields
          C.DeclUnion union   -> map Right union.fields
          _otherwise          -> []

    fieldDeps :: [PrelimDeclId]
    fieldDeps = map snd $ concatMap (either depsOfField depsOfField) allFields

    declIsUsed :: C.Decl Parse -> Bool
    declIsUsed decl = decl.info.id `elem` fieldDeps

-- | Detect implicit fields inside a union
--
-- Similar to 'detectStructImplicitFields', but for union fields.
-- This function partitions local declarations into those that are referenced by
-- some field ("regular declarations"), and those that are not (that is, the
-- implicit fields).
detectUnionImplicitFields ::
     [C.Decl Parse]
     -- ^ Nested declarations inside a union
  -> [C.UnionField Parse]
     -- ^ Fields of the (outer) union
  -> ([C.Decl Parse], [C.Decl Parse])
detectUnionImplicitFields nestedDecls outerFields =
    List.partition declIsUsed nestedDecls
  where
    allFields :: [Either (C.StructField Parse) (C.UnionField Parse)]
    allFields = map Right outerFields ++ concatMap nestedFields nestedDecls

    nestedFields :: C.Decl Parse -> [Either (C.StructField Parse) (C.UnionField Parse)]
    nestedFields decl =
        case decl.kind of
          C.DeclStruct struct -> map Left  struct.fields
          C.DeclUnion union   -> map Right union.fields
          _otherwise          -> []

    fieldDeps :: [PrelimDeclId]
    fieldDeps = map snd $ concatMap (either depsOfField depsOfField) allFields

    declIsUsed :: C.Decl Parse -> Bool
    declIsUsed decl = decl.info.id `elem` fieldDeps

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
    -- NOTE: `static` can be useful to be able to specify the /value/ of the
    -- constant in the header file (perhaps so that the compiler can inline it).
    -- However, `static` does not make sense without `const`: this would be a
    -- mutable variable, but it would be local to any C file that included the
    -- header; it would be invisible to the C API.
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
        storage   <- clang_Cursor_getStorageClass curr
        typ       <- clang_getCursorType curr
        canonical <- clang_getCanonicalType typ
        isConst   <- clang_isConstQualifiedType canonical
        case (fromSimpleEnum storage, isConst) of
          (Right CX_SC_Extern , _    ) -> return $ VarGlobal IsExtern
          (Right CX_SC_None   , _    ) -> return $ VarGlobal IsNotExtern
          (Right CX_SC_Static , True ) -> return $ VarGlobal IsNotExtern
          _otherwise -> return $ VarUnsupported storage
      _otherwise ->
        return VarThreadLocal

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
    fromSimpleLinkage :: SimpleEnum CXLinkageKind -> Either ParseMsg Linkage
    fromSimpleLinkage simpleLinkage =
      case fromSimpleEnum simpleLinkage of
        Right linkage' -> case linkage' of
          CXLinkage_Invalid ->
            Left $ Delayed ParseInvalidLinkage
          CXLinkage_NoLinkage ->
            Right NoLinkage
          CXLinkage_Internal ->
            Right InternalLinkage
          CXLinkage_UniqueExternal ->
            Left $ Delayed $ ParseUnsupportedLinkage "C++ specific" linkage'
          CXLinkage_External ->
            Right ExternalLinkage
        Left x -> do
          Left $ Immediate $ ParseUnexpectedLinkage (Left x)

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
    fromSimpleVisibility :: SimpleEnum CXVisibilityKind -> Either ParseMsg Visibility
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
            Left $ Delayed $ ParseInvalidVisibility
        Left x ->
          Left $ Immediate $ ParseUnexpectedVisibility (Left x)

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
