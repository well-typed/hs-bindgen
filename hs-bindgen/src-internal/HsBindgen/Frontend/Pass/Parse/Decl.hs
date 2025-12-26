-- | Fold declarations
module HsBindgen.Frontend.Pass.Parse.Decl (topLevelDecl, parseDecl) where

import Data.Either (partitionEithers)
import Data.List qualified as List
import Data.Text qualified as Text

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Documentation qualified as CDoc
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Type qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Decl.Monad
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId)
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.Parse.Type
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeExceptionInContext (..))
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
topLevelDecl = foldWithHandler handleTypeException parseDecl
  where
    handleTypeException ::
         CXCursor
      -> ParseTypeExceptionInContext ParseTypeExceptionContext
      -> ParseDecl (Maybe [ParseResult Parse])
    handleTypeException curr err = do
        declId  <- PrelimDeclId.atCursor curr contextNameKind
        declLoc <- HighLevel.clang_getCursorLocation' curr
        -- TODO https://github.com/well-typed/hs-bindgen/issues/1249: Only emit
        -- the trace when we use the declaration that we fail to parse.
        when (contextRequiredForScoping == RequiredForScoping) $
          recordImmediateTrace declId declLoc $
            ParseOfDeclarationRequiredForScopingFailed (parseException err)
        pure $ Just [
            parseFail declId declLoc $
              ParseUnsupportedType (parseException err)
          ]
      where
        ParseTypeExceptionContext{..} = parseContext err

{-------------------------------------------------------------------------------
  Info that we collect for all declarations
-------------------------------------------------------------------------------}

-- | Get declaration info
--
-- Must not be called on built-ins.
getDeclInfo :: CXCursor -> C.NameKind -> ParseDecl (C.DeclInfo Parse)
getDeclInfo = \curr nameKind -> do
    declId         <- PrelimDeclId.atCursor curr nameKind
    declLoc        <- HighLevel.clang_getCursorLocation' curr
    declHeaderInfo <- getHeaderInfo (singleLocPath declLoc)
    sAvailability  <- clang_getCursorAvailability curr
    declComment    <- fmap parseCommentReferences <$> CDoc.clang_getComment curr

    let mAvailability :: Maybe C.Availability
        mAvailability = fmap toAvailability $ fromSimple $ sAvailability

        -- NOTE: If the availability is unknown/undefined, we set the
        -- declaration to be unavailable. We do not attempt to parse unavailable
        -- declarations and emit an appropriate trace.
        declAvailability :: C.Availability
        declAvailability = fromMaybe C.Unavailable mAvailability

        info :: C.DeclInfo Parse
        info = C.DeclInfo{
              id           = declId
            , loc          = declLoc
            , headerInfo   = declHeaderInfo
            , availability = declAvailability
            , comment      = declComment
            }

    when (isNothing mAvailability) $
      recordImmediateTrace declId declLoc $
        ParseUnknownCursorAvailability sAvailability

    -- TODO: We might want a NameOriginBuiltin.
    pure info
  where
    fromSimple :: IsSimpleEnum a => SimpleEnum a -> Maybe a
    fromSimple x = either (const Nothing) Just $ fromSimpleEnum x

    toAvailability :: CXAvailabilityKind -> C.Availability
    toAvailability = \case
      CXAvailability_Available     -> C.Available
      CXAvailability_Deprecated    -> C.Deprecated
      CXAvailability_NotAvailable  -> C.Unavailable
      CXAvailability_NotAccessible -> C.Unavailable

getHeaderInfo :: SourcePath -> ParseDecl C.HeaderInfo
getHeaderInfo path = uncurry C.HeaderInfo <$> evalGetMainHeadersAndInclude path

getFieldInfo :: CXCursor -> ParseDecl (C.FieldInfo Parse)
getFieldInfo = \curr -> do
    fieldLoc     <- HighLevel.clang_getCursorLocation' curr
    fieldName    <- C.ScopedName <$> clang_getCursorDisplayName curr
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
  Functions for each kind of declaration
-------------------------------------------------------------------------------}

type Parser = CXCursor -> ParseDecl (Next ParseDecl [ParseResult Parse])

-- | Declarations
parseDecl :: HasCallStack => Parser
parseDecl curr = dispatchWithArg curr $ \case
      -- Ordinary kinds that we parse
      CXCursor_FunctionDecl    -> parseDeclWith functionDecl    NotRequiredForScoping C.NameKindOrdinary
      CXCursor_VarDecl         -> parseDeclWith varDecl         NotRequiredForScoping C.NameKindOrdinary
      CXCursor_TypedefDecl     -> parseDeclWith typedefDecl     RequiredForScoping    C.NameKindOrdinary
      CXCursor_MacroDefinition -> parseDeclWith macroDefinition NotRequiredForScoping C.NameKindOrdinary

      -- Tagged kinds that we parse
      CXCursor_StructDecl -> parseDeclWith structDecl NotRequiredForScoping (C.NameKindTagged C.TagKindStruct)
      CXCursor_UnionDecl  -> parseDeclWith unionDecl  NotRequiredForScoping (C.NameKindTagged C.TagKindUnion)
      CXCursor_EnumDecl   -> parseDeclWith enumDecl   NotRequiredForScoping (C.NameKindTagged C.TagKindEnum)

      -- Process macro expansions independent of any select predicates
      CXCursor_MacroExpansion -> macroExpansion

      -- Kinds that we skip over
      CXCursor_AlignedAttr        -> \_ -> foldContinue
      CXCursor_InclusionDirective -> \_ -> foldContinue
      CXCursor_PackedAttr         -> \_ -> foldContinue
      CXCursor_UnexposedAttr      -> \_ -> foldContinue
      CXCursor_UnexposedDecl      -> \_ -> foldContinue

      -- Report error for declarations we don't recognize
      kind -> unknownCursorKind kind

-- | Parse declaration
--
-- NOTE: We currently skip all built-ins. The only built-ins that can even
-- /have/ an associated declaration at all are macros. However, since we cannot
-- get the list of tokens for built-in macros, we would anyway need to
-- special-case them. For now we skip /all/ builtins.
parseDeclWith ::
     (C.DeclInfo Parse -> Parser)
  -> RequiredForScoping
  -> C.NameKind
  -> Parser
parseDeclWith parser requiredForScoping kind curr = do
    mBuiltin <- PrelimDeclId.checkIsBuiltin curr
    case mBuiltin of
      Just _name ->
        foldContinue
      Nothing -> do
        info <- getDeclInfo curr kind
        if | C.Unavailable <- info.availability ->
               foldContinueWith [
                   parseDoNotAttempt info DeclarationUnavailable
                 ]
           | RequiredForScoping <- requiredForScoping ->
               parser info curr
           | otherwise -> do
               matched <- evalPredicate info
               if matched then
                 parser info curr
               else
                 foldContinueWith [
                   parseDoNotAttempt info ParsePredicateNotMatched
                 ]

-- | Macros
--
-- In this phase, we return macro declarations simply as a list of tokens. We
-- will parse them later (after sorting all declarations in the file).
macroDefinition :: HasCallStack => C.DeclInfo Parse -> Parser
macroDefinition info = \curr -> do
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

structDecl :: C.DeclInfo Parse -> Parser
structDecl info = \curr -> do
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

        foldRecurseWith (declOrFieldDecl $ structFieldDecl info) $ \xs -> do
          let (otherRs, fields)   = first concat $ partitionEithers xs
              (fails, otherDecls) = partitionEithers $
                                      map getParseResultEitherDecl otherRs
          mPartitioned <- partitionChildren otherDecls fields
          pure $ (fails ++) $ case mPartitioned of
            Just decls ->
              map parseSucceed $ decls ++ [mkStruct fields]
            Nothing -> [
                -- If the struct has implicit fields, don't generate anything.
                parseFail info.id info.loc $
                  ParseUnsupportedImplicitFields
              ]
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

unionDecl :: C.DeclInfo Parse -> Parser
unionDecl info = \curr -> do
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

        foldRecurseWith (declOrFieldDecl $ unionFieldDecl info) $ \xs -> do
          let (otherRs, fields)   = first concat $ partitionEithers xs
              (fails, otherDecls) = partitionEithers $
                                      map getParseResultEitherDecl otherRs
          mPartitioned <- partitionChildren otherDecls fields
          pure $ (fails ++) $ case mPartitioned of
            Just decls ->
              map parseSucceed $ decls ++ [mkUnion fields]
            Nothing -> [
                -- If the union has implicit fields, don't generate anything.
                parseFail info.id info.loc $
                  ParseUnsupportedImplicitFields
              ]
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
     (CXCursor -> ParseDecl (a Parse))
  -> Fold ParseDecl (Either [ParseResult Parse] (a Parse))
declOrFieldDecl fieldDecl = simpleFold $ \curr -> do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        field <- fieldDecl curr
        -- Field declarations can have struct declarations as children in the
        -- clang AST; however, those are duplicates of declarations that appear
        -- elsewhere, so here we choose not to recurse.
        foldContinueWith $ Right field
      _otherwise -> do
        fmap Left <$> parseDecl curr

structFieldDecl :: C.DeclInfo Parse -> CXCursor -> ParseDecl (C.StructField Parse)
structFieldDecl info = \curr -> do
    structFieldInfo   <- getFieldInfo curr
    structFieldType   <-
      let ctx = ParseTypeExceptionContext
                  info
                  (C.NameKindTagged C.TagKindStruct)
                  NotRequiredForScoping
      in fromCXType' ctx
        =<< clang_getCursorType curr
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

unionFieldDecl :: C.DeclInfo Parse -> CXCursor -> ParseDecl (C.UnionField Parse)
unionFieldDecl info = \curr -> do
    unionFieldInfo <- getFieldInfo curr
    unionFieldType <-
      let ctx = ParseTypeExceptionContext
                  info
                  (C.NameKindTagged C.TagKindUnion)
                  NotRequiredForScoping
      in fromCXType' ctx
        =<< clang_getCursorType curr
    unionFieldAnn  <- getReparseInfo curr
    pure C.UnionField{
        info = unionFieldInfo
      , typ  = unionFieldType
      , ann  = unionFieldAnn
      }

typedefDecl :: C.DeclInfo Parse -> Parser
typedefDecl info = \curr -> do
    let ctx = ParseTypeExceptionContext
                info
                C.NameKindOrdinary
                RequiredForScoping
    typedefType <- fromCXType' ctx
                     =<< clang_getTypedefDeclUnderlyingType curr

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

enumDecl :: C.DeclInfo Parse -> Parser
enumDecl info = \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      Definition -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        ety       <-
          let ctx = ParseTypeExceptionContext
                      info
                      (C.NameKindTagged C.TagKindEnum)
                      NotRequiredForScoping
          in  fromCXType' ctx
            =<< clang_getEnumDeclIntegerType curr

        let mkEnum :: [C.EnumConstant Parse] -> C.Decl Parse
            mkEnum constants = C.Decl{
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

        foldRecursePure parseConstant ((:[]) . parseSucceed . mkEnum)
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
    parseConstant :: Fold ParseDecl (C.EnumConstant Parse)
    parseConstant = simpleFold $ \curr ->
        dispatch curr $ \case
          CXCursor_EnumConstantDecl -> enumConstantDecl curr
          CXCursor_PackedAttr       -> foldContinue
          kind                      -> unknownCursorKind kind curr

enumConstantDecl :: CXCursor -> ParseDecl (Next ParseDecl (C.EnumConstant Parse))
enumConstantDecl = \curr -> do
    enumConstantInfo  <- getFieldInfo curr
    enumConstantValue <- toInteger <$> clang_getEnumConstantDeclValue curr
    foldContinueWith C.EnumConstant {
        info  = enumConstantInfo
      , value = enumConstantValue
      }

functionDecl :: C.DeclInfo Parse -> Parser
functionDecl info = \curr -> do
    visibility <- getCursorVisibility curr
    linkage    <- getCursorLinkage curr
    declCls    <- HighLevel.classifyDeclaration curr
    typ        <-
      let ctx = ParseTypeExceptionContext
                  info
                  C.NameKindOrdinary
                  NotRequiredForScoping
      in  fromCXType' ctx =<< clang_getCursorType curr
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

            pure $ (fails ++) $
              if not (null anonDecls) then [
                  parseFail info.id info.loc $
                    ParseUnexpectedAnonInSignature
                ]
              else
                let nonPublicVisibility = [
                        ParseNonPublicVisibility
                      | visibilityCanCauseErrors visibility linkage isDefn
                      ]
                    potentialDuplicate = [
                        ParsePotentialDuplicateSymbol (visibility == PublicVisibility)
                      | isDefn && linkage == ExternalLinkage
                      ]
                    funDeclResult =
                      parseSucceedWith
                        (nonPublicVisibility ++ potentialDuplicate)
                        (mkDecl purity)
                in map parseSucceed otherDecls ++ [funDeclResult]
  where
    guardTypeFunction ::
         CXCursor
      -> C.Type Parse
      -> ParseDecl (
            Either
              [ParseResult Parse]
              ([(Maybe C.ScopedName, C.Type Parse)], C.Type Parse)
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
                       else Just (C.ScopedName argName)

              return (mbArgName, argCType)
            pure $ Right (args', res)
          C.TypeTypedef{} ->
            pure $ Left [
                parseFail info.id info.loc $
                  ParseFunctionOfTypeTypedef
              ]
          otherType ->
            panicIO $ "expected function type, but got " <> show otherType

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
          Right CXCursor_StructDecl -> fmap (fmap Left) <$> parseDecl curr
          Right CXCursor_UnionDecl  -> fmap (fmap Left) <$> parseDecl curr

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

          -- Panic on anything we don't recognize
          -- We could instead use 'foldContinue' here, but this is safer.
          _otherwise -> do
            loc <- HighLevel.clang_getCursorLocation' curr
            panicIO $ "Unexpected " ++ show kind ++ " at " ++ show loc

-- | Global variable declaration
varDecl :: C.DeclInfo Parse -> Parser
varDecl info = \curr -> do
    visibility <- getCursorVisibility curr
    linkage    <- getCursorLinkage curr
    declCls    <- HighLevel.classifyDeclaration curr
    typ        <-
      let ctx = ParseTypeExceptionContext
                  info
                  C.NameKindOrdinary
                  NotRequiredForScoping
      in  fromCXType' ctx =<< clang_getCursorType curr
    cls        <- classifyVarDecl curr
    let mkDecl :: C.DeclKind Parse -> C.Decl Parse
        mkDecl kind = C.Decl{
            info = info
          , kind = kind
          , ann  = NoAnn
          }

    -- TODO: https://github.com/well-typed/hs-bindgen/issues/831
    -- Call 'getReparseInfo' to support macro types in globals.

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

        pure $ (fails ++) $
          if not (null anonDecls) then [
              parseFail info.id info.loc $
                ParseUnexpectedAnonInExtern
            ]
          else (map parseSucceed otherDecls ++) $
            let nonPublicVisibility = [
                    ParseNonPublicVisibility
                  | visibilityCanCauseErrors visibility linkage isDefn
                  ]
                potentialDuplicate = [
                    ParsePotentialDuplicateSymbol (visibility == PublicVisibility)
                  | isDefn && linkage == ExternalLinkage
                  ]
                msgs = nonPublicVisibility ++ potentialDuplicate

            in  case cls of
              VarGlobal ->
                singleton $ parseSucceedWith msgs (mkDecl $ C.DeclGlobal typ)
              VarConst ->
                singleton $ parseSucceedWith msgs (mkDecl $ C.DeclGlobal typ)
              VarThreadLocal -> [
                  parseFail info.id info.loc $
                    ParseUnsupportedTLS
                ]
              VarUnsupported storage -> [
                  parseFail info.id info.loc $
                    ParseUnknownStorageClass storage
                ]
  where
    -- Look for nested declarations inside the global variable type
    nestedDecl :: Fold ParseDecl [ParseResult Parse]
    nestedDecl = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          -- Reference to previously declared type can safely be skipped
          Right CXCursor_TypeRef -> foldContinue

          -- Nested /new/ declarations
          Right CXCursor_StructDecl -> parseDecl curr
          Right CXCursor_UnionDecl  -> parseDecl curr

          -- Initializers
          --
          -- It's a bit annoying that we have to explicitly enumerate them, but
          -- a catch-all may result in us ignoring nodes that we shouldn't.
          --
          -- The order here roughly matches the order of 'CXCursor'.
          -- <https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013>
          Right CXCursor_IntegerLiteral      -> foldContinue
          Right CXCursor_FloatingLiteral     -> foldContinue
          -- TODO: CXCursor_ImaginaryLiteral
          Right CXCursor_StringLiteral       -> foldContinue
          Right CXCursor_ParenExpr           -> foldContinue
          Right CXCursor_UnaryOperator       -> foldContinue
          Right CXCursor_BinaryOperator      -> foldContinue
          Right CXCursor_ConditionalOperator -> foldContinue
          Right CXCursor_CStyleCastExpr      -> foldContinue
          Right CXCursor_InitListExpr        -> foldContinue
          Right CXCursor_CXXBoolLiteralExpr  -> foldContinue -- Since C23

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
          Right CXCursor_UnexposedExpr -> foldContinue
          Right CXCursor_DeclRefExpr   -> foldContinue

          -- @visibility@ attributes, where the value is obtained using
          -- @clang_getCursorVisibility@.
          Right CXCursor_VisibilityAttr -> foldContinue

          -- We are not interested in assembler labels.
          Right CXCursor_AsmLabelAttr -> foldContinue

          -- Function types
          Right CXCursor_ParmDecl -> foldContinue

          -- Panic on anything we don't recognize
          _otherwise -> do
            loc <- HighLevel.clang_getCursorLocation' curr
            panicIO $ "Unexpected " ++ show kind ++ " at " ++ show loc

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

data VarClassification =
    -- | The simplest case: a simple global variable
    --
    -- > extern int simpleGlobal;
    VarGlobal

    -- | Global constants
    --
    -- > extern const int globalConstant;
    -- > static const int staticConst = 123;
    --
    -- NOTE: `static` can be useful to be able to specify the /value/ of the
    -- constant in the header file (perhaps so that the compiler can inline it).
    -- However, `static` does not make sense without `const`: this would be a
    -- mutable variable, but it would be local to any C file that included the
    -- header; it would be invisible to the C API.
    --
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/829>
    -- We could in principle expose the /value/ of the constant, if we know it.
  | VarConst

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
          (Right CX_SC_Extern , False) -> return VarGlobal
          (Right CX_SC_None   , False) -> return VarGlobal
          (Right CX_SC_Extern , True ) -> return VarConst
          (Right CX_SC_Static , True ) -> return VarConst
          (Right CX_SC_None   , True)  -> return VarConst
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
getCursorLinkage :: MonadIO m => CXCursor -> m Linkage
getCursorLinkage = \curr -> do
    linkage   <- clang_getCursorLinkage curr
    case fromSimpleEnum linkage of
      Right linkage' -> case linkage' of
        CXLinkage_Invalid -> do
          loc <- HighLevel.clang_getCursorLocation' curr
          panicIO $ "Invalid linkage " ++ show linkage' ++ " at " ++ show loc
        CXLinkage_NoLinkage -> pure NoLinkage
        CXLinkage_Internal -> pure InternalLinkage
        -- This is C++ specific
        CXLinkage_UniqueExternal -> do
          loc <- HighLevel.clang_getCursorLocation' curr
          panicIO $ "Unsupported linkage " ++ show linkage' ++ " at " ++ show loc
        CXLinkage_External -> pure ExternalLinkage
      -- Panic on anything we don't recognize
      Left x -> do
        loc <- HighLevel.clang_getCursorLocation' curr
        panicIO $ "Unexpected linkage " ++ show x ++ " at " ++ show loc

-- | The visibility of a linker symbol determines whether or not a linker symbol
-- is visible to the linker outside of the shared object that it is defined in.
--
-- See the section on Visibility in the manual for more details.
data Visibility =
    PublicVisibility
  | NonPublicVisibility
  deriving stock (Show, Eq, Generic)

-- | Retrieve the visibilty of the entity that the cursor is currently pointing
-- to.
getCursorVisibility :: MonadIO m => CXCursor -> m Visibility
getCursorVisibility = \curr -> do
    vis  <- fromSimpleEnum <$> clang_getCursorVisibility curr
    case vis of
      -- See https://clang.llvm.org/doxygen/group__CINDEX__CURSOR__MANIP.html#gaf92fafb489ab66529aceab51818994cb
      Right vis' -> case vis' of
        -- Despite the name, /default/ always means public.
        CXVisibility_Default -> pure PublicVisibility
        CXVisibility_Hidden -> pure NonPublicVisibility
        -- This visibility is rarely useful in practice. For binding generation,
        -- we treat it as non-public visibility.
        CXVisibility_Protected -> pure NonPublicVisibility
        CXVisibility_Invalid -> do
          loc <- HighLevel.clang_getCursorLocation' curr
          panicIO $ "Invalid visibility " ++ show vis' ++ " at " ++ show loc
      -- Panic on anything we don't recognize
      Left x -> do
        loc <- HighLevel.clang_getCursorLocation' curr
        panicIO $ "Unexpected visibility " ++ show x ++ " at " ++ show loc

-- | Check if a function declaration or global variable declaration has a
-- problematic case of non-public visiblity.
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

{-------------------------------------------------------------------------------
  Parse types
-------------------------------------------------------------------------------}

data ParseTypeExceptionContext = ParseTypeExceptionContext {
      contextInfo               :: C.DeclInfo Parse
    , contextNameKind           :: C.NameKind
    , contextRequiredForScoping :: RequiredForScoping
    }
  deriving stock (Show)

fromCXType' ::
     MonadIO m
  => ParseTypeExceptionContext -> CXType -> m (C.Type Parse)
fromCXType' = fromCXType @ParseTypeExceptionContext
