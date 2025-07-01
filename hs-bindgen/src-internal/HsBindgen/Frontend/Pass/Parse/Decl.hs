-- | Fold declarations
module HsBindgen.Frontend.Pass.Parse.Decl (foldDecl) where

import Data.Either (partitionEithers)
import Data.List qualified as List

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.C.Predicate qualified as Predicate
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Decl.Monad
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException)
import HsBindgen.Imports
import HsBindgen.Language.C
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecl :: HasCallStack => Fold ParseDecl [C.Decl Parse]
foldDecl = foldWithHandler handleTypeException $ \curr ->
    evalPredicate curr >>= \case
      Right () ->
        dispatchFold curr $ \case
          CXCursor_AlignedAttr        -> attribute
          CXCursor_EnumDecl           -> enumDecl
          CXCursor_FunctionDecl       -> functionDecl
          CXCursor_InclusionDirective -> inclusionDirective
          CXCursor_MacroDefinition    -> macroDefinition
          CXCursor_MacroExpansion     -> macroExpansion
          CXCursor_PackedAttr         -> attribute
          CXCursor_StructDecl         -> structDecl
          CXCursor_TypedefDecl        -> typedefDecl
          CXCursor_UnexposedAttr      -> attribute
          CXCursor_UnexposedDecl      -> unexposedDecl
          CXCursor_UnionDecl          -> unionDecl
          CXCursor_VarDecl            -> varDecl
          kind                        -> unknownCursorKind kind
      Left skipReason ->
        -- We need to keep track of skipped declarations so that they can be
        -- given external bindings.
        case skipReason of
          Predicate.SkipPredicate{} -> do
            recordNonSelectedDecl curr
            foldContinue
          Predicate.SkipBuiltin{} ->
            foldContinue
          Predicate.SkipUnexposed{} ->
            foldContinue

handleTypeException ::
     CXCursor
  -> ParseTypeException
  -> ParseDecl (Maybe [C.Decl Parse])
handleTypeException curr err = do
    info <- getDeclInfo curr
    recordTrace $ UnsupportedType{
        unsupportedTypeContext   = info
      , unsupportedTypeException = err
      }
    return Nothing

{-------------------------------------------------------------------------------
  Info that we collect for all declarations
-------------------------------------------------------------------------------}

getDeclInfo :: CXCursor -> ParseDecl (C.DeclInfo Parse)
getDeclInfo = \curr -> do
    declId     <- getDeclId curr
    declLoc    <- HighLevel.clang_getCursorLocation' curr
    declHeader <- evalGetMainHeader $ singleLocPath declLoc
    let declOrigin = case declId of
          DeclNamed{}     -> C.NameOriginInSource
          DeclAnon anonId -> C.NameOriginGenerated anonId
    return C.DeclInfo{
        declId
      , declLoc
      , declOrigin
      , declAliases = []
      , declHeader
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

-- | Inclusion directive
--
-- We have already processed these (see 'processIncludes'), so here we just
-- skip over them.
inclusionDirective :: Fold ParseDecl [C.Decl Parse]
inclusionDirective = simpleFold $ \_ -> foldContinue

-- | Macros
--
-- In this phase, we return macro declaraitons simply as a list of tokens. We
-- will parse them later (after sorting all declarations in the file).
--
-- NOTE: We rely on selection to filter out clang internal macro declarations.
macroDefinition :: Fold ParseDecl [C.Decl Parse]
macroDefinition = simpleFold $ \curr -> do
    info <- getDeclInfo curr
    unit <- getTranslationUnit
    let mkDecl :: UnparsedMacro -> C.Decl Parse
        mkDecl body = C.Decl{
            declInfo = info
          , declKind = C.DeclMacro body
          , declAnn  = NoAnn
          }
    decl <- mkDecl <$> getUnparsedMacro unit curr
    foldContinueWith [decl]

structDecl :: Fold ParseDecl [C.Decl Parse]
structDecl = simpleFold $ \curr -> do
    info           <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty

        let mkStruct :: [C.StructField Parse] -> C.Decl Parse
            mkStruct fields = C.Decl{
                declInfo = info
              , declKind = C.DeclStruct C.Struct{
                    structSizeof    = fromIntegral sizeof
                  , structAlignment = fromIntegral alignment
                  , structFields    = fields
                  , structAnn       = NoAnn
                  }
              , declAnn  = NoAnn
              }

        -- Separate out nested declarations from regular struct fields
        --
        -- Local declarations inside structs that are not used by any fields
        -- result in implicit fields. Unfortunately, @libclang@ does not make
        -- these visible <https://github.com/llvm/llvm-project/issues/122257>.
        -- This matters, because we need the offsets of these implicit fields.
        -- For now we therefore only try to detect the situation and report an
        -- error when it happens. Hopefully this is anyway very rare.
        let partitionChildren :: [
                 Either [C.Decl Parse] (C.StructField Parse)]
              -> ParseDecl (Maybe ([C.Decl Parse], [C.StructField Parse]))
            partitionChildren xs
              | null unused = return $ Just (used, fields)
              | otherwise   = do
                  recordTrace $ UnsupportedImplicitFields info
                  return Nothing
              where
                otherDecls :: [C.Decl Parse]
                fields     :: [C.StructField Parse]
                (otherDecls, fields) = first concat $ partitionEithers xs

                used, unused :: [C.Decl Parse]
                (used, unused) = detectStructImplicitFields otherDecls fields

        foldRecurseWith (declOrFieldDecl structFieldDecl) $ \xs -> do
          mPartitioned <- partitionChildren xs
          case mPartitioned of
            Just (decls, fields) ->
              return $ decls ++ [mkStruct fields]
            Nothing ->
              -- If the struct has implicit fields, don't generate anything.
              return []
      DeclarationOpaque -> do
        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclStructOpaque
              , declAnn  = NoAnn
              }
        foldContinueWith [decl]
      DeclarationForward _ ->
        foldContinue

unionDecl :: Fold ParseDecl [C.Decl Parse]
unionDecl = simpleFold $ \curr -> do
    info           <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty

        let mkUnion :: [C.UnionField Parse] -> C.Decl Parse
            mkUnion fields = C.Decl{
                  declInfo = info
                , declKind = C.DeclUnion C.Union{
                      unionSizeof    = fromIntegral sizeof
                    , unionAlignment = fromIntegral alignment
                    , unionFields    = fields
                    , unionAnn       = NoAnn
                    }
                , declAnn  = NoAnn
                }

        -- TODO (#682): Support anonymous structures in unions.
        -- See 'partitionChildren' in 'structDecl'.
        let partitionChildren ::
                 [Either [C.Decl Parse] (C.UnionField Parse)]
              -> ParseDecl ([C.Decl Parse], [C.UnionField Parse])
            partitionChildren xs =
                return (otherDecls, fields)
              where
                otherDecls :: [C.Decl Parse]
                fields     :: [C.UnionField Parse]
                (otherDecls, fields) = first concat $ partitionEithers xs

        foldRecurseWith (declOrFieldDecl unionFieldDecl) $ \xs -> do
          (decls, fields) <- partitionChildren xs
          return $ decls ++ [mkUnion fields]
      DeclarationOpaque -> do
        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclUnionOpaque
              , declAnn  = NoAnn
              }
        foldContinueWith [decl]
      DeclarationForward _ ->
        foldContinue
  where

declOrFieldDecl ::
     (CXCursor -> ParseDecl (a Parse))
  -> Fold ParseDecl (Either [C.Decl Parse] (a Parse))
declOrFieldDecl fieldDecl = simpleFold $ \curr -> do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        field <- fieldDecl curr
        foldContinueWith $ Right field
      _otherwise -> do
        fmap Left <$> runFold foldDecl curr

structFieldDecl :: CXCursor -> ParseDecl (C.StructField Parse)
structFieldDecl = \curr -> do
    structFieldLoc    <- HighLevel.clang_getCursorLocation' curr
    structFieldName   <- CName <$> clang_getCursorDisplayName curr
    structFieldType   <- fromCXType =<< clang_getCursorType curr
    structFieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField curr
    structFieldAnn    <- getReparseInfo curr
    structFieldWidth  <- structWidth curr
    pure C.StructField{
        structFieldLoc
      , structFieldName
      , structFieldType
      , structFieldOffset
      , structFieldWidth
      , structFieldAnn
      }

structWidth :: CXCursor -> ParseDecl (Maybe Int)
structWidth = \curr -> do
    isBitField <- clang_Cursor_isBitField curr
    if isBitField
      then Just . fromIntegral <$> clang_getFieldDeclBitWidth curr
      else return Nothing

unionFieldDecl :: CXCursor -> ParseDecl (C.UnionField Parse)
unionFieldDecl = \curr -> do
    unionFieldLoc  <- HighLevel.clang_getCursorLocation' curr
    unionFieldName <- CName <$> clang_getCursorDisplayName curr
    unionFieldType <- fromCXType =<< clang_getCursorType curr
    unionFieldAnn  <- getReparseInfo curr
    pure C.UnionField{
        unionFieldLoc
      , unionFieldName
      , unionFieldType
      , unionFieldAnn
      }

typedefDecl :: Fold ParseDecl [C.Decl Parse]
typedefDecl = simpleFold $ \curr -> do
    info        <- getDeclInfo curr
    typedefType <- fromCXType =<< clang_getTypedefDeclUnderlyingType curr
    typedefAnn  <- getReparseInfo curr
    let decl :: C.Decl Parse
        decl = C.Decl{
            declInfo = info
          , declKind = C.DeclTypedef C.Typedef{
                typedefType
              , typedefAnn
              }
          , declAnn  = NoAnn
          }
    foldContinueWith [decl]

macroExpansion :: Fold ParseDecl [C.Decl Parse]
macroExpansion = simpleFold $ \curr -> do
    loc <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    recordMacroExpansionAt loc
    foldContinue

enumDecl :: Fold ParseDecl [C.Decl Parse]
enumDecl = simpleFold $ \curr -> do
    info <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        ety       <- fromCXType =<< clang_getEnumDeclIntegerType curr

        let mkEnum :: [C.EnumConstant Parse] -> C.Decl Parse
            mkEnum constants = C.Decl{
                declInfo = info
              , declKind = C.DeclEnum C.Enum{
                    enumType      = ety
                  , enumSizeof    = fromIntegral sizeof
                  , enumAlignment = fromIntegral alignment
                  , enumConstants = constants
                  , enumAnn       = NoAnn
                  }
              , declAnn  = NoAnn
              }

        foldRecursePure parseConstant ((:[]) . mkEnum)
      DeclarationOpaque -> do
        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclEnumOpaque
              , declAnn  = NoAnn
              }
        foldContinueWith [decl]
      DeclarationForward _ ->
        foldContinue
  where
    parseConstant :: Fold ParseDecl (C.EnumConstant Parse)
    parseConstant = simpleFold $ \curr ->
        dispatchFold curr $ \case
          CXCursor_EnumConstantDecl -> enumConstantDecl
          CXCursor_PackedAttr       -> attribute
          kind                      -> unknownCursorKind kind

enumConstantDecl :: Fold ParseDecl (C.EnumConstant Parse)
enumConstantDecl = simpleFold $ \curr -> do
    enumConstantLoc   <- HighLevel.clang_getCursorLocation' curr
    enumConstantName  <- CName <$> clang_getCursorDisplayName curr
    enumConstantValue <- toInteger <$> clang_getEnumConstantDeclValue curr
    foldContinueWith C.EnumConstant {
        enumConstantLoc
      , enumConstantName
      , enumConstantValue
      }

functionDecl :: Fold ParseDecl [C.Decl Parse]
functionDecl = simpleFold $ \curr -> do
    info <- getDeclInfo curr
    typ  <- fromCXType =<< clang_getCursorType curr
    (functionArgs, functionRes) <- guardTypeFunction typ
    functionAnn <- getReparseInfo curr
    let decl :: C.Decl Parse
        decl = C.Decl{
            declInfo = info
          , declKind = C.DeclFunction C.Function{
                functionArgs
              , functionRes
              , functionAnn
              }
          , declAnn  = NoAnn
          }
    foldRecurseWith parmDecl $ \nestedDecls -> do
      case concat nestedDecls of
        [] -> return [decl]
        ds -> do
          recordTrace UnsupportedDeclsInSignature{
              unsupportedDeclsInSignatureFun   = info
            , unsupportedDeclsInSignatureDecls = ds
            }
          return []
  where
    guardTypeFunction ::
         C.Type Parse
      -> ParseDecl ([C.Type Parse], C.Type Parse)
    guardTypeFunction ty =
        case ty of
          C.TypeFun args res ->
            pure (args, res)
          otherType ->
            panicIO $ "Expected function type, but got " <> show otherType

    -- Look for (unsupported) declarations inside function parameters
    parmDecl :: Fold ParseDecl [DeclId]
    parmDecl = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          -- 'ParmDecl' sometimes appear in nested in the AST
          Right CXCursor_ParmDecl ->
            foldRecurseWith parmDecl (return . concat)

          -- Found a nested declaration
          Right CXCursor_StructDecl -> report curr
          Right CXCursor_UnionDecl  -> report curr

          -- Harmless
          Right CXCursor_TypeRef        -> foldContinue
          Right CXCursor_IntegerLiteral -> foldContinue
          Right CXCursor_UnexposedAttr  -> foldContinue

          -- Panic on anything we don't recognize
          -- We could instead use 'foldContinue' here, but this is safer.
          _otherwise -> do
            loc <- HighLevel.clang_getCursorLocation' curr
            panicIO $ "Unexpected " ++ show kind ++ " at " ++ show loc
      where
        report :: CXCursor -> ParseDecl (Next ParseDecl [DeclId])
        report curr = do
          decl <- getDeclId curr
          foldContinueWith [decl]

-- | Global variable declaration
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/42>
varDecl :: Fold ParseDecl [C.Decl Parse]
varDecl = simpleFold $ \_ -> foldContinue

-- | Unexposed declarations
--
-- Since we not told what kind of declaration this is, we can't do much except
-- issue a warning.
unexposedDecl :: Fold ParseDecl [C.Decl Parse]
unexposedDecl = simpleFold $ \curr -> do
    skippedLoc <- HighLevel.clang_getCursorLocation' curr
    recordTrace $ Skipped $ Predicate.SkipUnexposed{skippedLoc}
    foldContinue

-- | Attributes (alignment, packed, ..)
--
-- These attributes are recorded as children of the record declaration, so we
-- can just skip over them.
attribute :: Fold ParseDecl a
attribute = simpleFold $ \_ -> foldContinue

{-------------------------------------------------------------------------------
  Auxiliary: detect implicit fields
-------------------------------------------------------------------------------}

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
    allFields :: [C.StructField Parse]
    allFields = outerFields ++ concatMap nestedFields nestedDecls

    nestedFields :: C.Decl Parse -> [C.StructField Parse]
    nestedFields C.Decl{declKind} =
        case declKind of
          C.DeclStruct struct -> C.structFields struct
          _otherwise          -> []

    fieldDeps :: [QualDeclId]
    fieldDeps = map snd $ concatMap (depsOfType . C.structFieldType) allFields

    declIsUsed :: C.Decl Parse -> Bool
    declIsUsed decl = declQualDeclId decl `elem` fieldDeps
