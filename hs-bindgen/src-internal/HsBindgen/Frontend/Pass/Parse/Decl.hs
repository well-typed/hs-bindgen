-- | Fold declarations
module HsBindgen.Frontend.Pass.Parse.Decl (foldDecl) where

import Data.Either (partitionEithers)
import Data.List qualified as List

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.HighLevel.Documentation
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.Decl.Monad
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Type
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException)
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecl :: Fold ParseDecl [C.Decl Parse]
foldDecl = foldWithHandler handleTypeException $ \curr -> do
    info <- getDeclInfo curr

    let parseWith ::
             (C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse])
          -> C.NameKind
          -> ParseDecl (Next ParseDecl [C.Decl Parse])
        parseWith parser kind = do
            selected <- evalPredicate info kind
            if selected
              then runFold (parser info) curr
              else recordNonSelectedDecl info kind >> foldContinue

    dispatch curr $ \case
      -- Kinds that we parse
      CXCursor_FunctionDecl    -> parseWith functionDecl    C.NameKindOrdinary
      CXCursor_VarDecl         -> parseWith varDecl         C.NameKindOrdinary
      CXCursor_TypedefDecl     -> parseWith typedefDecl     C.NameKindOrdinary
      CXCursor_MacroDefinition -> parseWith macroDefinition C.NameKindOrdinary
      CXCursor_StructDecl      -> parseWith structDecl      C.NameKindStruct
      CXCursor_UnionDecl       -> parseWith unionDecl       C.NameKindUnion
      CXCursor_EnumDecl        -> parseWith enumDecl        C.NameKindEnum

      -- Process macro expansions independent of any selection predicates
      CXCursor_MacroExpansion -> runFold macroExpansion curr

      -- Kinds that we skip over
      CXCursor_AlignedAttr        -> foldContinue
      CXCursor_InclusionDirective -> foldContinue
      CXCursor_PackedAttr         -> foldContinue
      CXCursor_UnexposedAttr      -> foldContinue
      CXCursor_UnexposedDecl      -> foldContinue

      -- Report error for declarations we don't recognize
      kind -> unknownCursorKind curr kind

handleTypeException ::
     CXCursor
  -> ParseTypeException
  -> ParseDecl (Maybe [C.Decl Parse])
handleTypeException curr err = do
    info <- getDeclInfo curr
    recordTrace $ ParseUnsupportedType info err
    return Nothing

{-------------------------------------------------------------------------------
  Info that we collect for all declarations
-------------------------------------------------------------------------------}

getDeclInfo :: CXCursor -> ParseDecl (C.DeclInfo Parse)
getDeclInfo = \curr -> do
    declId     <- getPrelimDeclId curr
    declLoc    <- HighLevel.clang_getCursorLocation' curr
    declHeader <- evalGetMainHeader $ singleLocPath declLoc
    declComment <- clang_getComment curr
    -- TODO: We might want a NameOriginBuiltin
    return C.DeclInfo{
        declId
      , declLoc
      , declAliases = []
      , declHeader
      , declComment
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

-- | Macros
--
-- In this phase, we return macro declarations simply as a list of tokens. We
-- will parse them later (after sorting all declarations in the file).
--
-- NOTE: We rely on selection to filter out clang internal macro declarations.
macroDefinition ::
     HasCallStack
  => C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse]
macroDefinition info = simpleFold $ \curr -> do
    unit <- getTranslationUnit
    let mkDecl :: UnparsedMacro -> C.Decl Parse
        mkDecl body = C.Decl{
            declInfo = info
          , declKind = C.DeclMacro body
          , declAnn  = NoAnn
          }
    decl <- mkDecl <$> getUnparsedMacro unit curr
    foldContinueWith [decl]

structDecl :: C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse]
structDecl info = simpleFold $ \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        comment   <- clang_getComment curr

        let mkStruct :: [C.StructField Parse] -> C.Decl Parse
            mkStruct fields = C.Decl {
                declInfo = info
              , declKind = C.DeclStruct C.Struct{
                    structSizeof    = fromIntegral sizeof
                  , structAlignment = fromIntegral alignment
                  , structFields    = fields
                  , structAnn       = NoAnn
                  , structComment   = comment
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
                  recordTrace $ ParseUnsupportedImplicitFields info
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

unionDecl :: C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse]
unionDecl info = simpleFold $ \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        comment   <- clang_getComment curr

        let mkUnion :: [C.UnionField Parse] -> C.Decl Parse
            mkUnion fields = C.Decl{
                  declInfo = info
                , declKind = C.DeclUnion C.Union{
                      unionSizeof    = fromIntegral sizeof
                    , unionAlignment = fromIntegral alignment
                    , unionFields    = fields
                    , unionAnn       = NoAnn
                    , unionComment   = comment
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
    structFieldName   <- C.Name <$> clang_getCursorDisplayName curr
    structFieldType   <- fromCXType =<< clang_getCursorType curr
    structFieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField curr
    structFieldAnn    <- getReparseInfo curr
    structFieldWidth  <- structWidth curr
    structFieldComment <- clang_getComment curr
    pure C.StructField{
        structFieldLoc
      , structFieldName
      , structFieldType
      , structFieldOffset
      , structFieldWidth
      , structFieldAnn
      , structFieldComment
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
    unionFieldName <- C.Name <$> clang_getCursorDisplayName curr
    unionFieldType <- fromCXType =<< clang_getCursorType curr
    unionFieldAnn  <- getReparseInfo curr
    unionFieldComment   <- clang_getComment curr
    pure C.UnionField{
        unionFieldLoc
      , unionFieldName
      , unionFieldType
      , unionFieldAnn
      , unionFieldComment
      }

typedefDecl :: C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse]
typedefDecl info = simpleFold $ \curr -> do
    typedefType <- fromCXType =<< clang_getTypedefDeclUnderlyingType curr
    typedefAnn  <- getReparseInfo curr
    typedefComment <- clang_getComment curr
    let decl :: C.Decl Parse
        decl = C.Decl{
            declInfo = info
          , declKind = C.DeclTypedef C.Typedef{
                typedefType
              , typedefAnn
              , typedefComment
              }
          , declAnn  = NoAnn
          }
    foldContinueWith [decl]

macroExpansion :: Fold ParseDecl [C.Decl Parse]
macroExpansion = simpleFold $ \curr -> do
    loc <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    recordMacroExpansionAt loc
    foldContinue

enumDecl :: C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse]
enumDecl info = simpleFold $ \curr -> do
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular -> do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        ety       <- fromCXType =<< clang_getEnumDeclIntegerType curr
        comment   <- clang_getComment curr

        let mkEnum :: [C.EnumConstant Parse] -> C.Decl Parse
            mkEnum constants = C.Decl{
                declInfo = info
              , declKind = C.DeclEnum C.Enum{
                    enumType      = ety
                  , enumSizeof    = fromIntegral sizeof
                  , enumAlignment = fromIntegral alignment
                  , enumConstants = constants
                  , enumAnn       = NoAnn
                  , enumComment   = comment
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
        dispatch curr $ \case
          CXCursor_EnumConstantDecl -> enumConstantDecl curr
          CXCursor_PackedAttr       -> foldContinue
          kind                      -> unknownCursorKind curr kind

enumConstantDecl :: CXCursor -> ParseDecl (Next ParseDecl (C.EnumConstant Parse))
enumConstantDecl curr = do
    enumConstantLoc   <- HighLevel.clang_getCursorLocation' curr
    enumConstantName  <- C.Name <$> clang_getCursorDisplayName curr
    enumConstantValue <- toInteger <$> clang_getEnumConstantDeclValue curr
    enumConstantComment <- clang_getComment curr
    foldContinueWith C.EnumConstant {
        enumConstantLoc
      , enumConstantName
      , enumConstantValue
      , enumConstantComment
      }

functionDecl :: C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse]
functionDecl info = simpleFold $ \curr -> do
    typ  <- fromCXType =<< clang_getCursorType curr
    (functionArgs, functionRes) <- guardTypeFunction typ
    functionAnn <- getReparseInfo curr
    functionComment <- clang_getComment curr
    let mkDecl :: C.FunctionPurity -> C.Decl Parse
        mkDecl purity = C.Decl{
            declInfo = info
          , declKind = C.DeclFunction C.Function {
                functionArgs
              , functionRes
              , functionAttrs = C.FunctionAttributes purity
              , functionAnn
              , functionComment
              }
          , declAnn  = NoAnn
          }
    foldRecurseWith nestedDecl $ \nestedDecls -> do
      let declsAndAttrs = concat nestedDecls
          (decls, attrs) = partitionEithers declsAndAttrs
          purity = C.decideFunctionPurity attrs
          (anonDecls, otherDecls) = partitionAnonDecls decls
      if not (null anonDecls) then do
        recordTrace $ ParseUnexpectedAnonInSignature info
        return []
      else do
        return $ otherDecls ++ [mkDecl purity]
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

    -- Look for (unsupported) declarations inside function parameters, and for
    -- function attributes. Function attributes are returned separately, so that
    -- we can pair them with the parent function.
    nestedDecl :: Fold ParseDecl [Either (C.Decl Parse) C.FunctionPurity]
    nestedDecl = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          -- 'ParmDecl' sometimes appear in nested in the AST
          Right CXCursor_ParmDecl ->
            foldRecurseWith nestedDecl (return . concat)

          -- Nested declarations
          Right CXCursor_StructDecl -> fmap (fmap Left) <$> runFold foldDecl curr
          Right CXCursor_UnionDecl  -> fmap (fmap Left) <$> runFold foldDecl curr

          -- Harmless
          Right CXCursor_TypeRef        -> foldContinue
          Right CXCursor_IntegerLiteral -> foldContinue
          Right CXCursor_UnexposedAttr  -> foldContinue

          -- @const@ and @pure@ function attributes.
          Right CXCursor_ConstAttr -> foldContinueWith $ [Right C.HaskellPureFunction]
          Right CXCursor_PureAttr  -> foldContinueWith $ [Right C.CPureFunction]

          -- TODO: <https://github.com/well-typed/hs-bindgen/issues/876>
          -- Take visibility into account.
          Right CXCursor_VisibilityAttr -> foldContinue

          -- Attributes we (probably?) want to ignore
          Right CXCursor_WarnUnusedResultAttr -> foldContinue

          -- Panic on anything we don't recognize
          -- We could instead use 'foldContinue' here, but this is safer.
          _otherwise -> do
            loc <- HighLevel.clang_getCursorLocation' curr
            panicIO $ "Unexpected " ++ show kind ++ " at " ++ show loc

-- | Global variable declaration
varDecl :: C.DeclInfo Parse -> Fold ParseDecl [C.Decl Parse]
varDecl info = simpleFold $ \curr -> do
    typ  <- fromCXType =<< clang_getCursorType curr
    cls  <- classifyVarDecl curr
    let mkDecl :: C.DeclKind Parse -> C.Decl Parse
        mkDecl kind = C.Decl{
            declInfo = info
          , declKind = kind
          , declAnn  = NoAnn
          }

    -- TODO: https://github.com/well-typed/hs-bindgen/issues/831
    -- Call 'getReparseInfo' to support macro types in globals.

    foldRecurseWith nestedDecl $ \nestedDecls -> do
      let (anonDecls, otherDecls) = partitionAnonDecls (concat nestedDecls)
      if not (null anonDecls) then do
        recordTrace $ ParseUnexpectedAnonInExtern info
        return []
      else (otherDecls ++) <$> do
        case cls of
          VarGlobal isExtern -> do
            unless isExtern $
              recordTrace $ ParsePotentialDuplicateGlobal info
            return [mkDecl $ C.DeclGlobal typ]
          VarConst _isExternOrStatic -> do
            recordTrace $ ParseUnsupportedConst info
            return []
            --unless isExternOrStatic $
            --  recordTrace $ PotentialDuplicateGlobal info
            --return [mkDecl $ C.DeclConst typ]
          VarThreadLocal -> do
            recordTrace $ ParseUnsupportedTLS info
            return []
          VarUnsupported storage -> do
            recordTrace $ ParseUnknownStorageClass info storage
            return []
  where
    -- Look for nested declarations inside the global variable type
    nestedDecl :: Fold ParseDecl [C.Decl Parse]
    nestedDecl = simpleFold $ \curr -> do
        kind <- fromSimpleEnum <$> clang_getCursorKind curr
        case kind of
          -- Reference to previously declared type can safely be skipped
          Right CXCursor_TypeRef -> foldContinue

          -- Nested /new/ declarations
          Right CXCursor_StructDecl -> runFold foldDecl curr
          Right CXCursor_UnionDecl  -> runFold foldDecl curr

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

          -- TODO: <https://github.com/well-typed/hs-bindgen/issues/876>
          -- Take visibility into account.
          Right CXCursor_VisibilityAttr -> foldContinue

          -- Panic on anything we don't recognize
          _otherwise -> do
            loc <- HighLevel.clang_getCursorLocation' curr
            panicIO $ "Unexpected " ++ show kind ++ " at " ++ show loc

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Partition declarations into anonymous and non-anonymous
--
-- We are only interested in the name of the declaration /itself/; if a named
-- declaration /contains/ anonymous declarations, that's perfectly fine.
partitionAnonDecls :: [C.Decl Parse] -> ([C.Decl Parse], [C.Decl Parse])
partitionAnonDecls = List.partition (declIdIsAnon . C.declId . C.declInfo)
  where
    declIdIsAnon :: PrelimDeclId -> Bool
    declIdIsAnon PrelimDeclIdAnon{} = True
    declIdIsAnon _otherwise         = False

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

    fieldDeps :: [NsPrelimDeclId]
    fieldDeps = map snd $ concatMap (depsOfType . C.structFieldType) allFields

    declIsUsed :: C.Decl Parse -> Bool
    declIsUsed decl = C.declNsPrelimDeclId decl `elem` fieldDeps

data VarClassification =
    -- | The simplest case: a simple global variable
    --
    -- > extern int simpleGlobal;
    --
    -- We record if the variable is declared @extern@ or not (if it isn't,
    -- we issue a warning, as this may result in duplicate symbols).
    VarGlobal Bool

    -- | Global constants
    --
    -- > extern const int globalConstant;
    -- > static const int staticConst = 123;
    --
    -- We record if the variable is declared @extern@ or @static@, for the same
    -- reason as in 'VarGlobal'.
    --
    -- NOTE: `static` can be useful to be able to specify the /value/ of the
    -- constant in the header file (perhaps so that the compiler can inline it).
    -- However, `static` does not make sense without `const`: this would be a
    -- mutable variable, but it would be local to any C file that included the
    -- header; it would be invisible to the C API.
    --
    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/829>
    -- We could in principle expose the /value/ of the constant, if we know it.
  | VarConst Bool

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
classifyVarDecl curr = do
    tls <- clang_getCursorTLSKind curr
    case fromSimpleEnum tls of
      Right CXTLS_None -> do
        storage   <- clang_Cursor_getStorageClass curr
        typ       <- clang_getCursorType curr
        canonical <- clang_getCanonicalType typ
        isConst   <- clang_isConstQualifiedType canonical
        case (fromSimpleEnum storage, isConst) of
          (Right CX_SC_Extern , False) -> return $ VarGlobal True
          (Right CX_SC_None   , False) -> return $ VarGlobal False
          (Right CX_SC_Extern , True ) -> return $ VarConst True
          (Right CX_SC_Static , True ) -> return $ VarConst True
          (Right CX_SC_None   , True)  -> return $ VarConst False
          _otherwise -> return $ VarUnsupported storage
      _otherwise ->
        return VarThreadLocal
