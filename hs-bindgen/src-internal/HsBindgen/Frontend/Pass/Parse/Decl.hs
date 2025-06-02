-- | Fold declarations
module HsBindgen.Frontend.Pass.Parse.Decl (foldDecl) where

import Data.Bifunctor
import Data.Either (partitionEithers)
import Data.List qualified as List
import GHC.Stack

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.C.Predicate (SkipReason (..), match)
import HsBindgen.Errors
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad
import HsBindgen.Frontend.Pass.Parse.Type
import HsBindgen.Frontend.Pass.Parse.Util
import HsBindgen.Language.C

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}


foldDecl :: HasCallStack => Fold M [C.Decl Parse]
foldDecl curr = do
    loc <- HighLevel.clang_getCursorLocation' curr

    predicate       <- getPredicate
    mainSourcePaths <- getMainSourcePaths
    matchResult     <- match mainSourcePaths curr loc predicate

    case matchResult of

      Left skipReason -> do
        name <- clang_getCursorSpelling curr
        case skipReason of
          SkipReasonBuiltIn ->
            recordTraceWithCallStack callStack $
              SkippedBuiltIn name
          SkipReasonPredicate {..} ->
            recordTraceWithCallStack callStack $
              SkippedPredicate name loc reason
        pure $ Continue Nothing

      Right _  -> dispatchWithArg curr $ \case
        CXCursor_InclusionDirective -> inclusionDirective
        CXCursor_MacroDefinition    -> macroDefinition
        CXCursor_StructDecl         -> structDecl
        CXCursor_UnionDecl          -> unionDecl
        CXCursor_TypedefDecl        -> typedefDecl
        CXCursor_MacroExpansion     -> macroExpansion
        CXCursor_EnumDecl           -> enumDecl
        CXCursor_FunctionDecl       -> functionDecl
        kind -> \_ -> panicIO $ "foldDecl: " ++ show kind

{-------------------------------------------------------------------------------
  Info that we collect for all declarations
-------------------------------------------------------------------------------}

getDeclInfo :: CXCursor -> M (C.DeclInfo Parse)
getDeclInfo curr = do
    declId  <- getDeclId curr
    declLoc <- HighLevel.clang_getCursorLocation' curr
    return C.DeclInfo{declId, declLoc}

getReparseInfo :: CXCursor -> M ReparseInfo
getReparseInfo curr = do
    extent <- fmap multiLocExpansion <$> HighLevel.clang_getCursorExtent curr
    hasMacroExpansion <- checkHasMacroExpansion extent
    if hasMacroExpansion then do
      unit <- getTranslationUnit
      ReparseNeeded <$> HighLevel.clang_tokenize unit extent
    else
      return ReparseNotNeeded

{-------------------------------------------------------------------------------
  Functions for each kind of AST node
-------------------------------------------------------------------------------}

inclusionDirective :: Fold M a
inclusionDirective curr = do
    fr <- getIncludeFrom
    to <- getIncludeTo
    recordTraceWithCallStack callStack $ RegisterInclude fr to
    modifyIncludeGraph $ IncludeGraph.register fr to
    -- TODO: We should update the main header.
    return $ Continue Nothing
  where
    getIncludeFrom :: M SourcePath
    getIncludeFrom = singleLocPath <$> HighLevel.clang_getCursorLocation' curr

    getIncludeTo :: M SourcePath
    getIncludeTo = do
        file <- clang_getIncludedFile curr
        SourcePath <$> clang_getFileName file

-- | Macros
--
-- In this phase, we return macro declaraitons simply as a list of tokens. We
-- will parse them later (after sorting all declarations in the file).
--
-- NOTE: We rely on selection to filter out clang internal macro declarations.
macroDefinition :: Fold M [C.Decl Parse]
macroDefinition curr = do
    info <- getDeclInfo curr
    unit <- getTranslationUnit
    let mkDecl :: UnparsedMacro -> C.Decl Parse
        mkDecl body = C.Decl{
            declInfo = info
          , declKind = C.DeclMacro body
          , declAnn  = NoAnn
          }
    Continue . Just . (:[]) . mkDecl <$> getUnparsedMacro unit curr

structDecl :: Fold M [C.Decl Parse]
structDecl curr = do
    info           <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular ->
        return $ Recurse (structOrUnionFieldDecl assembleStructField) (aux info)
      DeclarationOpaque -> do
        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclStructOpaque
              , declAnn  = NoAnn
              }
        return $ Continue $ Just [decl]
      DeclarationForward _ ->
        return $ Continue $ Nothing
  where
    aux ::
         C.DeclInfo Parse
      -> [Either [C.Decl Parse] (C.StructField Parse)]
      -> M (Maybe [C.Decl Parse])
    aux info xs = do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty

        -- Local declarations inside structs that are not used by any fields
        -- result in implicit fields. Unfortunately, @libclang@ does not make
        -- these visible <https://github.com/llvm/llvm-project/issues/122257>.
        -- This matters, because we need the offsets of these implicit fields.
        -- For now we therefore only try to detect the situation and report an
        -- error when it happens. Hopefully this is anyway very rare.
        --
        -- TODO: This check is wrong. When we get more nesting, we get /all/
        -- structs at once, even those used by /nested/ fields, thus triggering
        -- this error.
--        unless (null unusedDecls) $ do
--          mloc <- HighLevel.clang_getCursorLocation curr
--          recordTraceWithCallStack callStack $ UnsupportedImplicitFields (declId info)
        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclStruct C.Struct{
                    structSizeof    = fromIntegral sizeof
                  , structAlignment = fromIntegral alignment
                  , structFields    = fields
                  , structAnn       = NoAnn
                  }
              , declAnn  = NoAnn
              }
        return $ Just $ otherDecls ++ [decl]
      where
        otherDecls :: [C.Decl Parse]
        fields     :: [C.StructField Parse]
        (otherDecls, fields) = first concat $ partitionEithers xs

        fieldDeps :: [C.QualId Parse]
        fieldDeps = map snd $ concatMap (depsOfType . C.structFieldType) fields

        declIsUsed :: C.Decl Parse -> Bool
        declIsUsed decl = C.declQualId decl `elem` fieldDeps

        _usedDecls, _unusedDecls :: [C.Decl Parse]
        (_usedDecls, _unusedDecls) = List.partition declIsUsed otherDecls

unionDecl :: Fold M [C.Decl Parse]
unionDecl curr = do
    info           <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular ->
        return $ Recurse (structOrUnionFieldDecl assembleUnionField) (aux info)
      DeclarationOpaque -> do
        panicIO "Opaque union not yet supported"
      DeclarationForward _ ->
        return $ Continue $ Nothing
  where
    aux ::
         C.DeclInfo Parse
      -> [Either [C.Decl Parse] (C.UnionField Parse)]
      -> M (Maybe [C.Decl Parse])
    aux info xs = do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty

        -- TODO (#682): Support anonymous structures in unions.
        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclUnion C.Union{
                    unionSizeof    = fromIntegral sizeof
                  , unionAlignment = fromIntegral alignment
                  , unionFields    = fields
                  , unionAnn       = NoAnn
                  }
              , declAnn  = NoAnn
              }
        return $ Just $ otherDecls ++ [decl]
      where
        otherDecls :: [C.Decl Parse]
        fields     :: [C.UnionField Parse]
        (otherDecls, fields) = first concat $ partitionEithers xs

structOrUnionFieldDecl ::
     (CXCursor -> M (a Parse))
  -> Fold M (Either [C.Decl Parse] (a Parse))
structOrUnionFieldDecl assembleField curr = do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        field <- assembleField curr
        return $ Continue . Just . Right $ field
      _otherwise -> do
        fmap Left <$> foldDecl curr

assembleStructField :: CXCursor -> M (C.StructField Parse)
assembleStructField curr = do
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

structWidth :: CXCursor -> M (Maybe Int)
structWidth curr = do
    isBitField <- clang_Cursor_isBitField curr
    if isBitField
      then Just . fromIntegral <$> clang_getFieldDeclBitWidth curr
      else return Nothing

assembleUnionField :: CXCursor -> M (C.UnionField Parse)
assembleUnionField curr = do
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

typedefDecl :: Fold M [C.Decl Parse]
typedefDecl curr = do
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
    return $ Continue $ Just [decl]

macroExpansion :: Fold M [C.Decl Parse]
macroExpansion curr = do
    loc <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    recordMacroExpansionAt loc
    return $ Continue Nothing

enumDecl :: Fold M [C.Decl Parse]
enumDecl curr = do
    info <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular ->
        pure $ Recurse enumeratorDecl (aux info)
      DeclarationOpaque -> do
        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclEnumOpaque
              , declAnn  = NoAnn
              }
        pure $ Continue $ Just [decl]
      DeclarationForward _ ->
        pure $ Continue $ Nothing
    where
      aux ::
           C.DeclInfo Parse
        -> [C.EnumConstant Parse]
        -> M (Maybe [C.Decl Parse])
      aux info es = do
        ty        <- clang_getCursorType curr
        sizeof    <- clang_Type_getSizeOf  ty
        alignment <- clang_Type_getAlignOf ty
        ety       <- fromCXType =<< clang_getEnumDeclIntegerType curr

        let decl :: C.Decl Parse
            decl = C.Decl{
                declInfo = info
              , declKind = C.DeclEnum C.Enum{
                    enumType      = ety
                  , enumSizeof    = fromIntegral sizeof
                  , enumAlignment = fromIntegral alignment
                  , enumConstants = es
                  , enumAnn       = NoAnn
                  }
              , declAnn  = NoAnn
              }
        return $ Just [decl]

enumeratorDecl :: Fold M (C.EnumConstant Parse)
enumeratorDecl curr = do
    dispatch curr $ \case
      CXCursor_EnumConstantDecl -> do
        enumConstantLoc   <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
        enumConstantName  <- CName <$> clang_getCursorDisplayName curr
        enumConstantValue <- toInteger <$> clang_getEnumConstantDeclValue curr
        pure $ Continue $ Just C.EnumConstant {
            enumConstantLoc
          , enumConstantName
          , enumConstantValue
          }
      CXCursor_PackedAttr -> do
        -- 'packed' is handlded by clang; we can ignore it.
        pure $ Continue Nothing
      kind ->
        panicIO $ "Unrecognized cursor in enumerator declaration " <> show kind

functionDecl :: Fold M [C.Decl Parse]
functionDecl curr = do
    info <- getDeclInfo curr
    typ  <- fromCXType =<< clang_getCursorType curr
    (functionArgs, functionRes) <- guardTypeFunction typ
    functionAnn    <- getReparseInfo curr
    functionHeader <- getMainHeader
    let decl :: C.Decl Parse
        decl = C.Decl{
            declInfo = info
          , declKind = C.DeclFunction C.Function{
                functionArgs
              , functionRes
              , functionAnn
              , functionHeader
              }
          , declAnn  = NoAnn
          }
    -- TODO (#684): Handle inline type declarations.
    pure $ Continue $ Just [decl]
  where
    guardTypeFunction :: C.Type Parse -> M ([C.Type Parse], C.Type Parse)
    guardTypeFunction ty =
        case ty of
          C.TypeFun args res ->
            pure (args, res)
          otherType ->
            panicIO $ "Expected function type, but got " <> show otherType
