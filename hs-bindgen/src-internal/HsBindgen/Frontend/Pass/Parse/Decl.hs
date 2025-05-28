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
import HsBindgen.Frontend.AST
import HsBindgen.Frontend.AST.Deps
import HsBindgen.Frontend.Graph.Includes qualified as IncludeGraph
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Monad
import HsBindgen.Frontend.Pass.Parse.Type
import HsBindgen.Frontend.Pass.Parse.Util
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}


foldDecl :: HasCallStack => Fold M [Decl Parse]
foldDecl curr = do
    mloc <- HighLevel.clang_getCursorLocation curr
    let sloc = multiLocExpansion mloc
    predicate <- getPredicate
    mainSourcePaths <- getMainSourcePaths
    matchResult <- match mainSourcePaths curr sloc predicate
    case matchResult of
      Left skipReason -> do
        name <- clang_getCursorSpelling curr
        case skipReason of
          SkipReasonBuiltIn -> recordTraceWithCallStack callStack (SkippedBuiltIn name)
          SkipReasonPredicate {..} -> recordTraceWithCallStack callStack $ SkippedPredicate name mloc reason
        pure $ Continue Nothing
      Right _  -> dispatchWithArg curr $ \case
        CXCursor_InclusionDirective -> inclusionDirective
        CXCursor_MacroDefinition    -> macroDefinition
        CXCursor_StructDecl         -> structDecl
        CXCursor_UnionDecl          -> unionDecl
        CXCursor_TypedefDecl        -> typedefDecl
        CXCursor_MacroExpansion     -> macroExpansion
        CXCursor_EnumDecl           -> enumDecl
        kind -> \_ -> panicIO $ "foldDecl: " ++ show kind

{-------------------------------------------------------------------------------
  Info that we collect for all declarations
-------------------------------------------------------------------------------}

getDeclInfo :: CXCursor -> M (DeclInfo Parse)
getDeclInfo curr = do
    declId  <- getDeclId curr
    declLoc <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    return DeclInfo{declId, declLoc}

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
    loc  <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    includedFile <- clang_getIncludedFile curr
    includedFilePath <- SourcePath <$> clang_getFileName includedFile
    let includingFilePath = (singleLocPath loc)
    recordTraceWithCallStack callStack $ RegisterInclude includingFilePath includedFilePath
    modifyIncludeGraph $ IncludeGraph.register includingFilePath includedFilePath
    return $ Continue Nothing

-- | Macros
--
-- In this phase, we return macro declaraitons simply as a list of tokens. We
-- will parse them later (after sorting all declarations in the file).
--
-- NOTE: We rely on selection to filter out clang internal macro declarations.
macroDefinition :: Fold M [Decl Parse]
macroDefinition curr = do
    info <- getDeclInfo curr
    unit <- getTranslationUnit
    let mkDecl :: UnparsedMacro -> Decl Parse
        mkDecl body = Decl{
            declInfo = info
          , declKind = DeclMacro body
          , declAnn  = NoAnn
          }
    Continue . Just . (:[]) . mkDecl <$> getUnparsedMacro unit curr

structDecl :: Fold M [Decl Parse]
structDecl curr = do
    info           <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular ->
        return $ Recurse (structOrUnionFieldDecl assembleStructField) (aux info)
      DeclarationOpaque -> do
        let decl :: Decl Parse
            decl = Decl{
                declInfo = info
              , declKind = DeclStructOpaque
              , declAnn  = NoAnn
              }
        return $ Continue $ Just [decl]
      DeclarationForward _ ->
        return $ Continue $ Nothing
  where
    aux ::
         DeclInfo Parse
      -> [Either [Decl Parse] (StructField Parse)]
      -> M (Maybe [Decl Parse])
    aux info xs = do
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
        let decl :: Decl Parse
            decl = Decl{
                declInfo = info
              , declKind = DeclStruct fields
              , declAnn  = NoAnn
              }
        return $ Just $ otherDecls ++ [decl]
      where
        otherDecls :: [Decl Parse]
        fields     :: [StructField Parse]
        (otherDecls, fields) = first concat $ partitionEithers xs

        fieldDeps :: [QualId Parse]
        fieldDeps = map snd $ mapMaybe (depsOfType . structFieldType) fields

        declIsUsed :: Decl Parse -> Bool
        declIsUsed decl = declQualId decl `elem` fieldDeps

        usedDecls, unusedDecls :: [Decl Parse]
        (usedDecls, unusedDecls) = List.partition declIsUsed otherDecls

unionDecl :: Fold M [Decl Parse]
unionDecl curr = do
    info           <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular ->
        return $ Recurse (structOrUnionFieldDecl assembleUnionField) (aux info)
      DeclarationOpaque -> do
        let decl :: Decl Parse
            decl = Decl{
                declInfo = info
              , declKind = DeclUnionOpaque
              , declAnn  = NoAnn
              }
        return $ Continue $ Just [decl]
      DeclarationForward _ ->
        return $ Continue $ Nothing
  where
    aux ::
         DeclInfo Parse
      -> [Either [Decl Parse] (UnionField Parse)]
      -> M (Maybe [Decl Parse])
    aux info xs = do
        -- TODO (#682): Support anonymous structures in unions.
        let decl :: Decl Parse
            decl = Decl{
                declInfo = info
              , declKind = DeclUnion fields
              , declAnn  = NoAnn
              }
        return $ Just $ otherDecls ++ [decl]
      where
        otherDecls :: [Decl Parse]
        fields     :: [UnionField Parse]
        (otherDecls, fields) = first concat $ partitionEithers xs

structOrUnionFieldDecl ::
     (CXCursor -> M (a Parse))
  -> Fold M (Either [Decl Parse] (a Parse))
structOrUnionFieldDecl assembleField curr = do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        field <- assembleField curr
        return $ Continue . Just . Right $ field
      _otherwise -> do
        fmap Left <$> foldDecl curr

assembleStructField :: CXCursor -> M (StructField Parse)
assembleStructField curr = do
    structFieldName   <- clang_getCursorDisplayName curr
    structFieldType   <- fromCXType =<< clang_getCursorType curr
    structFieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField curr
    structFieldAnn    <- getReparseInfo curr
    pure StructField{
        structFieldName
      , structFieldType
      , structFieldOffset
      , structFieldAnn
      }

assembleUnionField :: CXCursor -> M (UnionField Parse)
assembleUnionField curr = do
    unionFieldName   <- clang_getCursorDisplayName curr
    unionFieldType   <- fromCXType =<< clang_getCursorType curr
    unionFieldAnn    <- getReparseInfo curr
    pure UnionField{
        unionFieldName
      , unionFieldType
      , unionFieldAnn
      }

typedefDecl :: Fold M [Decl Parse]
typedefDecl curr = do
    info        <- getDeclInfo curr
    typedefType <- fromCXType =<< clang_getTypedefDeclUnderlyingType curr
    typedefAnn  <- getReparseInfo curr
    let decl :: Decl Parse
        decl = Decl{
            declInfo = info
          , declKind = DeclTypedef Typedef{
                typedefType
              , typedefAnn
              }
          , declAnn  = NoAnn
          }
    return $ Continue $ Just [decl]

macroExpansion :: Fold M [Decl Parse]
macroExpansion curr = do
    loc <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    recordMacroExpansionAt loc
    return $ Continue Nothing

enumDecl :: Fold M [Decl Parse]
enumDecl curr = do
    info <- getDeclInfo curr
    classification <- HighLevel.classifyDeclaration curr
    case classification of
      DeclarationRegular ->
        pure $ Recurse enumeratorDecl (aux info)
      DeclarationOpaque -> do
        let decl :: Decl Parse
            decl = Decl{
                declInfo = info
              , declKind = DeclEnumOpaque
              , declAnn  = NoAnn
              }
        pure $ Continue $ Just [decl]
      DeclarationForward _ ->
        pure $ Continue $ Nothing
    where
      aux :: DeclInfo Parse -> [EnumConstant] -> M (Maybe [Decl Parse])
      aux info es = let decl = Decl { declInfo = info
                                    , declKind = DeclEnum es
                                    , declAnn  = NoAnn
                                    }
                     in pure $ Just [decl]

enumeratorDecl :: Fold M EnumConstant
enumeratorDecl curr = do
  dispatch curr $ \case
    CXCursor_EnumConstantDecl -> do
      enumConstantName  <- clang_getCursorDisplayName curr
      enumConstantValue <- toInteger <$> clang_getEnumConstantDeclValue curr
      pure $ Continue $ Just EnumConstant { enumConstantName
                                          , enumConstantValue
                                          }
    CXCursor_PackedAttr -> do
      -- No need to handle the `packed` attribute since `libclang` handles it for us.
      pure $ Continue Nothing
    kind -> panicIO $ "Unrecognized cursor in enumerator declaration " <> show kind
