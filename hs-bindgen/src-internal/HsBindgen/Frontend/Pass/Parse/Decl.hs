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
    loc <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr

    if nullSourcePath (singleLocPath loc) then
      -- Skip clang built-ins
      -- TODO: Generalize this to selection predicate
      return $ Continue Nothing
    else
      dispatchWithArg curr $ \case
        CXCursor_InclusionDirective -> inclusionDirective
        CXCursor_MacroDefinition    -> macroDefinition
        CXCursor_StructDecl         -> structDecl
        CXCursor_TypedefDecl        -> typedefDecl
        CXCursor_MacroExpansion     -> macroExpansion
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
    file <- clang_getIncludedFile curr
    path <- SourcePath <$> clang_getFileName file
    modifyIncludeGraph $ IncludeGraph.register (singleLocPath loc) path
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
        return $ Recurse fieldDecl (aux info)
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
      -> [Either [Decl Parse] (Field Parse)]
      -> M (Maybe [Decl Parse])
    aux info xs = do
        -- Local declarations inside structs that are not used by any fields
        -- result in implicit fields. Unfortunately, @libclang@ does not make
        -- these visible <https://github.com/llvm/llvm-project/issues/122257>.
        -- This matters, because we need the offsets of these implicit fields.
        -- For now we therefore only try to detect the situation and report an
        -- error when it happens. Hopefully this is anyway very rare.
        unless (null unusedDecls) $
          recordTraceWithCallStack callStack $ UnsupportedImplicitFields (declId info)
        let decl :: Decl Parse
            decl = Decl{
                declInfo = info
              , declKind = DeclStruct fields
              , declAnn  = NoAnn
              }
        return $ Just $ usedDecls ++ [decl]
      where
        otherDecls :: [Decl Parse]
        fields     :: [Field Parse]
        (otherDecls, fields) = first concat $ partitionEithers xs

        fieldDeps :: [QualId Parse]
        fieldDeps = map snd $ mapMaybe (depsOfType . fieldType) fields

        declIsUsed :: Decl Parse -> Bool
        declIsUsed decl = declQualId decl `elem` fieldDeps

        usedDecls, unusedDecls :: [Decl Parse]
        (usedDecls, unusedDecls) = List.partition declIsUsed otherDecls

fieldDecl :: Fold M (Either [Decl Parse] (Field Parse))
fieldDecl curr = do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        fieldName   <- clang_getCursorDisplayName curr
        fieldType   <- fromCXType =<< clang_getCursorType curr
        fieldOffset <- fromIntegral <$> clang_Cursor_getOffsetOfField curr
        fieldAnn    <- getReparseInfo curr
        return $ Continue . Just . Right $ Field{
            fieldName
          , fieldType
          , fieldOffset
          , fieldAnn
          }
      _otherwise -> do
        fmap Left <$> foldDecl curr

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


