-- | Fold declarations
module HsBindgen.C.Raw.Pass.Parse.Decl (foldDecl) where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Either (partitionEithers)
import GHC.Stack

import Clang.Enum.Simple
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.C.Raw.AST
import HsBindgen.C.Raw.Graph.Includes qualified as IncludeGraph
import HsBindgen.C.Raw.Pass.Parse.IsPass
import HsBindgen.C.Raw.Pass.Parse.Monad
import HsBindgen.C.Raw.Pass.Parse.Type
import HsBindgen.C.Raw.Util
import HsBindgen.Errors

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

foldDecl :: HasCallStack => Fold M [Decl Parsed]
foldDecl curr = do
    loc  <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr

    if nullSourcePath (singleLocPath loc) then
      -- Skip clang built-ins
      return $ Continue Nothing
    else
      dispatchWithArg curr $ \case
        CXCursor_InclusionDirective -> inclusionDirective
        CXCursor_StructDecl         -> structDecl
        CXCursor_TypedefDecl        -> typedefDecl
        kind -> \_ -> panicIO $ "foldDecl: " ++ show kind

{-------------------------------------------------------------------------------
  Info that we collect for all declarations
-------------------------------------------------------------------------------}

getDeclInfo :: MonadIO m => CXCursor -> m (DeclInfo Parsed)
getDeclInfo curr = do
    declId   <- getDeclId curr
    declLoc  <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    return DeclInfo{declId, declLoc}

{-------------------------------------------------------------------------------
  Functions for each kind of AST node
-------------------------------------------------------------------------------}

inclusionDirective :: Fold M a
inclusionDirective curr = do
    loc  <- multiLocExpansion <$> HighLevel.clang_getCursorLocation curr
    file <- clang_getIncludedFile curr
    path <- SourcePath <$> clang_getFileName file
    modify $ IncludeGraph.register (singleLocPath loc) path
    return $ Continue Nothing

structDecl :: Fold M [Decl Parsed]
structDecl curr = do
    return $ Recurse fieldDecl aux
  where
    aux :: [Either [Decl Parsed] (Field Parsed)] -> M (Maybe [Decl Parsed])
    aux xs = do
        info <- getDeclInfo curr
        return $ Just $ otherDecls ++ [Decl info $ DeclStruct fields]
      where
        otherDecls :: [Decl Parsed]
        fields     :: [Field Parsed]
        (otherDecls, fields) = first concat $ partitionEithers xs

fieldDecl :: Fold M (Either [Decl Parsed] (Field Parsed))
fieldDecl curr = do
    kind <- fromSimpleEnum <$> clang_getCursorKind curr
    case kind of
      Right CXCursor_FieldDecl -> do
        name <- clang_getCursorDisplayName curr
        ty   <- fromCXType =<< clang_getCursorType curr
        return $ Continue $ Just (Right $ Field name ty)
      _otherwise ->
        fmap Left <$> foldDecl curr

typedefDecl :: Fold M [Decl Parsed]
typedefDecl curr = do
    info <- getDeclInfo curr
    ty   <- fromCXType =<< clang_getTypedefDeclUnderlyingType curr
    return $ Continue $ Just [Decl info $ DeclTypedef ty]

