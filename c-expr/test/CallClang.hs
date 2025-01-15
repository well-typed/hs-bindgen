{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CallClang
  ( CType(..)
  , queryClangForResultType
  , getExpansionTypeMapping
  , queryClangBuildTargetTriple
  )
  where

-- base
import Control.Exception
  ( bracket )
--import Control.Monad
--  ( unless )
import Control.Monad.IO.Class
  ( MonadIO(liftIO) )
import Data.Foldable
  ( toList )
import Data.List
  ( intercalate, partition )
import Data.Maybe
  ( listToMaybe )
import Text.Read
  ( readMaybe )

-- containers
import Data.IntMap.Strict
  ( IntMap )
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict
  ( Map )
import Data.Map.Strict qualified as Map

-- directory
import System.Directory
  ( removeFile )

-- temporary
import System.IO.Temp qualified as Tmp

-- text
import Data.Text
  ( Text )
import Data.Text qualified as Text

-- vec
import Data.Vec.Lazy
  ( Vec(..) )
import Data.Vec.Lazy qualified as Vec

-- hs-bindgen-libclang
import HsBindgen.Clang.HighLevel qualified as Clang
  hiding ( clang_getCursorLocation )
import HsBindgen.Clang.HighLevel.Types qualified as Clang
import HsBindgen.Clang.LowLevel.Core qualified as Clang
  hiding ( clang_visitChildren, clang_getCursorSpelling )
import HsBindgen.Clang.Args qualified as Clang

-- hs-bindgen-runtime
import HsBindgen.Runtime.Enum.Simple qualified as Clang
  ( fromSimpleEnum )
import HsBindgen.Runtime.Enum.Bitfield qualified as Clang
  ( bitfieldEnum )

-- c-expr
import C.Type

--------------------------------------------------------------------------------

-- | A C type, extended with typedefs for use with Clang.
data CType
  = TypeDef !Text
  | CType !(Type CType)
  deriving stock ( Eq, Ord )
instance Show CType where
  show (TypeDef ty) = Text.unpack ty
  show (CType ty) = showTypeAsCType ty ""

showCType :: CType -> String -> String
showCType (TypeDef ty) s = Text.unpack ty ++ " " ++ s
showCType (CType ty) s = showTypeAsCType ty s


-- | Parse a 'CXType' into a 'Type'.
parseClangType :: Clang.CXType -> IO ( Maybe CType )
parseClangType cxTy = do
  ty <- Clang.clang_getTypeSpelling cxTy
  case Clang.fromSimpleEnum $ Clang.cxtKind cxTy of
    Left {} -> return $ Nothing
    Right ki -> do
      case ki of
        Clang.CXType_Invalid             -> return Nothing
        Clang.CXType_Unexposed           -> return Nothing
        Clang.CXType_Void                -> return $ Just $ CType Void
        Clang.CXType_Bool                -> return $ Just $ CType $ Arithmetic $ Integral $ Bool
        Clang.CXType_Char_U              -> return $ Just $ CType $ Arithmetic $ Integral $ CharLike UChar
        Clang.CXType_UChar               -> return $ Just $ CType $ Arithmetic $ Integral $ CharLike UChar
        Clang.CXType_Char16              -> return Nothing
        Clang.CXType_Char32              -> return Nothing
        Clang.CXType_UShort              -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ Short    Unsigned
        Clang.CXType_UInt                -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ Int      Unsigned
        Clang.CXType_ULong               -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ Long     Unsigned
        Clang.CXType_ULongLong           -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ LongLong Unsigned
        Clang.CXType_UInt128             -> return Nothing
        Clang.CXType_Char_S              -> return $ Just $ CType $ Arithmetic $ Integral $ CharLike Char
        Clang.CXType_SChar               -> return $ Just $ CType $ Arithmetic $ Integral $ CharLike SChar
        Clang.CXType_WChar               -> return Nothing
        Clang.CXType_Short               -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ Short    Signed
        Clang.CXType_Int                 -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ Int      Signed
        Clang.CXType_Long                -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ Long     Signed
        Clang.CXType_LongLong            -> return $ Just $ CType $ Arithmetic $ Integral $ IntLike $ LongLong Signed
        Clang.CXType_Int128              -> return Nothing
        Clang.CXType_Float               -> return $ Just $ CType $ Arithmetic $ FloatLike FloatType
        Clang.CXType_Double              -> return $ Just $ CType $ Arithmetic $ FloatLike DoubleType
        Clang.CXType_LongDouble          -> return Nothing
        Clang.CXType_NullPtr             -> return Nothing
        Clang.CXType_Overload            -> return Nothing
        Clang.CXType_Dependent           -> return Nothing
        Clang.CXType_ObjCId              -> return Nothing
        Clang.CXType_ObjCClass           -> return Nothing
        Clang.CXType_ObjCSel             -> return Nothing
        Clang.CXType_Float128            -> return Nothing
        Clang.CXType_Half                -> return Nothing
        Clang.CXType_Float16             -> return Nothing
        Clang.CXType_ShortAccum          -> return Nothing
        Clang.CXType_Accum               -> return Nothing
        Clang.CXType_LongAccum           -> return Nothing
        Clang.CXType_UShortAccum         -> return Nothing
        Clang.CXType_UAccum              -> return Nothing
        Clang.CXType_ULongAccum          -> return Nothing
        Clang.CXType_BFloat16            -> return Nothing
        Clang.CXType_Ibm128              -> return Nothing
        Clang.CXType_Complex             -> return Nothing
        Clang.CXType_Pointer             -> fmap ( CType . Ptr ) <$> ( parseClangType =<< Clang.clang_getPointeeType cxTy )
        Clang.CXType_BlockPointer        -> return Nothing
        Clang.CXType_LValueReference     -> return Nothing
        Clang.CXType_RValueReference     -> return Nothing
        Clang.CXType_Record              -> return $ Just $ TypeDef ty
        Clang.CXType_Enum                -> do { tyDecl <- Clang.clang_getTypeDeclaration cxTy
                                               ; enumTy <- Clang.clang_getEnumDeclIntegerType tyDecl
                                               ; parseClangType enumTy }
        Clang.CXType_Typedef             -> do { canTy <- Clang.clang_getCanonicalType cxTy
                                               ; parseClangType canTy }
        Clang.CXType_ObjCInterface       -> return Nothing
        Clang.CXType_ObjCObjectPointer   -> return Nothing
        Clang.CXType_FunctionNoProto     -> return Nothing
        Clang.CXType_FunctionProto       -> return Nothing
        Clang.CXType_ConstantArray       -> return Nothing
        Clang.CXType_Vector              -> return Nothing
        Clang.CXType_IncompleteArray     -> return Nothing
        Clang.CXType_VariableArray       -> return Nothing
        Clang.CXType_DependentSizedArray -> return Nothing
        Clang.CXType_MemberPointer       -> return Nothing
        Clang.CXType_Auto                -> do { canTy <- Clang.clang_getCanonicalType cxTy
                                               ; parseClangType canTy }
        Clang.CXType_Elaborated          -> do { namedTy <- Clang.clang_Type_getNamedType cxTy
                                               ; parseClangType namedTy }

-- | Query @clang@ for canonical names for types.
getExpansionTypeMapping :: Clang.ClangArgs -> [ CType ] -> IO ( Map CType CType )
getExpansionTypeMapping clangArgs tys = do
  cxTys <- Map.fromList <$> clangVisitChildren clangArgs ( getCanonicalType Nothing ) sourceProgram
  traverse ( \ cxTy -> expectJust cxTy =<< parseClangType cxTy ) cxTys

  where

    getCanonicalType :: Maybe Int -> Clang.Fold IO ( CType, Clang.CXType )
    getCanonicalType inTestFunDecl cursor = do
      loc <- liftIO $ Clang.clang_getCursorLocation cursor
      inMain <- liftIO $ Clang.clang_Location_isFromMainFile loc
      if not inMain
      then
        return $ Clang.Continue Nothing
      else do
        cursorKind <- liftIO $ Clang.fromSimpleEnum <$> Clang.clang_getCursorKind cursor
        case cursorKind of
          Right kind
            | Clang.CXCursor_FunctionDecl <- kind
            -> do
              mbFunNm <- liftIO $ Clang.getUserProvided <$> Clang.clang_getCursorSpelling cursor
              return $
                case mbFunNm of
                  Just funNm
                    | let ( nm, nb ) = Text.splitAt 6 funNm
                    , nm == "testFn"
                    , Just i <- readMaybe ( Text.unpack nb )
                    -> Clang.Recurse ( getCanonicalType ( Just i ) ) listToMaybe
                  _ ->
                    Clang.Continue Nothing
            | Just nb <- inTestFunDecl
            , Clang.CXCursor_DeclRefExpr <- kind
            -> do
              cxTy  <- liftIO $ Clang.clang_getCursorType    cursor
              mbRhsTy <- parseClangType cxTy
              let lhsTy = tyPairs IntMap.! nb
                  res
                    | Just rhsTy <- mbRhsTy
                    , lhsTy /= rhsTy
                    -- Don't bother when a type is mapped to itself.
                    = Just ( lhsTy, cxTy )
                    | otherwise
                    = Nothing
              return $ Clang.Continue res
          _ -> return $ Clang.Recurse ( getCanonicalType inTestFunDecl ) listToMaybe

    tyPairs :: IntMap CType
    tyPairs = IntMap.fromList [ (i, ty) | i <- [ (1 :: Int) .. ] | ty <- tys ]

    sourceProgram :: String
    sourceProgram = unlines $ concat
      [ [ "#include <stddef.h>" ]
      , [ unlines
            [ "static " ++ showTy "testFn" ++ show i ++ "(" ++ showTy "x" ++ ") {"
            , "  return x;"
            , "}"
            ]
        | ( i, ty ) <- IntMap.assocs tyPairs
        , let showTy = showCType ty
        ]
      ]

    expectJust :: Clang.CXType -> Maybe a -> IO a
    expectJust cxTy =
      \case
        Nothing -> do
          tyNm <- Clang.clang_getTypeSpelling cxTy
          error $ unlines
            [ "getExpansionTypeMapping: could not parse CXType " ++ show cxTy
            , Text.unpack tyNm ]
        Just ty -> return ty

queryClangForResultType :: forall n. Clang.ClangArgs -> Vec n CType -> ( Vec n String -> String ) -> IO ( Maybe CType )
queryClangForResultType clangArgs tys op =
  listToMaybe <$> clangVisitChildren clangArgs ( extractType ( False, False ) ) sourceProgram
  where
    n :: Int
    n = length tys

    args, typedArgs :: Vec n String
    args = Vec.imap (\ i _ -> "x_" ++ show i) tys
    typedArgs = Vec.imap ( \i ty -> showCType ty ( "x_" ++ show i ) ) tys

    sourceProgram :: String
    sourceProgram = unlines $ concat $
      [ [ "// #include <stdio.h>"
        , "#include <stddef.h>"
        , "#define bool _Bool"
        , ""
        ]
      , [ "typedef struct { void **unused; } " ++ s ++ ";"
        | i <- [ 1 .. n ]
        , let s = "ty_" ++ show i
        ]
      , [ ""
        , "static int testFunction (" ++ intercalate ", " (toList typedArgs) ++ ") {"
        , "  (void)(" ++ op args ++ ");"
        , "  return 0;"
        , "}"
        ]
      ]

    extractType :: ( Bool, Bool ) -> Clang.CXCursor -> IO ( Clang.Next IO CType )
    extractType ( inTestFunDecl, inCast ) cursor = do
      loc <- Clang.clang_getCursorLocation cursor
      inMain <- Clang.clang_Location_isFromMainFile loc
      if not inMain
      then
        return $ Clang.Continue Nothing
      else do
        cursorKind <- Clang.fromSimpleEnum <$> Clang.clang_getCursorKind cursor
        case cursorKind of
          Right kind
            | Clang.CXCursor_CStyleCastExpr <- kind
            -> return $ Clang.Recurse ( extractType ( inTestFunDecl, True ) ) listToMaybe
            | Clang.CXCursor_FunctionDecl <- kind
            -> do
              funNm <- Clang.getUserProvided <$> Clang.clang_getCursorSpelling cursor
              return $
                if funNm == Just "testFunction"
                then
                  Clang.Recurse ( extractType ( True, False ) ) listToMaybe
                else
                  Clang.Continue Nothing
            | inTestFunDecl
            , inCast
            , kind == Clang.CXCursor_UnaryOperator || kind == Clang.CXCursor_BinaryOperator
            -> do
              cxTy <- Clang.clang_getCursorType cursor
              mbTy <- parseClangType cxTy
              return $ Clang.Break mbTy
          _ -> return $ Clang.Recurse ( extractType ( inTestFunDecl, inCast ) ) listToMaybe

clangGetTranslationUnit :: Clang.ClangArgs -> String -> IO Clang.CXTranslationUnit
clangGetTranslationUnit userClangArgs srcContents = do
  --putStrLn srcContents

  fp <- Tmp.writeSystemTempFile "src.c" srcContents
  -- TODO: we wouldn't need to create a temporary file if we supported
  -- unsaved files in 'clang_parseTranslationUnit'.
  index  <- Clang.clang_createIndex Clang.DontDisplayDiagnostics
  let clangArgs =
        userClangArgs
          { Clang.clangOtherArgs =
              Clang.clangOtherArgs userClangArgs
                ++
                [ "-Werror=pointer-integer-compare"
                , "-Werror=compare-distinct-pointer-types"
                ]
          }
      flags = [ Clang.CXTranslationUnit_DetailedPreprocessingRecord
              , Clang.CXTranslationUnit_IncludeAttributedTypes
              , Clang.CXTranslationUnit_VisitImplicitAttributes ]
  unit   <- Clang.clang_parseTranslationUnit index fp clangArgs (Clang.bitfieldEnum flags)
  removeFile fp
  return unit

clangVisitChildren :: Clang.ClangArgs -> Clang.Fold IO a -> String -> IO [a]
clangVisitChildren args f srcContents = do

  unit <- clangGetTranslationUnit args srcContents

  diags  <- Clang.clang_getDiagnostics Nothing unit Nothing
  let (errors, _warnings) = partition diagnosticIsSevere diags

{-
  unless (null errors) $
    putStrLn $ unlines $ map (\ x -> show x ++ "\n") errors
  unless (null _warnings) $
    putStrLn $ unlines $ ("WARNINGS:":) (map (\ x -> show x ++ "\n") _warnings)
-}

  if null errors
  then do
    rootCursor <- Clang.clang_getTranslationUnitCursor unit
    Clang.clang_visitChildren rootCursor f
  else
    return []

diagnosticIsSevere :: Clang.Diagnostic -> Bool
diagnosticIsSevere diag =
  Clang.diagnosticIsError diag ||
    diagTextIsSevere ( Clang.diagnosticSpelling diag )
  where
    diagTextIsSevere :: Text -> Bool
    diagTextIsSevere diagTxt =
      or
        -- Turn warnings about comparison between e.g. 'char *' and 'int'
        -- into errors.
        --
        -- NB: for some reason, -Werror=pointer-integer-compare isn't sufficient
        -- to achieve this.
        [ Text.isPrefixOf "ordered comparison between pointer and integer" diagTxt
        ]

-------------------------------------------------------------------------------

-- | Get the target triple of the build system, as reported by Clang.
queryClangBuildTargetTriple :: IO Text
queryClangBuildTargetTriple =
  getTriple =<< clangGetTranslationUnit Clang.defaultClangArgs ""
  where
    getTriple unit =
      bracket
          ( Clang.clang_getTranslationUnitTargetInfo unit )
          Clang.clang_TargetInfo_dispose
          Clang.clang_TargetInfo_getTriple
