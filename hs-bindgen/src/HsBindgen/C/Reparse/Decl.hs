{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsBindgen.C.Reparse.Decl where


-- base
import Control.Monad
  ( void )
import Data.Either
  ( partitionEithers )
import Data.Foldable
  ( toList )
import Data.Functor
  ( ($>) )
import Data.Kind qualified as Hs

import Data.List
  ( intercalate )
import Data.List.NonEmpty qualified as NE
import Data.Maybe
  ( isJust, mapMaybe )
import Data.Type.Bool
  ( If )
import Data.Type.Equality
  ( type (==) )
import Numeric.Natural
  ( Natural )

-- containers
import Data.Map.Strict qualified as Map

-- parsec
import Text.Parsec

-- text
import Data.Text
  ( Text )

-- hs-bindgen
import HsBindgen.C.AST
  ( TokenSpelling(..), tokenSpelling )
import HsBindgen.C.AST.Literal
  ( IntegerLiteral(..) )

import HsBindgen.C.AST.Name
import HsBindgen.C.AST.Type
import HsBindgen.C.Reparse.Common (reparseName)
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Reparse.Type

-- TODO: re-using the macro expression parser for simplicity
import HsBindgen.C.AST.Macro

  ( MExpr (..), MTerm (..) )
import HsBindgen.C.Reparse.Macro
  ( mExpr )

import HsBindgen.C.Tc.Macro qualified as TcMacro

import HsBindgen.Clang.LowLevel.Core
  ( CXTokenKind(..) )

-- | Field declaration (in a struct)
reparseFieldDecl :: TcMacro.TypeEnv -> Reparse (Type, CName)
reparseFieldDecl macroTys = do
  ( specs, Declarator ptrs decl ) <-
    manyTillLookahead
      ( reparseDeclarationSpecifier macroTys )
      ( reparseDeclarator @Concrete macroTys )
  _mbBitSize <- optionMaybe $
    ( do { punctuation ":" ; reparseSizeExpression } <?> "bit size" )
  eof
  case declaratorType specs decl of
    Left err -> fail err
    Right ( baseTy, nm ) ->
      return ( mkPtr ptrs baseTy, nm )

-- | Function declaration.
reparseFunDecl :: TcMacro.TypeEnv -> Reparse (([Type],Type), CName)
reparseFunDecl macroTys = do
  ( specs, Declarator ptrs decl ) <-
    manyTillLookahead
      ( reparseDeclarationSpecifier macroTys )
      ( reparseDeclarator @Concrete macroTys )
  eof
  case declaratorType specs decl of
    Left err -> fail err
    Right ( ty, nm ) ->
      case ty of
        TypeFun args res ->
          return ((args, mkPtr ptrs res), nm)
        _ ->
          fail $ "expected a function type, but got: " ++ show ty

declaratorType :: [ DeclarationSpecifier ]
               -> DirectDeclarator abs
               -> Either String ( Type, If (abs == Concrete) CName () )
declaratorType specs decl =
  case declarationSpecifiersType specs of
    Nothing -> Left "declarator missing a type"
    Just baseTy ->
      case declaratorTypeAndName baseTy decl of
        Left err -> Left err
        Right tyAndNm ->
          return tyAndNm

manyTillLookahead :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m e -> ParsecT s u m ([a], e)
manyTillLookahead p end = go
  where
    go = choice
      [ try $
        do { e <- end
           ; eof <|> notFollowedBy (void p <|> void end)
           ; return ([], e)
           }
      , do { a <- p; (as, e) <- go ; return (a:as, e)}
      ]

mkPtr :: Foldable f => Pointers f -> Type -> Type
mkPtr (Pointers l) = go ( toList l )
  where
    go [] a = a
    go (_:r) a = go r (TypePointer a)

declarationSpecifiersType :: [DeclarationSpecifier] -> Maybe Type
declarationSpecifiersType specs =
  case mapMaybe isTy specs of
    [ ty ] -> Just ty
    [] -> Nothing
    tys@(_:_:_) ->
      error $ unlines
        [ "declarationSpecifiersType: multiple types"
        , show tys
        ]

  where
    isTy :: DeclarationSpecifier -> Maybe Type
    isTy = \case
      DeclFunctionSpecifier {} -> Nothing
      DeclStorageSpecifier  {} -> Nothing
      DeclTypeQualifierSpecifier  tq ->
        case tq of
          TQS_TypeQualifier {} -> Nothing
          TQS_AlignmentSpecifier {} -> Nothing
          TQS_TypeSpecifier ts ->
            case ts of
              TypeSpecifier ty -> Just ty
              TypeDefTypeSpecifier nm -> Just $ TypeTypedef nm

declaratorTypeAndName :: Type -> DirectDeclarator abs -> Either String ( Type, If ( abs == Concrete ) CName () )
declaratorTypeAndName ty = \case
  IdentifierDeclarator (Identifier nm _) ->
    Right (ty, nm)
  ParenDeclarator d ->
    case d of
      Declarator ptrs dd ->
        declaratorTypeAndName (mkPtr ptrs ty) dd
      PointerAbstractDeclarator ptrs ->
        return ( mkPtr ptrs ty, () )
  ArrayDirectDeclarator (ArrayDeclarator { arrayDirectDeclarator = d, arraySize = sz }) -> do
    arrTy <-
      case sz of
        ArrayNoSize ->
          Right $ TypeIncompleteArray ty
        VLA ->
          Right $ TypeIncompleteArray ty
        ArraySize ( SizeExpression sizeExpr ) ->
          case literalMaybe sizeExpr of
            Just n -> Right $ TypeConstArray n ty
            Nothing -> Left $ "cannot evaluate array size: " ++ show sizeExpr
    declaratorTypeAndName arrTy d
  FunctionDirectDeclarator
    (FunctionDeclarator
      { functionDirectDeclarator = d
      , functionParameters = xs
      , functionVariadic = variadic
      }
    ) -> do
    let (missingTys, argTys) = parameterTypes xs
    funTy <-
      case missingTys of
        [] ->
          if variadic
          then
            Left "unsupported variadic function declaration"
          else
            Right $ TypeFun argTys ty
        ms@(_:rest) -> do
          let s = if null rest then "" else "s"
          Left $
            "missing type specifier" ++ s ++ " in function parameter" ++ s
              ++ " " ++ intercalate ", " ( map show ms )
    declaratorTypeAndName funTy d

parameterTypes :: [Parameter] -> ([Int], [Type])
parameterTypes ps =
  partitionEithers $
    zipWith aux [1..] ps
  where
    aux i ( Parameter { parameterDeclSpecifiers = specs, parameterDeclarator = pDecl }) =
      case pDecl of
        ParameterDeclarator ( Declarator ptrs d ) ->
          case declaratorType specs d of
            Left {} -> Left i
            Right ( ty, _ ) ->
              -- TODO: pointers probably wrong?
              Right $ mkPtr ptrs ty
        ParameterAbstractDeclarator mbAbs ->
          case mbAbs of
            Nothing ->
              -- TODO: no idea if this is right?
              case declarationSpecifiersType specs of
                Nothing -> Left i
                Just ty -> Right ty
            Just decl ->
              case decl of
                Declarator ptrs d ->
                  case declaratorType specs d of
                    Left {} -> Left i
                    Right ( ty, _ ) ->
                      Right $ mkPtr ptrs ty
                PointerAbstractDeclarator ptrs ->
                  case declarationSpecifiersType specs of
                    Nothing -> Left i
                    Just ty -> Right $ mkPtr ptrs ty


literalMaybe :: MExpr -> Maybe Natural
literalMaybe = \case
  MTerm tm ->
    case tm of
      MInt ( IntegerLiteral { integerLiteralValue = i }) -> Just $ fromIntegral i
      _ -> Nothing
  MApp {} -> Nothing
-- TODO: change this to to support simple macro defined constants, e.g.:
--
--   int arr[SOME_CONSTANT];


data FieldDecl
  = FieldDecl { fieldDeclarationSpecifiers :: [DeclarationSpecifier]
              , fieldDeclarator :: Declarator Concrete
              , fieldBitSize    :: Maybe SizeExpression
              }
  deriving stock ( Eq, Show )

reparseDeclarator :: forall abs. KnownDeclaratorType abs => TcMacro.TypeEnv -> Reparse (Declarator abs)
reparseDeclarator macroTys = do
  ptr <- many reparsePointer
  case knownDeclarator @abs of
    SConcrete -> do
      decl <- reparseDirectDeclarator @abs macroTys
      return $
        Declarator ( Pointers ptr ) decl
    SAbstract ->
      case ptr of
        p:ps -> do
          mbDecl <- optionMaybe $ reparseDirectDeclarator @abs macroTys
          return $
            case mbDecl of
              Nothing ->
                PointerAbstractDeclarator ( Pointers ( p NE.:| ps ) )
              Just decl ->
                Declarator ( Pointers ptr ) decl
        [] -> do
          decl <- reparseDirectDeclarator @abs macroTys
          return $
            Declarator ( Pointers ptr ) decl

reparsePointer :: Reparse ( [ AttributeSpecifier ], [ TypeQualifier ] )
reparsePointer = do
  punctuation "*"
  attrs <- many reparseAttributeSpecifier
  tqs   <- many reparseTypeQualifier
  return ( attrs, tqs )
  <?> "pointer"

reparseDirectDeclarator
  :: forall abs. KnownDeclaratorType abs
  => TcMacro.TypeEnv -> Reparse ( DirectDeclarator abs )
reparseDirectDeclarator macroTys = do
  decl <-
    choice $
      [ ParenDeclarator <$> parens ( reparseDeclarator @abs macroTys )
      ]
      ++
      case knownDeclarator @abs of
        SConcrete -> [ IdentifierDeclarator <$> reparseIdentifier ]
        SAbstract -> []
  withArrayOrFunctionSuffixes macroTys decl

withArrayOrFunctionSuffixes
  :: forall abs. KnownDeclaratorType abs
  => TcMacro.TypeEnv -> DirectDeclarator abs -> Reparse ( DirectDeclarator abs )
withArrayOrFunctionSuffixes macroTys decl =
  choice
    [ do { newDecl <- reparseArrayDeclarator @abs macroTys decl
         ; withArrayOrFunctionSuffixes macroTys (ArrayDirectDeclarator newDecl)
         }
    , do { newDecl <- reparseFunctionDeclarator @abs macroTys decl
         ; withArrayOrFunctionSuffixes macroTys (FunctionDirectDeclarator newDecl)
         }
    , return decl
    ]

reparseIdentifier :: Reparse Identifier
reparseIdentifier = do
  i <- tokenOfKind CXToken_Identifier (Just . CName)
  attrs <- many reparseAttributeSpecifier
  return $ Identifier i attrs
  <?> "identifier"

abstractOptional :: forall abs a. KnownDeclaratorType abs => Reparse a -> Reparse ( AbstractOptional abs a )
abstractOptional p = case knownDeclarator @abs of
  SConcrete -> NonAbstract <$> p
  SAbstract -> choice [ NonAbstract <$> p, pure NoAbstract ]

reparseArrayDeclarator :: TcMacro.TypeEnv -> DirectDeclarator abs -> Reparse ( ArrayDeclarator abs )
reparseArrayDeclarator macroTys decl = do
  punctuation "["
  ( static, tq, sz ) <-
    choice
      [ do { s1 <- isJust <$> optionMaybe (keyword "static")
           ; tq <- many $ reparseTypeQualifierSpecifier macroTys
           ; s2 <- isJust <$> optionMaybe (keyword "static")
           ; size <- optionMaybe reparseSizeExpression
           ; return ( s1 || s2, tq, case size of { Nothing -> ArrayNoSize; Just s -> ArraySize s } )
           }
      , do { tq <- many $ reparseTypeQualifierSpecifier macroTys
           ; punctuation "*"
           ; return ( False, tq, VLA )
           }
      ]
  punctuation "]"
  attrs <- many reparseAttributeSpecifier
  return $
    ArrayDeclarator
     { arrayDirectDeclarator = decl
     , arrayStatic           = static
     , arrayTypeQualifiers   = tq
     , arraySize             = sz
     , arrayAttributes       = attrs
     }
  <?> "array declarator"


reparseFunctionDeclarator :: TcMacro.TypeEnv -> DirectDeclarator abs -> Reparse ( FunctionDeclarator abs )
reparseFunctionDeclarator macroTys decl = do
  ( params, variadic ) <- parens $ reparseParameterList macroTys
  attrs <- many reparseAttributeSpecifier
  return $
    FunctionDeclarator
      { functionDirectDeclarator = decl
      , functionParameters       = params
      , functionVariadic         = variadic
      , functionAttributes       = attrs
      }
  <?> "function declarator"

reparseParameterList :: TcMacro.TypeEnv -> Reparse ( [ Parameter ], Bool )
reparseParameterList macroTys =
  choice
    [ do { punctuation "..."; return ( [], True ) } <?> "varargs"
    , do { p <- reparseParameter macroTys
         ; nxt <- optionMaybe $ do { punctuation ","; reparseParameterList macroTys }
         ; return $
            case nxt of
              Nothing -> ([p], False)
              Just (ps, v) -> (p:ps, v) }
    , return ( [], False )
    ]

reparseParameter :: TcMacro.TypeEnv -> Reparse Parameter
reparseParameter macroTys = do
  attrs <- many reparseAttributeSpecifier
  declSpecs <- many1 $ reparseDeclarationSpecifier macroTys
  decl <- reparseParameterDeclarator macroTys
  return $ Parameter attrs declSpecs decl
  <?> "function parameter"

reparseParameterDeclarator :: TcMacro.TypeEnv -> Reparse ParameterDeclarator
reparseParameterDeclarator macroTys =
  choice
    [ ParameterDeclarator <$> reparseDeclarator @Concrete macroTys
    , ParameterAbstractDeclarator <$> optionMaybe ( reparseDeclarator @Abstract macroTys )
    ]

data DeclaratorType = Concrete | Abstract
  deriving stock ( Eq, Show )

type SDeclaratorType :: DeclaratorType -> Hs.Type
data SDeclaratorType abs where
  SConcrete :: SDeclaratorType Concrete
  SAbstract :: SDeclaratorType Abstract

deriving stock instance Eq   ( SDeclaratorType abs )
deriving stock instance Show ( SDeclaratorType abs )

class KnownDeclaratorType abs where
  knownDeclarator :: SDeclaratorType abs
instance KnownDeclaratorType Concrete where
  knownDeclarator = SConcrete
instance KnownDeclaratorType Abstract where
  knownDeclarator = SAbstract



type Declarator :: DeclaratorType -> Hs.Type
data Declarator abs where
  Declarator
    :: { declaratorPointer :: Pointers []
       , directDeclarator :: DirectDeclarator abs
       }
    -> Declarator abs
  PointerAbstractDeclarator
    :: { abstractPointerDeclaratorPointer :: Pointers NE.NonEmpty }
    -> Declarator Abstract
deriving stock instance Eq   ( Declarator abs )
deriving stock instance Show ( Declarator abs )


newtype Pointers f = Pointers ( f ( [ AttributeSpecifier ], [ TypeQualifier ] ) )

deriving stock instance ( forall a. Eq   a => Eq   ( f a ) ) => Eq ( Pointers f )
deriving stock instance ( forall a. Show a => Show ( f a ) ) => Show ( Pointers f )

type DirectDeclarator :: DeclaratorType -> Hs.Type
data DirectDeclarator abs where
  IdentifierDeclarator :: Identifier -> DirectDeclarator Concrete
  ParenDeclarator :: Declarator abs -> DirectDeclarator abs
  ArrayDirectDeclarator :: ArrayDeclarator abs -> DirectDeclarator abs
  FunctionDirectDeclarator :: FunctionDeclarator abs -> DirectDeclarator abs
deriving stock instance Eq   ( DirectDeclarator abs )
deriving stock instance Show ( DirectDeclarator abs )

data Identifier = Identifier CName [AttributeSpecifier]
  deriving stock ( Eq, Show )

type ArrayDeclarator :: DeclaratorType -> Hs.Type
data ArrayDeclarator abs
  = ArrayDeclarator
    { arrayDirectDeclarator :: DirectDeclarator abs
    , arrayStatic           :: Bool
    , arrayTypeQualifiers   :: [TypeQualifierSpecifier]
    , arraySize             :: ArraySize
    , arrayAttributes       :: [AttributeSpecifier]
    }
deriving stock instance Eq   ( ArrayDeclarator abs )
deriving stock instance Show ( ArrayDeclarator abs )

data ArraySize
  = ArraySize SizeExpression
  | ArrayNoSize
  | VLA
  deriving stock ( Eq, Show )

type AbstractOptional :: DeclaratorType -> Hs.Type -> Hs.Type
data AbstractOptional abs a where
  NonAbstract :: a -> AbstractOptional abs a
  NoAbstract :: AbstractOptional Abstract a
deriving stock instance Eq   a => Eq   ( AbstractOptional abs a )
deriving stock instance Show a => Show ( AbstractOptional abs a )

type AbstractOmitted :: DeclaratorType -> Hs.Type -> Hs.Type
data AbstractOmitted abs a where
  NotOmitted :: a -> AbstractOmitted Concrete a
  Omitted    :: AbstractOmitted Abstract a
deriving stock instance Eq   a => Eq   ( AbstractOmitted abs a )
deriving stock instance Show a => Show ( AbstractOmitted abs a )

type FunctionDeclarator :: DeclaratorType -> Hs.Type
data FunctionDeclarator abs
  = FunctionDeclarator
  { functionDirectDeclarator :: DirectDeclarator abs
  , functionParameters :: [Parameter]
  , functionVariadic :: Bool
      -- ^ whether there is a final @...@
  , functionAttributes :: [AttributeSpecifier]
  }
  deriving stock ( Eq, Show )

data Parameter =
  Parameter
    { parameterAttributes :: [AttributeSpecifier]
    , parameterDeclSpecifiers :: [DeclarationSpecifier]
    , parameterDeclarator :: ParameterDeclarator }
  deriving stock ( Eq, Show )

data ParameterDeclarator
  = ParameterDeclarator ( Declarator Concrete )
  | ParameterAbstractDeclarator ( Maybe ( Declarator Abstract ) )
  deriving stock ( Eq, Show )

newtype AttributeSpecifier = AttributeSpecifier [ Attribute ]
  deriving stock ( Eq, Show )

data Attribute
  = Attribute
  { attributeToken :: AttributeToken
  , attributeArgumentClause :: Maybe [BalancedToken]
  }
  deriving stock ( Eq, Show )

data AttributeToken
  = StandardAttribute Identifier
  | AttributePrefixedToken Identifier Identifier
  deriving stock ( Eq, Show )

data BalancedToken
  = BalancedTokenNormal Text
  | BalancedToken [BalancedToken]
  deriving stock ( Eq, Show )



reparseAttributeSpecifier :: Reparse AttributeSpecifier
reparseAttributeSpecifier = do
  try $ do { punctuation "[" ; punctuation "[" }
  attrs <- many reparseAttribute
  do { punctuation "]" ; punctuation "]" }
  return $ AttributeSpecifier attrs
  <?> "attribute"

reparseAttribute :: Reparse Attribute
reparseAttribute = do
  attrToken <- reparseAttributeToken
  argClause <- optionMaybe $ parens (many reparseBalancedToken)
  return $
    Attribute attrToken argClause

reparseAttributeToken :: Reparse AttributeToken
reparseAttributeToken = do
  pref <- reparseIdentifier
  mbSuff <- optionMaybe $ do { punctuation "::"; reparseIdentifier }
  return $
    case mbSuff of
      Nothing -> StandardAttribute pref
      Just suff -> AttributePrefixedToken pref suff

reparseBalancedToken :: Reparse BalancedToken
reparseBalancedToken =
  choice
    [ reparseParenBalancedToken
    , BalancedTokenNormal . getTokenSpelling . tokenSpelling <$> anyToken
    ]

reparseParenBalancedToken :: Reparse BalancedToken
reparseParenBalancedToken = do
  closer <- choice [ punctuation "(" $> ")"
                   , punctuation "[" $> "]"
                   , punctuation "{" $> "}"
                   ]
  inner <- many reparseBalancedToken
  punctuation closer
  return $ BalancedToken inner


data DeclarationSpecifier
  = DeclStorageSpecifier        StorageClassSpecifier
  | DeclTypeQualifierSpecifier  TypeQualifierSpecifier
  | DeclFunctionSpecifier       FunctionSpecifier
  deriving stock ( Eq, Show )

reparseDeclarationSpecifier :: TcMacro.TypeEnv -> Reparse DeclarationSpecifier
reparseDeclarationSpecifier macroTys = do
  choice
    [ DeclStorageSpecifier       <$> reparseStorageClassSpecifier
    , DeclTypeQualifierSpecifier <$> reparseTypeQualifierSpecifier macroTys
    , DeclFunctionSpecifier      <$> reparseFunctionSpecifier
    ]

data TypeQualifier
  = TQ_const
  | TQ_restrict
  | TQ_volatile
  | TQ__Atomic
  deriving stock ( Eq, Show )

data TypeSpecifier
  = TypeSpecifier Type
  | TypeDefTypeSpecifier CName
  deriving stock ( Eq, Show )

data AlignmentSpecifier
  = AlignAsTypeName TypeName
  | AlignAsConstExpr SizeExpression
  deriving stock ( Eq, Show )

reparseAlignmentSpecifier :: TcMacro.TypeEnv -> Reparse AlignmentSpecifier
reparseAlignmentSpecifier macroTys = do
  void $ keyword "alignas"
  parens $
    choice
      [ AlignAsTypeName <$> reparseTypeName macroTys
      , AlignAsConstExpr <$> reparseSizeExpression
      ]
  <?> "alignment"

data TypeName
  = TypeName [TypeSpecifier] [AttributeSpecifier] (Maybe (Declarator Abstract))
  deriving stock ( Eq, Show )

reparseTypeName :: TcMacro.TypeEnv -> Reparse TypeName
reparseTypeName macroTys = do
  tySpecs <- many $ reparseTypeSpecifier macroTys
  attrs <- many reparseAttributeSpecifier
  mbDecl <- optionMaybe $ reparseDeclarator @Abstract macroTys
  return $
    TypeName tySpecs attrs mbDecl

data TypeQualifierSpecifier
  = TQS_TypeQualifier TypeQualifier
  | TQS_TypeSpecifier TypeSpecifier
  | TQS_AlignmentSpecifier AlignmentSpecifier
  deriving stock ( Eq, Show )

reparseTypeQualifierSpecifier :: TcMacro.TypeEnv -> Reparse TypeQualifierSpecifier
reparseTypeQualifierSpecifier macroTys = choice
  [ TQS_TypeQualifier <$> reparseTypeQualifier
  , TQS_TypeSpecifier <$> reparseTypeSpecifier macroTys
  , TQS_AlignmentSpecifier <$> reparseAlignmentSpecifier macroTys
  ]


reparseTypeQualifier :: Reparse TypeQualifier
reparseTypeQualifier = choice
  [ keyword "const"    $> TQ_const
  , keyword "restrict" $> TQ_restrict
  , keyword "volatile" $> TQ_volatile
  , keyword "_Atomic"  $> TQ__Atomic
  ]
  <?> "type qualifier"

reparseTypeSpecifier :: TcMacro.TypeEnv -> Reparse TypeSpecifier
reparseTypeSpecifier macroTys =
  choice
  [ TypeSpecifier <$> reparsePrimType
  , try $
    do { nm <- reparseName
       ; case Map.lookup nm macroTys of
           Nothing -> fail $ "out of scope type specifier macro name " ++ show nm
           Just ty
              | TcMacro.Quant bf <- ty
              , TcMacro.isPrimTy bf
              -> return $ TypeDefTypeSpecifier nm
              | otherwise
              -> fail $ "macro name does not refer to a type: " ++ show nm
       }
  ] <?> "type"


data StorageClassSpecifier
  = SC_auto
  | SC_constexpr
  | SC_extern
  | SC_register
  | SC_static
  | SC_thread_local
  | SC_typedef
  deriving stock ( Eq, Show )

reparseStorageClassSpecifier :: Reparse StorageClassSpecifier
reparseStorageClassSpecifier =
  choice
    [ keyword "auto"         $> SC_auto
    , keyword "constexpr"    $> SC_constexpr
    , keyword "extern"       $> SC_extern
    , keyword "register"     $> SC_register
    , keyword "static"       $> SC_static
    , keyword "thread_local" $> SC_thread_local
    , keyword "typedef"      $> SC_typedef
    ]
  <?> "storage class"

data FunctionSpecifier
  = FS_inline
  | FS__Noreturn
  deriving stock ( Eq, Show )

reparseFunctionSpecifier :: Reparse FunctionSpecifier
reparseFunctionSpecifier = choice
  [ keyword "inline"    $> FS_inline
  , keyword "_Noreturn" $> FS__Noreturn
  ]



-- TODO: currently re-using the macro parser for expressions.
--
-- Currently we only need to parse expressions for:
--
--  - sizes of arrays,
--  - bit-field sizes,
--  - alignment specifiers.
newtype SizeExpression = SizeExpression MExpr
  deriving newtype ( Eq, Show )

reparseSizeExpression :: Reparse SizeExpression
reparseSizeExpression = SizeExpression <$> mExpr
