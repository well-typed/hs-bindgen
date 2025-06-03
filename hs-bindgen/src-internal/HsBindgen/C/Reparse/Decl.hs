{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsBindgen.C.Reparse.Decl
  ( -- * Reparsing API
    reparseFieldDecl, reparseFunDecl, reparseTypedef

    -- * Types
  , TypeName(..)
  , reparseTypeName
  , typeNameType
  , TypeSpecifier(..)
  , TypeSpecifierQualifier(..)
  , AlignmentSpecifier(..)
  , StorageClassSpecifier(..)
  , FunctionSpecifier(..)
  , TypeQualifier(..)
  , AttributeSpecifier(..)
  , reparseAttributeSpecifier
  , Attribute(..), AttributeToken(..), BalancedToken(..)
  , DeclName(..)
  , DeclaratorType(..)
  , Declarator(..), DirectDeclarator(..)
  , DeclarationSpecifier(..)
  , ArrayDeclarator(..), FunctionDeclarator(..)
  , Pointers(..)
  , StructOrUnionSpecifier(..), StructOrUnion(..)
  , EnumSpecifier(..)
  , SizeExpression(..), litSizeExpression
  , ArraySize(..)
  , Parameter(..)
  , ParameterDeclarator(..)
  )
  where

-- base
import Control.Monad
  ( void )
import Data.Either
  ( partitionEithers )
import Data.Functor
  ( ($>) )
import Data.Kind qualified as Hs

import Data.List
  ( intercalate )
import Data.List.NonEmpty qualified as NE
import Data.Maybe
  ( isJust, mapMaybe )
import Numeric.Natural
  ( Natural )
import GHC.Generics
  ( Generic )

-- c-expr
import C.Type qualified

-- containers
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- parsec
import Text.Parsec

-- text
import Data.Text
  ( Text )
import Data.Text qualified as Text
  ( pack, unpack )

-- hs-bindgen
import Clang.HighLevel.Types
import Clang.LowLevel.Core ( CXTokenKind(..) )
import HsBindgen.C.Reparse.Common (reparseName, manyTillLookahead)
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Reparse.Macro ( mExpr )
import HsBindgen.C.Reparse.Type
import HsBindgen.C.Tc.Macro qualified as Macro
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.Errors
import HsBindgen.Frontend.Macros.AST.C qualified as C
import HsBindgen.Frontend.Macros.AST.Syntax
import HsBindgen.Imports (fromMaybe)
import HsBindgen.Language.C (CName(..))
import HsBindgen.Language.C qualified as C

-- TODO: re-using the macro expression parser for simplicity
-- import HsBindgen.C.AST.Macro

--------------------------------------------------------------------------------

-- | Reparse a C field declaration (in a struct)
reparseFieldDecl :: Macro.TypeEnv -> Reparse (C.Type, CName)
reparseFieldDecl tyEnv = do
  ( specs, decl ) <- reparseDeclaration @Concrete tyEnv
  _mbBitSize <- optionMaybe $
    ( do { punctuation ":" ; reparseSizeExpression tyEnv } <?> "bit size" )
  -- TODO: discarding bit size
  eof
  case declarationTypeAndName specs decl of
    Left err  -> unexpected err
    Right ( ty, DeclName nm ) -> return ( ty, nm )

-- | Reparse a C function declaration.
reparseFunDecl :: Macro.TypeEnv -> Reparse (([C.Type],C.Type), CName)
reparseFunDecl tyEnv = do
  ( specs, decl ) <- reparseDeclaration @Concrete tyEnv
  eof
  case declarationTypeAndName specs decl of
    Left err -> unexpected err
    Right ( ty, DeclName nm )
      | C.TypeFun args res <- ty
      -> return ( (args, res), nm )
      | otherwise
      -> unexpected $ "expected a function type, but got: " ++ show ty

-- | Reparse a C @typedef@ declaration.
reparseTypedef :: Macro.TypeEnv -> Reparse C.Type
reparseTypedef macroTys = do
  ( specs, decl ) <- reparseDeclaration @Concrete macroTys
  -- We expect a 'typedef' storage class specifier to be provided,
  -- but I don't see any point in enforcing that.
  eof
  case declarationTypeAndName specs decl of
    Left err  -> unexpected err
    Right ( ty, _) -> return ty

--------------------------------------------------------------------------------

-- | Whether a declarator is concrete (= named) or abstract (= anonymous).
data DeclaratorType
  = Concrete -- ^ Concrete (= named) declarator
  | Abstract -- ^ Abstract (= anonymous) declarator
  deriving stock ( Eq, Show )
-- | Singleton for 'DeclaratorType'.
type SDeclaratorType :: DeclaratorType -> Hs.Type
data SDeclaratorType abs where
  SConcrete :: SDeclaratorType Concrete
  SAbstract :: SDeclaratorType Abstract
deriving stock instance Eq   ( SDeclaratorType abs )
deriving stock instance Show ( SDeclaratorType abs )

type DeclName :: DeclaratorType -> Hs.Type
data family DeclName abs
data    instance DeclName Abstract = AbstractName
  deriving stock ( Eq, Show, Generic )
newtype instance DeclName Concrete = DeclName CName
  deriving newtype ( Eq, Show, Generic )

-- | Singleton instances for 'DeclaratorType'.
class ( Show ( Declarator abs )        , Eq ( Declarator abs )
      , Show ( DirectDeclarator abs )  , Eq ( DirectDeclarator abs )
      , Show ( FunctionDeclarator abs ), Eq ( FunctionDeclarator abs )
      , Show ( ArrayDeclarator abs )   , Eq ( ArrayDeclarator abs )
      , Show ( SDeclaratorType abs )   , Eq ( SDeclaratorType abs )
      , Show ( DeclName abs )          , Eq ( DeclName abs )
      )
    => KnownDeclaratorType abs where
  knownDeclarator :: SDeclaratorType abs
instance KnownDeclaratorType Concrete where
  knownDeclarator = SConcrete
instance KnownDeclaratorType Abstract where
  knownDeclarator = SAbstract

--------------------------------------------------------------------------------
-- Declarations and declarators

-- | A @C@ declarator (concrete or abstract).
type Declarator :: DeclaratorType -> Hs.Type
data Declarator abs
  = Declarator
      { declaratorPointer :: Pointers
      , directDeclarator  :: DirectDeclarator abs
      }
  deriving stock Generic
deriving stock instance KnownDeclaratorType abs => Eq   ( Declarator abs )
deriving stock instance KnownDeclaratorType abs => Show ( Declarator abs )


-- | Pointer annotations: a sequence of @*@, with each @*@ having its own
-- attributes and type qualifiers.
newtype Pointers = Pointers [ ( [ AttributeSpecifier ], [ TypeQualifier ] ) ]
  deriving stock ( Eq, Show, Generic )

-- | A @C@ direct declarator.
type DirectDeclarator :: DeclaratorType -> Hs.Type
data DirectDeclarator abs
  = IdentifierDeclarator ( DeclName abs ) [ AttributeSpecifier ]
  | ParenDeclarator ( Declarator abs )
  | ArrayDirectDeclarator ( ArrayDeclarator abs )
  | FunctionDirectDeclarator ( FunctionDeclarator abs )
  deriving stock Generic
deriving stock instance KnownDeclaratorType abs => Eq   ( DirectDeclarator abs )
deriving stock instance KnownDeclaratorType abs => Show ( DirectDeclarator abs )

-- | A @C@ array direct declarator.
type ArrayDeclarator :: DeclaratorType -> Hs.Type
data ArrayDeclarator abs
  = ArrayDeclarator
    { arrayDirectDeclarator :: DirectDeclarator abs
    , arrayStatic           :: Bool
    , arrayTypeQualifiers   :: [TypeSpecifierQualifier]
    , arraySize             :: ArraySize
    , arrayAttributes       :: [AttributeSpecifier]
    }
  deriving stock Generic
deriving stock instance KnownDeclaratorType abs => Eq   ( ArrayDeclarator abs )
deriving stock instance KnownDeclaratorType abs => Show ( ArrayDeclarator abs )

data ArraySize
  = ArraySize SizeExpression
  | ArrayNoSize
  | VLA
  deriving stock ( Eq, Show, Generic )

-- | A @C@ function direct declarator.
type FunctionDeclarator :: DeclaratorType -> Hs.Type
data FunctionDeclarator abs
  = FunctionDeclarator
  { functionDirectDeclarator :: DirectDeclarator abs
  , functionParameters       :: [Parameter]
  , functionVariadic         :: Bool
      -- ^ whether there is a final @...@
  , functionAttributes       :: [AttributeSpecifier]
  }
  deriving stock Generic
deriving stock instance KnownDeclaratorType abs => Eq   ( FunctionDeclarator abs )
deriving stock instance KnownDeclaratorType abs => Show ( FunctionDeclarator abs )

-- | A @C@ function parameter
data Parameter =
  Parameter
    { parameterAttributes     :: [AttributeSpecifier]
    , parameterDeclSpecifiers :: (NE.NonEmpty (DeclarationSpecifier, [AttributeSpecifier]))
    , parameterDeclarator     :: ParameterDeclarator
    }
  deriving stock ( Eq, Show, Generic )

-- | A @C@ function parameter declarator.
data ParameterDeclarator
  = ParameterDeclarator         ( Declarator Concrete )
  | ParameterAbstractDeclarator ( Declarator Abstract )
  deriving stock ( Eq, Show, Generic )

--------------------------------------------------------------------------------
-- Specifiers, qualifiers, attributes...

newtype AttributeSpecifier = AttributeSpecifier [ Attribute ]
  deriving stock ( Eq, Show, Generic )
data Attribute
  = Attribute
  { attributeToken :: AttributeToken
  , attributeArgumentClause :: Maybe [BalancedToken]
  }
  deriving stock ( Eq, Show, Generic )
data AttributeToken
  = StandardAttribute CName
  | AttributePrefixedToken CName CName
  deriving stock ( Eq, Show, Generic )
data BalancedToken
  = BalancedTokenNormal Text
  | BalancedToken [BalancedToken]
  deriving stock ( Eq, Show, Generic )

-- | A type specified as a C abstract declarator.
-- What the C23 standard calls @type-name@.
--
-- Examples:
--
--  - primitive types, e.g. @int@,
--  - pointers, e.g. @char *@,
--  - arrays, e.g. @int[5]@,
--  - functions, e.g. @float *(int)@,
--  - function pointers, e.g. @int (*)(double)@.
data TypeName
  = TypeName TypeSpecifier [AttributeSpecifier] (Declarator Abstract)
  deriving stock ( Eq, Show, Generic )
data DeclarationSpecifier
  = DeclStorageSpecifier        StorageClassSpecifier
  | DeclTypeSpecifierQualifier  TypeSpecifierQualifier
  | DeclFunctionSpecifier       FunctionSpecifier
  deriving stock ( Eq, Show, Generic )
data TypeQualifier
  = TQ_const
  | TQ_restrict
  | TQ_volatile
  | TQ__Atomic
  deriving stock ( Eq, Show, Generic )
data TypeSpecifier
  = TypeSpecifier C.PrimType
  | TypeDefTypeSpecifier CName
  | StructOrUnionTypeSpecifier StructOrUnionSpecifier
  | EnumTypeSpecifier EnumSpecifier
  deriving stock ( Eq, Show, Generic )
data StructOrUnionSpecifier
  = StructOrUnionSpecifier
    { structOrUnion :: StructOrUnion
    , structOrUnionAttributeSpecifiers :: [AttributeSpecifier]
    , structOrUnionIdentifier :: CName
    }
  deriving stock ( Eq, Show, Generic )
data StructOrUnion
  = IsStruct | IsUnion
  deriving stock ( Eq, Show, Generic )
data EnumSpecifier
  = EnumSpecifier
    { enumSpecifierIdentifier :: CName
    , enumAttributeSpecifiers :: [AttributeSpecifier]
    , enumSpecifierQualifiers :: [(TypeSpecifierQualifier, [AttributeSpecifier])]
    }
  deriving stock ( Eq, Show, Generic )

data AlignmentSpecifier
  = AlignAsTypeName TypeName
  | AlignAsConstExpr SizeExpression
  deriving stock ( Eq, Show, Generic )
data TypeSpecifierQualifier
  = TSQ_TypeQualifier TypeQualifier
  | TSQ_TypeSpecifier TypeSpecifier
  | TSQ_AlignmentSpecifier AlignmentSpecifier
  deriving stock ( Eq, Show, Generic )
data StorageClassSpecifier
  = SC_auto
  | SC_constexpr
  | SC_extern
  | SC_register
  | SC_static
  | SC_thread_local
  | SC_typedef
  deriving stock ( Eq, Show, Generic )
data FunctionSpecifier
  = FS_inline
  | FS__Noreturn
  deriving stock ( Eq, Show, Generic )

--------------------------------------------------------------------------------
-- Sizes

-- | A size expression, e.g. a @C@ integer constant expression for a type-level
-- size (e.g. a size of an array).
data SizeExpression = SizeExpression
  { sizeExpressionMExpr :: MExpr Ps
  , sizeExpressionValue :: Maybe Natural
  } deriving stock ( Eq, Ord, Show, Generic )

litSizeExpression :: Natural -> SizeExpression
litSizeExpression n
  = SizeExpression
    { sizeExpressionMExpr =
        MTerm $ MInt $
          C.IntegerLiteral ( Text.pack $ show n ) C.Type.Size ( fromIntegral n )
    , sizeExpressionValue = Just n
    }

-- TODO: we currently re-use the macro parser for expressions.
--
-- Currently we only need to parse expressions for:
--
--  - sizes of arrays,
--  - bitfield sizes,
--  - alignment specifiers.
reparseSizeExpression :: Macro.TypeEnv -> Reparse SizeExpression
reparseSizeExpression tyEnv = do
  e <- mExpr tyEnv
  let val = Macro.evaluateMExpr tyEnv e
  case val of
    Macro.NoValue ->
      unexpected "non-constant size expression"
    Macro.Value ty v ->
      return $ SizeExpression e ( Macro.naturalMaybe ty v )

--------------------------------------------------------------------------------

-- | Helper function for reparsing a declaration, i.e. declaration specifiers
-- followed by a declarator.
reparseDeclaration
  :: forall abs
  .  KnownDeclaratorType abs
  => Macro.TypeEnv
  -> Reparse ([DeclarationSpecifier], Declarator abs)
reparseDeclaration macroTys =
  manyTillLookahead
    ( reparseDeclarationSpecifier macroTys )
    ( reparseDeclarator @abs macroTys )

declaratorType :: C.Type -> Declarator abs -> Either String ( C.Type, DeclName abs )
declaratorType baseTy (Declarator ptrs decl) =
  directDeclaratorTypeAndName (mkPtr ptrs baseTy) decl

declarationTypeAndName
  :: [ DeclarationSpecifier ]
  -> Declarator abs
  -> Either String ( C.Type, DeclName abs )
declarationTypeAndName specs decl =
  case declarationSpecifiersType specs of
    Nothing -> Left "declarator missing a type"
    Just baseTy -> declaratorType baseTy decl

mkPtr :: Pointers -> C.Type -> C.Type
mkPtr (Pointers l) = go l
  where
    go [] a = a
    go (_:r) a = go r (C.TypePointer a)

declarationSpecifiersType :: [DeclarationSpecifier] -> Maybe C.Type
declarationSpecifiersType specs =
  -- TODO: we are discarding
  --  - function specifiers (inline/noreturn)
  --  - storage specifiers (constexpr/static/...)
  --  - type qualifiers (const/volatile/atomic/...)
  --  - alignment specifiers
  case mapMaybe isTy specs of
    [ ty ] -> Just ty
    [] -> Nothing
    tys@(_:_:_) ->
      panicPure $ unlines
        [ "declarationSpecifiersType: multiple types"
        , show tys
        ]

  where
    isTy :: DeclarationSpecifier -> Maybe C.Type
    isTy = \case
      DeclFunctionSpecifier {} -> Nothing
      DeclStorageSpecifier  {} -> Nothing
      DeclTypeSpecifierQualifier  tsq ->
        case tsq of
          TSQ_TypeQualifier {} -> Nothing
          TSQ_AlignmentSpecifier {} -> Nothing
          TSQ_TypeSpecifier ts ->
            Just $ typeSpecifierType ts

typeNameType :: TypeName -> Either String C.Type
typeNameType (TypeName tySpec _attrs decl) =
  -- TODO: discarding attributes
    fst <$> declaratorType ty decl
  where
    ty = typeSpecifierType tySpec

typeSpecifierType :: TypeSpecifier -> C.Type
typeSpecifierType = \case
  TypeSpecifier ty -> C.TypePrim ty
  TypeDefTypeSpecifier nm -> C.TypeTypedef nm
  StructOrUnionTypeSpecifier
    StructOrUnionSpecifier
      { structOrUnion = su, structOrUnionIdentifier = i }
     -> case su of
          IsStruct -> C.TypeStruct i
          IsUnion  -> C.TypeUnion  i
  EnumTypeSpecifier
    EnumSpecifier { enumSpecifierIdentifier = i } ->
      C.TypeEnum i

directDeclaratorTypeAndName
  :: C.Type -> DirectDeclarator abs
  -> Either String ( C.Type, DeclName abs )
directDeclaratorTypeAndName ty = \case
  IdentifierDeclarator nm _attrs ->
    -- TODO: discarding attributes
    Right (ty, nm)
  ParenDeclarator d ->
    declaratorType ty d
  ArrayDirectDeclarator (ArrayDeclarator { arrayDirectDeclarator = d, arraySize = sz }) -> do
    let
      arrTy =
        case sz of
          ArrayNoSize ->
            C.TypeIncompleteArray ty
          VLA ->
             C.TypeIncompleteArray ty
          ArraySize ( SizeExpression _ mbKnownSize )
            | Just n <- mbKnownSize
            -> C.TypeConstArray n ty
            | otherwise
            -> C.TypeIncompleteArray ty -- TODO: this discards information
    directDeclaratorTypeAndName arrTy d
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
            Right $ C.TypeFun argTys ty
        ms@(_:rest) -> do
          let s = if null rest then "" else "s"
          Left $
            "missing type specifier" ++ s ++ " in function parameter" ++ s
              ++ " " ++ intercalate ", " ( map show ms )
    directDeclaratorTypeAndName funTy d

parameterTypes :: [Parameter] -> ([Int], [C.Type])
parameterTypes ps =
  partitionEithers $
    zipWith aux [1..] ps
  where
    aux i ( Parameter { parameterDeclSpecifiers = specs, parameterDeclarator = decl }) =
      -- TODO: dropping attributes
      let declSpecs = fmap fst $ NE.toList specs
      in
        case parameterDeclaratorType declSpecs decl of
            Left {} -> Left i
            Right ty -> Right ty

parameterDeclaratorType :: [DeclarationSpecifier] -> ParameterDeclarator -> Either String C.Type
parameterDeclaratorType specs = \case
  ParameterDeclarator decl         -> fmap fst $ declarationTypeAndName specs decl
  ParameterAbstractDeclarator decl -> fmap fst $ declarationTypeAndName specs decl

reparseDeclarator :: forall abs. KnownDeclaratorType abs => Macro.TypeEnv -> Reparse (Declarator abs)
reparseDeclarator macroTys =
  Declarator <$> reparsePointers <*> reparseDirectDeclarator @abs macroTys

reparsePointers :: Reparse Pointers
reparsePointers = Pointers <$> many pointer <?> "pointer"
  where
    pointer = do
      punctuation "*"
      attrs <- many reparseAttributeSpecifier
      tqs   <- many reparseTypeQualifier
      return ( attrs, tqs )

reparseDirectDeclarator
  :: forall abs. KnownDeclaratorType abs
  => Macro.TypeEnv -> Reparse ( DirectDeclarator abs )
reparseDirectDeclarator macroTys = do
  decl <-
    choice $
      ( ParenDeclarator <$> try ( parens ( reparseDeclarator @abs macroTys ) ) )
          -- try: because parentheses could be a parenthesised declarator
          -- or it could be a function declarator
      : case knownDeclarator @abs of
          SConcrete -> [ IdentifierDeclarator <$> fmap DeclName reparseIdentifier <*> many reparseAttributeSpecifier ]
          SAbstract -> [ IdentifierDeclarator AbstractName <$> many reparseAttributeSpecifier ]
  withArrayOrFunctionSuffixes macroTys decl

withArrayOrFunctionSuffixes
  :: forall abs. KnownDeclaratorType abs
  => Macro.TypeEnv -> DirectDeclarator abs -> Reparse ( DirectDeclarator abs )
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

reparseIdentifier :: Reparse CName
reparseIdentifier =
  tokenOfKind CXToken_Identifier (Just . CName)
    <?> "identifier"

reparseArrayDeclarator
  :: Macro.TypeEnv
  -> DirectDeclarator abs
  -> Reparse ( ArrayDeclarator abs )
reparseArrayDeclarator macroTys decl = do
  punctuation "["
  ( static, tq, sz ) <-
    choice
      [ do { s1 <- isJust <$> optionMaybe (keyword "static")
           ; tq <- many $ reparseTypeQualifierSpecifier macroTys
           ; s2 <- isJust <$> optionMaybe (keyword "static")
           ; size <- optionMaybe $ reparseSizeExpression macroTys
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

reparseFunctionDeclarator
  :: Macro.TypeEnv
  -> DirectDeclarator abs
  -> Reparse ( FunctionDeclarator abs )
reparseFunctionDeclarator macroTys decl = do
  ( params, variadic ) <-
    ( parens $ reparseParameterList macroTys )
      <?> "function parameter list"
  attrs <- many reparseAttributeSpecifier
  return $
    FunctionDeclarator
      { functionDirectDeclarator = decl
      , functionParameters       = params
      , functionVariadic         = variadic
      , functionAttributes       = attrs
      }
  <?> "function declarator"

reparseParameterList :: Macro.TypeEnv -> Reparse ( [ Parameter ], Bool )
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

reparseParameter :: Macro.TypeEnv -> Reparse Parameter
reparseParameter macroTys = do
  attrs <- many reparseAttributeSpecifier
  declSpecs <- reparseDeclarationSpecifiers macroTys
  decl <- reparseParameterDeclarator macroTys
  return $ Parameter attrs declSpecs decl
  <?> "function parameter"

reparseParameterDeclarator :: Macro.TypeEnv -> Reparse ParameterDeclarator
reparseParameterDeclarator macroTys =
  choice
    [ ParameterDeclarator         <$> try (reparseDeclarator @Concrete macroTys)
    , ParameterAbstractDeclarator <$> reparseDeclarator @Abstract macroTys
    ]
  <?> "function parameter declarator"

reparseDeclarationSpecifiers
  :: Macro.TypeEnv
  -> Reparse (NE.NonEmpty (DeclarationSpecifier, [AttributeSpecifier]))
reparseDeclarationSpecifiers macroTys =
  toNE <$> (many1 $ do
    spec <- reparseDeclarationSpecifier macroTys
    attrs <- many reparseAttributeSpecifier
    return (spec, attrs))
  where
    toNE [] = panicPure "many1: empty list"
    toNE (a:as) = a NE.:| as

reparseAttributeSpecifier :: Reparse AttributeSpecifier
reparseAttributeSpecifier = do
  -- Allow both [[attr]] and __attribute((attr)) (GNU extension)
  gnuStyle <-
    choice
      [ False <$ try ( punctuation "[" >> punctuation "[" ) <?> "[["
      , do { exact CXToken_Keyword "__attribute__" <?> "__attribute__"
           ; ( punctuation "(" >> punctuation "(" ) <?> "(("
           ; return True
           }
      ]
  attrs <- reparseAttribute `sepBy` punctuation ","
  if gnuStyle
  then ( punctuation ")" >> punctuation ")" ) <?> "))"
  else ( punctuation "]" >> punctuation "]" ) <?> "]]"
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
  <?> "attribute token"

reparseBalancedToken :: Reparse BalancedToken
reparseBalancedToken =
  choice
    [ reparseParenBalancedToken
    , BalancedTokenNormal . getTokenSpelling . tokenSpelling <$> anyToken
    ]
  <?> "balanced token"

reparseParenBalancedToken :: Reparse BalancedToken
reparseParenBalancedToken = do
  closer <- choice [ punctuation "(" $> ")"
                   , punctuation "[" $> "]"
                   , punctuation "{" $> "}"
                   ]
  inner <- many reparseBalancedToken
  punctuation closer
  return $ BalancedToken inner

reparseDeclarationSpecifier :: Macro.TypeEnv -> Reparse DeclarationSpecifier
reparseDeclarationSpecifier macroTys = do
  choice
    [ DeclStorageSpecifier       <$> reparseStorageClassSpecifier
    , DeclTypeSpecifierQualifier <$> reparseTypeQualifierSpecifier macroTys
    , DeclFunctionSpecifier      <$> reparseFunctionSpecifier
    ]

reparseAlignmentSpecifier :: Macro.TypeEnv -> Reparse AlignmentSpecifier
reparseAlignmentSpecifier macroTys = do
  void $ keyword "alignas"
  parens $
    choice
      [ AlignAsTypeName  <$> reparseTypeName macroTys
      , AlignAsConstExpr <$> reparseSizeExpression macroTys
      ]
  <?> "alignment"

reparseTypeName :: Macro.TypeEnv -> Reparse TypeName
reparseTypeName macroTys = do
  ty      <- reparseTypeSpecifier macroTys
  attrs   <- many reparseAttributeSpecifier
  mbDecl  <- reparseDeclarator @Abstract macroTys
  return $
    TypeName ty attrs mbDecl

reparseTypeQualifierSpecifier :: Macro.TypeEnv -> Reparse TypeSpecifierQualifier
reparseTypeQualifierSpecifier macroTys = choice
  [ TSQ_TypeQualifier <$> reparseTypeQualifier
  , TSQ_TypeSpecifier <$> reparseTypeSpecifier macroTys
  , TSQ_AlignmentSpecifier <$> reparseAlignmentSpecifier macroTys
  ]

reparseTypeQualifier :: Reparse TypeQualifier
reparseTypeQualifier = choice
  [ keyword "const"    $> TQ_const
  , keyword "restrict" $> TQ_restrict
  , keyword "volatile" $> TQ_volatile
  , keyword "_Atomic"  $> TQ__Atomic
  ]
  <?> "type qualifier"

-- | See the C23 specification, 6.7.3.1 @type-specifier@.
reparseTypeSpecifier :: Macro.TypeEnv -> Reparse TypeSpecifier
reparseTypeSpecifier macroTypeEnv =
  choice
  -- Primitive type (such as void, int, float)
  [ TypeSpecifier <$> reparsePrimType
  -- struct-or-union-specifier
  , StructOrUnionTypeSpecifier <$> reparseStructOrUnionSpecifier
  -- enum-specifier
  , EnumTypeSpecifier <$> reparseEnumSpecifier macroTypeEnv
  -- _BitInt, atomic-type-specifier and typeof-specifier (unsupported for now)
  , do { kw <- try $ choice [ keyword "_BitInt", keyword "_Atomic", keyword "typeof", keyword "typeof_unqual" ]
       ; _ <- parens $ anythingMatchingBrackets [("(", ")")]
       ; unexpected $ "unsupported '" ++ Text.unpack kw ++ "' type specifier"
       }
  -- typedef-name
  , try $
    do { nm <- reparseName
       ; if Set.member nm (Macro.typeEnvTypedefs macroTypeEnv) then
           return $ TypeDefTypeSpecifier nm
         else
           case Map.lookup nm (Macro.typeEnvMacros macroTypeEnv) of
             Nothing -> unexpected $ "out of scope type specifier macro name " ++ show nm
             Just valAndTy
                | Macro.Quant bf <- fmap snd valAndTy
                , Macro.isPrimTy bf
                -> return $ TypeDefTypeSpecifier nm
                | otherwise
                -> unexpected $ "macro name does not refer to a type: " ++ show nm
       }
  ] <?> "type"

reparseStructOrUnion :: Reparse StructOrUnion
reparseStructOrUnion = choice [ keyword "struct" $> IsStruct, keyword "union" $> IsUnion ]

reparseStructOrUnionSpecifier :: Reparse StructOrUnionSpecifier
reparseStructOrUnionSpecifier = do
  structOrUnion <- reparseStructOrUnion
  let what = case structOrUnion of { IsStruct -> "struct"; IsUnion -> "union" }
  attrs <- many reparseAttributeSpecifier
  mbIdent <- optionMaybe reparseIdentifier
  mbMembers <- optionMaybe $ braces $ anythingMatchingBrackets [("{", "}")]
  case (mbIdent, mbMembers) of
    (Just i, Nothing) ->
      return $
        StructOrUnionSpecifier
          { structOrUnion
          , structOrUnionAttributeSpecifiers = attrs
          , structOrUnionIdentifier = i }
    (_, Just {}) ->
      unexpected $ "unsupported member declaration list in " ++ what ++ " specifier"
    (Nothing, Nothing) ->
      unexpected $ "invalid " ++ what ++ " specifier: missing identifier or member declaration list"

reparseEnumSpecifier :: Macro.TypeEnv -> Reparse EnumSpecifier
reparseEnumSpecifier macroTys = do
  _ <- keyword "enum"
  attrs <- many reparseAttributeSpecifier
  mbIdent <- optionMaybe reparseIdentifier
  enumTypeSpecifiers <- optionMaybe $
    do { void $ punctuation ":"
       ; many1 $
           (,) <$> reparseTypeQualifierSpecifier macroTys
               <*> many reparseAttributeSpecifier
       }
  mbEnumerators <- optionMaybe $ braces $ anythingMatchingBrackets [("{", "}")]
  case (mbIdent, mbEnumerators) of
    (Just i, Nothing) ->
      return $
        EnumSpecifier
          { enumSpecifierIdentifier = i
          , enumAttributeSpecifiers = attrs
          , enumSpecifierQualifiers = fromMaybe [] enumTypeSpecifiers
          }
    (_, Just {}) ->
      unexpected "unsupported enumerator list in enum specifier"
    (Nothing, Nothing) ->
      unexpected "invalid enum specifier: missing identifier or enumerator list"
  <?> "enum"

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

reparseFunctionSpecifier :: Reparse FunctionSpecifier
reparseFunctionSpecifier = choice
  [ keyword "inline"    $> FS_inline
  , keyword "_Noreturn" $> FS__Noreturn
  ]
  <?> "function specifier"

