module C.Expr.Parse.Expr (parseMacro, parseMacroType) where

import Control.Monad
import Data.Functor.Identity
import Data.Text (Text)
import Data.Type.Nat
import Data.Vec.Lazy (Vec (..))
import Data.Vec.Lazy qualified as Vec
import DeBruijn (Idx (..))
import Text.Parsec hiding (parseTest, token)
import Text.Parsec.Expr

import C.Expr.Parse.Infra
import C.Expr.Parse.Literal
import C.Expr.Parse.Name
import C.Expr.Syntax

import Clang.CStandard
import Clang.Enum.Simple
import Clang.HighLevel.Types
import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  Top-level

  Some useful references:

  - Section 6.10 "Preprocessing directives" of the C standard
    <https://fog.misty.com/perry/osp/standard/preproc.pdf>
  - Section 3 "Macros" of the @cpp@ documentation
    <https://gcc.gnu.org/onlinedocs/cpp/Macros.html>
  - "C operator precedence"
    <https://en.cppreference.com/w/c/language/operator_precedence>
-------------------------------------------------------------------------------}

-- | Parse a macro definition (type or value expression)
--
-- Tries to parse the body as a type expression first. A valid type token
-- sequence always produces @'Term' ('Type' …)@; everything else parses as an
-- expression. Only when typechecking macros, we can fully discriminate type and
-- value expressions.
parseMacro :: ClangCStandard -> Parser Macro
parseMacro cStd = do
    (macroLoc, macroName) <- parseLocName
    let functionLike :: Parser Macro
        functionLike = do
          paramNames <- formalParams
          withParamNames paramNames $ \macroParams -> do
            macroExpr <- bodyExpr macroParams
            pure $ Macro macroLoc macroName macroParams macroExpr

        objectLike :: Parser Macro
        objectLike = do
          macroExpr <- bodyExpr VNil
          pure $ Macro macroLoc macroName VNil macroExpr
    m <- choice [try functionLike, objectLike]
    eof
    return m
  where
    -- Try the body as a type expression first. The @'eof'@ inside the @'try'@
    -- is essential: if @'parseMacroType'@ succeeds on a prefix (e.g. the bare
    -- identifier in @size_t + 1@) but leaves tokens unconsumed, the whole
    -- attempt is abandoned and we fall back to the expression parser.
    bodyExpr :: Vec ctx Name -> Parser (Expr ctx Ps)
    bodyExpr macroParams = try (parseMacroType cStd macroParams <* eof) <|> exprTuple cStd macroParams

withParamNames ::
     [Name]
  -> (forall ctx. Vec ctx Name -> Parser Macro)
  -> Parser Macro
withParamNames paramNames mkParser = go VNil paramNames
  where
    go :: forall ctx. Vec ctx Name -> [Name] -> Parser Macro
    -- We store the local parameters in reverse order (the left most local
    -- parameter is the last element of the vector). This ensures "DeBruijn
    -- indices" are distributed as usual (i.e., the right-most or inner-most
    -- local parameter has index 0).
    go vec []     = mkParser vec
    go vec (n:ns) = go (n ::: vec) ns

formalParams :: Parser [Name]
formalParams = parens $ formalParam `sepBy` comma

formalParam :: Parser Name
formalParam = parseName

lookupParam :: Name -> Vec ctx Name -> Maybe (Idx ctx)
lookupParam _ VNil    = Nothing
lookupParam n (m ::: vec)
  | n == m    = Just IZ
  | otherwise = IS <$> lookupParam n vec

{-------------------------------------------------------------------------------
  Types

-------------------------------------------------------------------------------}

-- | Parse a macro body as a C type expression
--
-- Recognizes the following grammar (informally):
--
-- @
--   type           ::= const? type_base const? pointer_layers?
--
--   type_base      ::= sign_specifier? int_size_keyword? 'int'?
--                    | sign_specifier? 'char'
--                    | 'float' | 'double'
--                    | 'void'
--                    | '_Bool' | 'bool'
--                    | ('struct' | 'union' | 'enum') identifier
--                    | identifier
--
--   pointer_layers ::= ('*' const?)+
-- @
--
-- Returns an 'Expr Ps' where:
--
-- * A keyword or tagged base type becomes @'Term' ('Type' literal)@.
-- * A bare identifier that is a local macro parameter becomes @'Term' ('LocalParam' …)@.
-- * A bare identifier that is a free variable becomes @'Term' ('Var' …)@;
--   the typechecker decides whether it names a type or a value.
-- * Each @const@ qualifier wraps the expression in @'TyApp' 'Const'@.
-- * Each @*@ pointer layer wraps the expression in @'TyApp' 'Pointer'@.
parseMacroType :: ClangCStandard -> Vec ctx Name -> Parser (Expr ctx Ps)
parseMacroType cStd macroParams = do
    constBefore <- option False (True <$ keyword "const")
    base        <- typeBase cStd macroParams
    constAfter  <- option False (True <$ keyword "const")
    ptrs        <- pointerLayers
    -- In C, @const@ is idempotent: @const int const@ is valid but equivalent
    -- to @const int@. We therefore wrap with at most one 'Const' layer,
    -- regardless of whether the qualifier appeared before or after the base.
    let withConst
          | constBefore || constAfter = TyApp Const (base ::: VNil)
          | otherwise                 = base
    return (foldl apPtr withConst ptrs)
  where
    apPtr acc ptrConst =
      let withPtr = TyApp Pointer (acc ::: VNil)
      in if ptrConst then TyApp Const (withPtr ::: VNil) else withPtr

-- | Base of a type expression (without const/pointer layers)
--
-- Returns:
--
-- * @'Term' ('Literal' …)@ for keyword or elaborated base types.
-- * @'Term' ('LocalParam' …)@ for a bare identifier that is a local macro parameter.
-- * @'Term' ('Var' …)@ for any other bare identifier; the typechecker decides
--   whether it names a type or a value.
typeBase :: forall ctx. ClangCStandard -> Vec ctx Name -> Parser (Expr ctx Ps)
typeBase cStd macroParams =
    choice [
        -- Type literal.
        Term . Literal . TypeLit <$> typeLiteral cStd
        -- Tagged type (e.g., @struct Foo@)
      , Term . Literal . uncurry TypeTagged <$> taggedTypeLit
        -- The bare identifier (typedef name, type macro, or expression
        -- variable) is needed to parse pointer-qualified typedef references
        -- such as @size_t *@: without it, @parseMacroType@ would reject the
        -- identifier base and the expression parser would then fail on @*@ (a
        -- binary operator without a right-hand side). Attempting to detangle
        -- the two by restricting @parseMacroType@ to keyword\/tagged bases only
        -- does not help (both paths produce the same @'Var'@ node) while
        -- adding backtracking overhead.
      , (Term . mkVar) <$> parseName
      ]
  where
    mkVar :: Name -> Term ctx Ps
    mkVar n = case lookupParam n macroParams of
      Just i  -> LocalParam i
      Nothing -> Var NoXVar n []

-- | Parse a sequence of type-literal keywords and combine them
--
-- C type literal keywords can appear in various orders:
--
-- @
--   unsigned long int
--   long unsigned int
--   int long unsigned
-- @
--
-- are all the same type.
typeLiteral :: ClangCStandard -> Parser TypeLit
typeLiteral cStd = do
    kws <- many1 (typeKeyword cStd)
    case interpretKeywords kws of
      Just lit -> return lit
      Nothing  -> fail "unrecognised type literal"

-- | Parse an elaborated type literal
--
-- @
--   struct tag
--   union  tag
--   enum   tag
-- @
taggedTypeLit :: Parser (TagKind, Name)
taggedTypeLit = do
    tag <- choice [
        TagStruct <$ keyword "struct"
      , TagUnion  <$ keyword "union"
      , TagEnum   <$ keyword "enum"
      ]
    name <- parseName
    return $ (tag, name)

data TypeKeyword =
    KwSigned | KwUnsigned
  | KwShort | KwInt | KwLong | KwChar
  | KwFloat | KwDouble
  | KwVoid | KwBool
  deriving stock (Eq)

typeKeyword :: ClangCStandard -> Parser TypeKeyword
typeKeyword cStd = choice $
      [ KwSigned   <$ keyword "signed"
      , KwUnsigned <$ keyword "unsigned"
      , KwShort    <$ keyword "short"
      , KwInt      <$ keyword "int"
      , KwLong     <$ keyword "long"
      , KwChar     <$ keyword "char"
      , KwFloat    <$ keyword "float"
      , KwDouble   <$ keyword "double"
      , KwVoid     <$ keyword "void"
      , KwBool     <$ keyword "_Bool"
      ]
      ++
      -- @bool@ is a keyword in C23 and later.
      case cStd of
        ClangCStandard std _ | std >= C23 -> [bool]
        _                                 -> []
  where
    bool = KwBool <$ keyword "bool"

-- | Combine a list of type keywords into a type literal
--
-- Returns 'Nothing' if the combination is invalid.
--
-- Duplicate keywords (e.g. @signed signed int@) are accepted, since input
-- tokens come from libclang-validated C source and such constructs are
-- rejected by the C compiler long before we see them.
interpretKeywords :: [TypeKeyword] -> Maybe TypeLit
interpretKeywords kws
  -- void
  | kws == [KwVoid]
  = Just TypeVoid

  -- _Bool / bool
  | kws == [KwBool]
  = Just TypeBool

  -- float
  | kws == [KwFloat]
  = Just $ TypeFloat SizeFloat

  -- double
  | kws == [KwDouble]
  = Just $ TypeFloat SizeDouble

  -- char with optional sign
  | KwChar `elem` kws
  , let sign = extractSign kws
  , all (\k -> k `elem` [KwChar, KwSigned, KwUnsigned]) kws
  = Just $ TypeChar sign

  -- integral types: combinations of sign, size, and int
  | all (\k -> k `elem` [KwSigned, KwUnsigned, KwShort, KwInt, KwLong]) kws
  = Just $ TypeInt (extractSign kws) (extractIntSize kws)

  | otherwise
  = Nothing

extractSign :: [TypeKeyword] -> Maybe Sign
extractSign kws
  | KwSigned   `elem` kws = Just Signed
  | KwUnsigned `elem` kws = Just Unsigned
  | otherwise              = Nothing

extractIntSize :: [TypeKeyword] -> Maybe IntSize
extractIntSize kws
  | KwShort `elem` kws                   = Just SizeShort
  | length (filter (== KwLong) kws) >= 2 = Just SizeLongLong
  | KwLong `elem` kws                    = Just SizeLong
  | KwInt `elem` kws                     = Just SizeInt
  | otherwise                            = Nothing
  -- NB: @signed@ alone (no size keyword, no @int@) means @signed int@.
  -- We return Nothing here; the caller interprets (Just sign, Nothing) as int.

-- | Parse zero or more pointer indirections, optionally followed by @const@
pointerLayers :: Parser [Bool]
pointerLayers = many pointerLayer

-- | Parse a pointer indirection, optionally followed by @const@
pointerLayer :: Parser Bool
pointerLayer = do
    punctuation "*"
    option False (True <$ keyword "const")

-- | Match a keyword token with the given spelling
keyword :: Text -> Parser ()
keyword expected = token $ \t ->
    if fromSimpleEnum (tokenKind t) == Right CXToken_Keyword
       && getTokenSpelling (tokenSpelling t) == expected
      then Just ()
      else Nothing

{-------------------------------------------------------------------------------
  Simple expressions
-------------------------------------------------------------------------------}

term :: forall ctx. ClangCStandard -> Vec ctx Name -> Parser (Term ctx Ps)
term cStd macroParams =
    buildExpressionParser ops trm <?> "simple expression"
  where
    trm :: Parser (Term ctx Ps)
    trm = choice [
        Literal <$> lit
      , localParamOrVar
      ]

    localParamOrVar :: Parser (Term ctx Ps)
    localParamOrVar = do
      varName <- parseName
      case lookupParam varName macroParams of
        Just i  -> pure $ LocalParam i
        Nothing -> Var NoXVar varName <$> option [] (actualArgs cStd macroParams)

    lit :: Parser Literal
    lit = ValueLit <$> choice [
        ValueInt    <$> literalInteger
      , ValueFloat  <$> literalFloat
      , ValueChar   <$> literalChar
      , ValueString <$> literalString
      ]

    ops :: OperatorTable [Token TokenSpelling] () Identity (Term ctx Ps)
    ops = []


-- | Parse integer literal
literalInteger :: Parser IntegerLiteral
literalInteger = do
  (txt, (val, ty)) <-
    parseTokenOfKind CXToken_Literal parseLiteralInteger
  return $
    IntegerLiteral
      { integerLiteralText  = txt
      , integerLiteralType  = ty
      , integerLiteralValue = val
      }

-- | Parse floating point literal
literalFloat :: Parser FloatingLiteral
literalFloat = do
  (txt, (fltVal, dblVal, ty)) <-
    parseTokenOfKind CXToken_Literal parseLiteralFloating
  return $
    FloatingLiteral
      { floatingLiteralText = txt
      , floatingLiteralType = ty
      , floatingLiteralFloatValue = fltVal
      , floatingLiteralDoubleValue = dblVal
      }

-- | Parse character literal
literalChar :: Parser CharLiteral
literalChar = do
  (txt, val) <-
    parseTokenOfKind CXToken_Literal parseLiteralChar
  return $
    CharLiteral
      { charLiteralText  = txt
      , charLiteralValue = val
      }

-- | Parse string literal
literalString :: Parser StringLiteral
literalString = do
  (txt, val) <-
    parseTokenOfKind CXToken_Literal parseLiteralString
  return $
    StringLiteral
      { stringLiteralText  = txt
      , stringLiteralValue = val
      }

actualArgs :: ClangCStandard -> Vec ctx Name -> Parser [Expr ctx Ps]
actualArgs cStd macroParams = parens $ expr cStd macroParams `sepBy` comma

{-------------------------------------------------------------------------------
  Expressions

  This is currently only a subset of the operators described in
  <https://en.cppreference.com/w/c/language/operator_precedence>, but we do
  follow the same structure.
-------------------------------------------------------------------------------}

exprTuple :: ClangCStandard -> Vec ctx Name -> Parser (Expr ctx Ps)
exprTuple cStd macroParams = try tuple <|> expr cStd macroParams
  where
    tuple = do
      openParen <- optionMaybe $ punctuation "("
      (e1, e2, es) <- expr cStd macroParams `sepBy2` comma
      case openParen of
        Nothing -> return ()
        Just {} -> punctuation ")"
      return $
        Vec.reifyList es $ \es' ->
           VaApp NoXApp MTuple ( e1 ::: e2 ::: es' )

expr :: forall ctx. ClangCStandard -> Vec ctx Name -> Parser (Expr ctx Ps)
expr cStd macroParams = buildExpressionParser ops trm <?> "expression"
  where

    trm :: Parser (Expr ctx Ps)
    trm = choice [
          parens (expr cStd macroParams)
        , Term <$> term cStd macroParams
        ]

    -- 'OperatorTable' expects the list in descending precedence
    ops = [
        -- Precedence 1 (all left-to-right)
        []

        -- Precedence 2 (all right-to-left)
      , [ Prefix (ap1 MUnaryPlus  <$ punctuation "+")
        , Prefix (ap1 MUnaryMinus <$ punctuation "-")
        , Prefix (ap1 MLogicalNot <$ punctuation "!")
        , Prefix (ap1 MBitwiseNot <$ punctuation "~")
        ]

        -- Precedence 3 (precedence 3 .. 12 are all left-to-right)
      , [ Infix (ap2 MMult <$ punctuation "*") AssocLeft
        , Infix (ap2 MDiv  <$ punctuation "/") AssocLeft
        , Infix (ap2 MRem  <$ punctuation "%") AssocLeft
        ]

        -- Precedence 4
      , [ Infix (ap2 MAdd <$ punctuation "+") AssocLeft
        , Infix (ap2 MSub <$ punctuation "-") AssocLeft
        ]

        -- Precedence 5
      , [ Infix (ap2 MShiftLeft  <$ punctuation "<<") AssocLeft
        , Infix (ap2 MShiftRight <$ punctuation ">>") AssocLeft
        ]

        -- Precedence 6
      , [ Infix (ap2 MRelLT <$ punctuation "<")  AssocLeft
        , Infix (ap2 MRelLE <$ punctuation "<=") AssocLeft
        , Infix (ap2 MRelGT <$ punctuation ">")  AssocLeft
        , Infix (ap2 MRelGE <$ punctuation ">=") AssocLeft
        ]

        -- Precedence 7
      , [ Infix (ap2 MRelEQ <$ punctuation "==") AssocLeft
        , Infix (ap2 MRelNE <$ punctuation "!=") AssocLeft
        ]

        -- Precedence 8 .. 12
      , [ Infix (ap2 MBitwiseAnd <$ punctuation "&")  AssocLeft ]
      , [ Infix (ap2 MBitwiseXor <$ punctuation "^")  AssocLeft ]
      , [ Infix (ap2 MBitwiseOr  <$ punctuation "|")  AssocLeft ]
      , [ Infix (ap2 MLogicalAnd <$ punctuation "&&") AssocLeft ]
      , [ Infix (ap2 MLogicalOr  <$ punctuation "||") AssocLeft ]
      ]

    ap1 :: VaFun (S Z) -> Expr ctx Ps -> Expr ctx Ps
    ap1 op arg = VaApp NoXApp op ( arg ::: VNil )

    ap2 :: VaFun (S (S Z)) -> Expr ctx Ps -> Expr ctx Ps -> Expr ctx Ps
    ap2 op arg1 arg2 = VaApp NoXApp op ( arg1 ::: arg2 ::: VNil )

sepBy2 :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (a, a, [a])
{-# INLINEABLE sepBy2 #-}
sepBy2 p sep = do
  x1 <- p
  void sep
  x2 <- p
  xs <- many $ sep >> p
  return (x1, x2, xs)
