{-# LANGUAGE ScopedTypeVariables #-}

module C.Operator.Internal where

-- base
import Control.Arrow
  ( (***), first, second )
import Control.Exception
  ( assert )
import qualified Data.Kind as Hs
import Data.Void qualified as Absurd
import GHC.Generics
  ( Generic )

-- fin
import Data.Nat ( Nat(..) )

-- vec
import Data.Vec.Lazy ( Vec(..) )

-- c-expr
import C.Type

--------------------------------------------------------------------------------

-- | **Internal implementation detail**
--
-- How is a C operator, when instantiated at a particular type, implemented?
type OpImpl :: Nat -> Hs.Type
data OpImpl n
  -- | Convert arguments with the given conversions and then apply
  -- the implied function (e.g. addition for the 'Add' class)
  -- at the resulting type.
  = ConvertThenOp
  { argumentConversions :: !( Vec n [ Conversion ] ) }
  -- | Add an integral value and a pointer.
  | AddIntegralAndPtr
  -- | Add a pointer and an integral value.
  | AddPtrAndIntegral
  -- | Get the difference between two pointers.
  | SubPtrAndPtr
  -- | Subtract an integral value from a pointer.
  | SubPtrAndIntegral
  deriving stock ( Show, Generic )

data Conversion
  = FromIntegralTo { fromIntegralTo :: !( Type Absurd.Void ) }
  | RealToFracTo   { realToFracTo   :: !( Type Absurd.Void ) }
  | PtrToInt
  deriving stock ( Show, Generic )

--------------------------------------------------------------------------------

type Op :: Nat -> Hs.Type
data Op arity where
  UnaryOp  :: UnaryOp -> Op ( S Z )
  BinaryOp :: BinaryOp -> Op ( S ( S Z ) )

data UnaryOp
  -- | @+@
  = UnaryPlus
  -- | @-@
  | UnaryMinus
  -- | @!@
  | LogicalNot
  -- | @~@
  | BitwiseNot
  deriving stock ( Eq, Ord, Show, Enum, Bounded )

data BinaryOp
  -- | @*@
  = Mult
  -- | @/@
  | Div
  -- | @%@
  | Rem
  -- | @+@
  | Add
  -- | @-@
  | Sub
  -- | @<<@
  | ShiftLeft
  -- | @>>@
  | ShiftRight
  -- | @<@
  | RelLT
  -- | @<=@
  | RelLE
  -- | @>@
  | RelGT
  -- | @>=@
  | RelGE
  -- | @==@
  | RelEQ
  -- | @!=@
  | RelNE
  -- | @&@
  | BitwiseAnd
  -- | @^@
  | BitwiseXor
  -- | @|@
  | BitwiseOr
  -- | @&&@
  | LogicalAnd
  -- | @||@
  | LogicalOr
  deriving stock ( Eq, Ord, Show, Enum, Bounded )

pprOp :: Op arity -> String
pprOp = \case
  UnaryOp op ->
    case op of
      UnaryPlus  -> "+"
      UnaryMinus -> "-"
      LogicalNot -> "!"
      BitwiseNot -> "~"
  BinaryOp op ->
    case op of
      Mult       -> "*"
      Div        -> "/"
      Rem        -> "%"
      Add        -> "+"
      Sub        -> "-"
      ShiftLeft  -> "<<"
      ShiftRight -> ">>"
      RelLT      -> "<"
      RelLE      -> "<="
      RelGT      -> ">"
      RelGE      -> ">="
      RelEQ      -> "=="
      RelNE      -> "!="
      BitwiseAnd -> "&"
      BitwiseXor -> "^"
      BitwiseOr  -> "|"
      LogicalAnd -> "&&"
      LogicalOr  -> "||"

pprOpApp :: forall arity. Op arity -> Vec arity String -> String
pprOpApp op args =
  case op of
    UnaryOp  {} -> unary
    BinaryOp {} -> binary
  where
    unary :: arity ~ S Z => String
    unary =
      case args of
        a ::: VNil ->
          pprOp op ++ a
    binary :: arity ~ S ( S Z ) => String
    binary =
      case args of
        a ::: b ::: VNil ->
          a ++ pprOp op ++ b

--------------------------------------------------------------------------------

opResTypeAndImpl :: forall arity a. Eq a => Platform -> Op arity -> Vec arity ( Type a ) -> Maybe ( Type a, OpImpl arity )
opResTypeAndImpl plat op args =
  case op of
    UnaryOp o ->
      case o of
        UnaryPlus  -> unary unaryPlusType
        UnaryMinus -> unary unaryMinusType
        LogicalNot -> unary unaryLogicalType
        BitwiseNot -> unary integralUnaryType
    BinaryOp o ->
      case o of
        Mult       -> binary binaryMultiplicativeType
        Div        -> binary binaryMultiplicativeType
        Rem        -> binary integralBinaryType
        Add        -> binary binaryAddType
        Sub        -> binary binarySubType
        ShiftLeft  -> binary shiftType
        ShiftRight -> binary shiftType
        RelLT      -> binary binaryRelType
        RelLE      -> binary binaryRelType
        RelGT      -> binary binaryRelType
        RelGE      -> binary binaryRelType
        RelEQ      -> binary binaryEqType
        RelNE      -> binary binaryEqType
        BitwiseAnd -> binary integralBinaryType
        BitwiseXor -> binary integralBinaryType
        BitwiseOr  -> binary integralBinaryType
        LogicalAnd -> binary binaryLogicalType
        LogicalOr  -> binary binaryLogicalType
  where
    unary :: arity ~ S Z => ( Platform -> Type a -> Maybe r ) -> Maybe r
    unary f =
      case args of
        a ::: VNil ->
          f plat a
    binary :: arity ~ S ( S Z ) => ( Platform -> Type a -> Type a -> Maybe r ) -> Maybe r
    binary f =
      case args of
        a ::: b ::: VNil ->
          f plat a b

--------------------------------------------------------------------------------

-- | Result type of unary @+@
unaryPlusType :: Platform -> Type a -> Maybe ( Type a, OpImpl ( S Z ) )
unaryPlusType plat = \case
  Arithmetic ty ->
    Just $ mkArithConv1 $ arithmeticPromotion plat ty
  Ptr {} ->
    -- The C++ standard allows unary plus on pointers, but the C standard doesn't.
    Nothing
  Void -> Nothing

-- | Result type of unary @-@
unaryMinusType :: Platform -> Type a -> Maybe ( Type a, OpImpl ( S Z ) )
unaryMinusType plat = \case
  Arithmetic ty ->
    Just $ mkArithConv1 $ arithmeticPromotion plat ty
  Ptr {} -> Nothing
  Void   -> Nothing

-- | Result type of binary @+@
binaryAddType :: Platform -> Type a -> Type a -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
binaryAddType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ mkArithConv2 $ arithmeticConversion plat a1 a2
binaryAddType _ ( Arithmetic ( Integral {} ) ) ptr@( Ptr {} )
  = Just ( ptr, AddIntegralAndPtr )
binaryAddType _ ptr@( Ptr {} ) ( Arithmetic ( Integral {} ) )
  = Just ( ptr, AddPtrAndIntegral )
binaryAddType _ _ _
  = Nothing

-- | Result type of binary @-@
binarySubType :: Eq a => Platform -> Type a -> Type a -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
binarySubType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ mkArithConv2 $ arithmeticConversion plat a1 a2
binarySubType _ ptr@( Ptr {} ) ( Arithmetic ( Integral {} ) )
  = Just ( ptr, SubPtrAndIntegral )
binarySubType _ ( Ptr ty1 ) ( Ptr ty2 )
  | ty1 == ty2
  -- TODO: do we want to be more lenient in allowing subtraction of pointers
  -- with different pointee types, e.g. allow @(x :: Ptr Void) - (y :: Ptr Int)@?
  = Just ( Arithmetic $ Integral $ IntLike PtrDiff, SubPtrAndPtr )
binarySubType _ _ _
  = Nothing


-- | Result type for multiplication and division (integral and floating-point)
binaryMultiplicativeType :: Platform -> Type a -> Type a -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
binaryMultiplicativeType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ mkArithConv2 $ arithmeticConversion plat a1 a2
binaryMultiplicativeType _ _ _ = Nothing

-- | Result type for bitwise not operator
integralUnaryType :: Platform -> Type a -> Maybe ( Type a, OpImpl ( S Z ) )
integralUnaryType plat ( Arithmetic a1 )
  | Integral {} <- a1
  = Just $ mkArithConv1 $ arithmeticPromotion plat a1
integralUnaryType _ _ = Nothing

-- | Type for integral remainder and binary bitwise logic operators
integralBinaryType :: Platform -> Type a -> Type a -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
integralBinaryType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  | Integral {} <- a1
  , Integral {} <- a2
  = Just $ mkArithConv2 $ arithmeticConversion plat a1 a2
integralBinaryType _ _ _ = Nothing

-- | Type for binary shift operators
shiftType :: Platform
          -> Type a -- ^ type of the value being shifted
          -> Type a -- ^ type of the shift amount
          -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
shiftType plat ( Arithmetic a1@( Integral {} ) ) ( Arithmetic a2@( Integral {} ) )
  = let ( i1, c1 ) = arithmeticPromotion plat a1
        ( _ , c2 ) = arithmeticPromotion plat a2
    in Just ( Arithmetic i1, ConvertThenOp ( c1 ::: c2 ::: VNil ) )
shiftType _ _ _
  = Nothing

intType, uintType :: Type a
intType  = Arithmetic $ Integral $ IntLike $ Int Signed
uintType = Arithmetic $ Integral $ IntLike $ Int Unsigned

convertToInt :: Platform -> Type a -> Maybe [ Conversion ]
convertToInt _ = \case
  Arithmetic a -> Just $ case a of
    Integral i ->
      case i of
        IntLike ( Int Signed ) -> []
        _ -> [ FromIntegralTo intType ]
    FloatLike {} ->
      [ RealToFracTo intType ]
  Ptr {}        ->
    Just [ PtrToInt ]
  _             ->
    Nothing

-- | Type for logical not operation @!@.
unaryLogicalType :: Platform -> Type a -> Maybe ( Type a, OpImpl ( S Z ) )
unaryLogicalType plat a = do
  _conv <- convertToInt plat a
  return $ ( intType, ConvertThenOp ( [] ::: VNil ) )

-- | Type for binary equality operators @==@ and @!=@.
binaryEqType :: Eq a => Platform -> Type a -> Type a -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
binaryEqType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ mkArithConv2 ( Integral $ IntLike $ Int Signed, snd $ arithmeticConversion plat a1 a2 )
binaryEqType _ ( Ptr ty1 ) ( Ptr ty2 )
  | ty1 == ty2
  -- TODO: does the C standard allow "Ptr Int" == "Ptr Void"?
  = Just ( intType, ConvertThenOp ( [] ::: [] ::: VNil ) )
binaryEqType _ _ _ = Nothing

-- | Type for binary logical operators @&&@ and @||@.
binaryLogicalType :: Platform -> Type a -> Type a -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
binaryLogicalType plat a1 a2 = do
  _c1 <- convertToInt plat a1
  _c2 <- convertToInt plat a2
  return ( intType, ConvertThenOp ( [] ::: [] ::: VNil ) )

-- | Type for binary comparison operators @<@, @<=@, @>@, @>=@.
binaryRelType :: Eq a => Platform -> Type a -> Type a -> Maybe ( Type a, OpImpl ( S ( S Z ) ) )
binaryRelType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ mkArithConv2 ( Integral $ IntLike $ Int Signed, snd $ arithmeticConversion plat a1 a2 )
binaryRelType _ ( Ptr ty1 ) ( Ptr ty2 )
  | ty1 == ty2
  -- TODO: C is a bit more lenient than requiring the inner types to
  -- match exactly.
  = Just ( intType, ConvertThenOp ( [] ::: [] ::: VNil ) )
binaryRelType _ _ _ = Nothing

mkArithConv1 :: ( ArithmeticType, [ Conversion ] ) -> ( Type a, OpImpl ( S Z ) )
mkArithConv1 =
  ( Arithmetic *** ( \ a -> ConvertThenOp ( a ::: VNil ) ) )

mkArithConv2 :: ( ArithmeticType, ( [ Conversion ], [ Conversion ] ) ) -> ( Type a, OpImpl ( S ( S Z ) ) )
mkArithConv2 =
  ( Arithmetic *** ( \ ( a, b ) -> ConvertThenOp ( a ::: b ::: VNil ) ) )

--------------------------------------------------------------------------------

arithmeticPromotion :: Platform -> ArithmeticType -> ( ArithmeticType, [ Conversion ] )
arithmeticPromotion _ f@( FloatLike {} ) =
  ( f, [] )
arithmeticPromotion plat ( Integral i ) =
  first ( Integral . IntLike ) $ integralPromotion plat i

integralPromotion :: Platform -> IntegralType -> ( IntLikeType, [ Conversion ] )
  -- C standard: Promotion from integral types (non bit-field case).
  --
  -- If the integer conversion rank of T is lower than that of int:
  --
  --   1. promote T to int if int can represent all the values of T,
  --   2. otherwise promote T to unsigned int
integralPromotion plat ( IntLike i )
  | intLikeTypeConversionRank plat i < intLikeTypeConversionRank plat ( Int Signed )
  = if intLikeTypeFitsInInt plat i
    then ( Int Signed  , [ FromIntegralTo intType  ] )
    else ( Int Unsigned, [ FromIntegralTo uintType ] )
  | otherwise
  = ( i, [ ] )
integralPromotion plat ( CharLike c )
  = assert ( charLikeTypeSizeInBits plat c < intLikeTypeSizeInBits plat ( Int Signed ) )
    ( Int Signed, [ FromIntegralTo intType ] )
integralPromotion _ Bool
  = ( Int Signed, [ FromIntegralTo intType ] )

--------------------------------------------------------------------------------
-- Arithmetic conversion

arithmeticConversion :: Platform -> ArithmeticType -> ArithmeticType -> ( ArithmeticType, ( [ Conversion ], [ Conversion ] ) )
arithmeticConversion plat ( Integral i1 ) ( Integral i2 )
  -- Both arguments are integral: do integral promotion then integral conversion.
  = let
      ( j1, c1 ) = integralPromotion plat i1
      ( j2, c2 ) = integralPromotion plat i2
      ( r, ( d1, d2 ) ) = integralArithmeticConversion plat j1 j2
    in ( Integral $ IntLike r, ( c1 ++ d1, c2 ++ d2 ) )

-- At least one of the arguments is of floating-point type:
-- pick the largest floating-point type.
arithmeticConversion _ ( FloatLike f1 )  ( FloatLike f2 ) =
  ( FloatLike ( max f1 f2 ), ( if f2 > f1 then [ rf f2 ] else [], if f1 > f2 then [ rf f1 ] else [] ) )
    where
      rf f = RealToFracTo $ Arithmetic $ FloatLike f
arithmeticConversion _ f@( FloatLike {} ) ( Integral {} ) =
  ( f, ( [], [ FromIntegralTo ( Arithmetic f ) ] ) )
arithmeticConversion _ ( Integral {} ) f@( FloatLike {} ) =
  ( f, ( [ FromIntegralTo ( Arithmetic f ) ], [] ) )

integralArithmeticConversion :: Platform -> IntLikeType -> IntLikeType -> ( IntLikeType, ( [ Conversion ], [ Conversion ] ) )
integralArithmeticConversion plat t1 t2
  -- The following rules are applied to determine the arithmetic conversion result type 'C':
  --
  --   1. If 'T1' and 'T2' are the same type, 'C' is that type.
  | t1 == t2
  = ( t1, ( [], [] ) )
  --   2. If T1 and T2 are both signed integer types or both unsigned integer types,
  --      C is the type of greater integer conversion rank.
  | s1 == s2
  = if rk1 >= rk2
    then ( t1, ( [], [ FromIntegralTo ( Arithmetic $ Integral $ IntLike t1 ) ] ) )
    else ( t2, ( [ FromIntegralTo ( Arithmetic $ Integral $ IntLike t2 ) ], [] ) )
  | otherwise
  --   3. Otherwise, the types are of different signs.
  --      Implement the logic in 'integralArithmeticConversion_differentSigns'.
  = case s1 of
      Signed ->
        integralArithmeticConversion_differentSigns plat ( t1, rk1 ) ( t2, rk2 )
      Unsigned ->
        second ( \ ( c1, c2 ) -> ( c2, c1 ) ) $
          integralArithmeticConversion_differentSigns plat ( t2, rk2 ) ( t1, rk1 )
  where
    s1, s2 :: Sign
    s1 = intLikeTypeSign t1
    s2 = intLikeTypeSign t2
    rk1, rk2 :: IntegerConversionRank
    rk1 = intLikeTypeConversionRank plat t1
    rk2 = intLikeTypeConversionRank plat t2


integralArithmeticConversion_differentSigns
  :: Platform
  -> ( IntLikeType, IntegerConversionRank ) -- ^ the signed type
  -> ( IntLikeType, IntegerConversionRank ) -- ^ the unsigned type
  -> ( IntLikeType, ( [ Conversion ], [ Conversion ] ) )
integralArithmeticConversion_differentSigns plat s@( t_s, rk_s ) u@( t_u, rk_u )
  -- Implement the following rules to determine the arithmetic conversion
  -- result type C for signed type S and unsigned type U:
  --
  --   1. If the rank of U is greater than or equal to the rank of S, C is U.
  | rk_u >= rk_s
  = ( t_u, ( [ FromIntegralTo ( Arithmetic $ Integral $ IntLike t_u ) ], [] ) )
  -- Otherwise, S has (strictly) greater rank than U.
  --
  --   2. If S can represent all of the values of U, C is S.
  | unsignedFitsInSigned plat t_u t_s
  = ( t_s, ( [], [ FromIntegralTo ( Arithmetic $ Integral $ IntLike t_s ) ] ) )
  --   3. Otherwise, C is the unsigned integer type corresponding to S.
  | otherwise
  = ( \ iTy ->
      let ty = Arithmetic $ Integral $ IntLike iTy
      in ( iTy , ( [ FromIntegralTo ty ], [ FromIntegralTo ty ] ) )
    ) $
     case t_s of
      Short    {} -> Short    Unsigned
      Int      {} -> Int      Unsigned
      Long     {} -> Long     Unsigned
      LongLong {} -> LongLong Unsigned
      _ ->
        -- Should never happen, because any unsigned type of rank strictly
        -- less than that of ptrdiff_t fits into ptrdiff_t.
        error $ unlines
          [ "integralArithmeticConversion_differentSigns: extended type"
          , "ty: " ++ show t_s
          , "s: " ++ show s
          , "u: " ++ show u
          ]

-- | Does the given unsigned type fit into the given signed type?
unsignedFitsInSigned
  :: Platform
  -> IntLikeType -- ^ the unsigned type
  -> IntLikeType -- ^ the signed type
  -> Bool
unsignedFitsInSigned plat u s =
  intLikeTypeSizeInBits plat s > intLikeTypeSizeInBits plat u

--------------------------------------------------------------------------------
