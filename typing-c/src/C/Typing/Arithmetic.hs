{-# LANGUAGE LambdaCase #-}

module C.Typing.Arithmetic where

-- base
import Control.Exception (assert)

-- typing-c
import C.Type

--------------------------------------------------------------------------------

unaryPlusType :: Platform -> Type -> Maybe Type
unaryPlusType plat = \case
  Arithmetic ty ->
    Just $ Arithmetic $ arithmeticPromotion plat ty
  ptr@( Ptr {} ) ->
    Just ptr
  Void -> Nothing
  Struct {} -> Nothing

unaryMinusType :: Platform -> Type -> Maybe Type
unaryMinusType plat = \case
  Arithmetic ty ->
    Just $ Arithmetic $ arithmeticPromotion plat ty
  Ptr {} -> Nothing
  Void   -> Nothing
  Struct {} -> Nothing

binaryAddType :: Platform -> Type -> Type -> Maybe Type
binaryAddType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ Arithmetic $ arithmeticConversion plat a1 a2
binaryAddType _ ( Arithmetic ( Integral {} ) ) ptr@( Ptr {} )
  = Just ptr
binaryAddType _ ptr@( Ptr {} ) ( Arithmetic ( Integral {} ) )
  = Just ptr
binaryAddType _ _ _
  = Nothing

binarySubType :: Platform -> Type -> Type -> Maybe Type
binarySubType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ Arithmetic $ arithmeticConversion plat a1 a2
binarySubType _ ptr@( Ptr {} ) ( Arithmetic ( Integral {} ) )
  = Just ptr
binarySubType _ ( Ptr ty1 ) ( Ptr ty2 )
  | ty1 == ty2
  = Just $ Arithmetic $ Integral $ IntLike $ PtrDiff
binarySubType _ _ _
  = Nothing


-- | Type for multiplication and division
binaryMultiplicativeType :: Platform -> Type -> Type -> Maybe Type
binaryMultiplicativeType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  = Just $ Arithmetic $ arithmeticConversion plat a1 a2
binaryMultiplicativeType _ _ _ = Nothing

-- | Type for bitwise not operator
integralUnaryType :: Platform -> Type -> Maybe Type
integralUnaryType plat ( Arithmetic a1 )
  | Integral {} <- a1
  = Just $ Arithmetic $ arithmeticPromotion plat a1
integralUnaryType _ _ = Nothing

-- | Type for division with remainder and binary bitwise logic operators
integralBinaryType :: Platform -> Type -> Type -> Maybe Type
integralBinaryType plat ( Arithmetic a1 ) ( Arithmetic a2 )
  | Integral {} <- a1
  , Integral {} <- a2
  = Just $ Arithmetic $ arithmeticConversion plat a1 a2
integralBinaryType _ _ _ = Nothing

shiftTypes :: Platform
           -> Type -- ^ type of the value being shifted
           -> Type -- ^ type of the shift amount
           -> Maybe Type
shiftTypes plat ( Arithmetic a1@( Integral {} ) ) ( Arithmetic ( Integral {} ) )
  = Just ( Arithmetic $ arithmeticPromotion plat a1 )
shiftTypes _ _ _
  = Nothing

--------------------------------------------------------------------------------

arithmeticPromotion :: Platform -> ArithmeticType -> ArithmeticType
arithmeticPromotion _ f@( FloatLike {} ) = f
arithmeticPromotion plat ( Integral i ) = Integral $ IntLike $ integralPromotion plat i

integralPromotion :: Platform -> IntegralType -> IntLikeType
  -- If the integer conversion rank of T is lower than the rank of int:
  --
  --   1. promote T to int if int can represent all the values of T,
  --   2. otherwise promote T to unsigned int
integralPromotion plat ( IntLike i )
  | intLikeTypeConversionRank plat i < intLikeTypeConversionRank plat ( Int Signed )
  = assert ( intLikeTypeSizeInBits plat i < intLikeTypeSizeInBits plat ( Int Signed ) )
    ( Int Signed )
  | otherwise
  = i
integralPromotion plat ( CharLike c )
  = assert ( charLikeTypeSizeInBits plat c < intLikeTypeSizeInBits plat ( Int Signed ) )
    ( Int Signed )
integralPromotion _ Bool
  = Int Signed

--------------------------------------------------------------------------------
-- Arithmetic conversion

arithmeticConversion :: Platform -> ArithmeticType -> ArithmeticType -> ArithmeticType
arithmeticConversion plat ( Integral i1 ) ( Integral i2 )
  -- Both arguments are integral: do integral promotion then integral conversion.
  = Integral $ IntLike $
      integralArithmeticConversion plat
        ( integralPromotion plat i1 )
        ( integralPromotion plat i2 )

-- At least one of the arguments is of floating-point type:
-- pick the largest floating-point type.
arithmeticConversion _ ( FloatLike f1 )  ( FloatLike f2 ) = FloatLike ( max f1 f2 )
arithmeticConversion _ f@( FloatLike {} ) ( Integral {} ) = f
arithmeticConversion _ ( Integral {} ) f@( FloatLike {} ) = f

integralArithmeticConversion :: Platform -> IntLikeType -> IntLikeType -> IntLikeType
integralArithmeticConversion plat t1 t2
  -- The following rules are applied to determine the arithmetic conversion result type 'C':
  --
  --   1. If 'T1' and 'T2' are the same type, 'C' is that type.
  | t1 == t2
  = t1
  --   2. If T1 and T2 are both signed integer types or both unsigned integer types,
  --      C is the type of greater integer conversion rank.
  | s1 == s2
  = if rk1 >= rk2 then t1 else t2
  | otherwise
  --   3. Otherwise, the types are of different signs.
  --      Implement the logic in 'integralArithmeticConversion_differentSigns'.
  = case s1 of
      Signed ->
        integralArithmeticConversion_differentSigns plat ( t1, rk1 ) ( t2, rk2 )
      Unsigned ->
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
  -> IntLikeType
integralArithmeticConversion_differentSigns plat s@( t_s, rk_s ) u@( t_u, rk_u )
  -- Implement the following rules to determine the arithmetic conversion
  -- result type C for signed type S and unsigned type U:
  --
  --   1. If the rank of U is greater than or equal to the rank of S, C is U.
  | rk_u > rk_s
  = t_u
  -- Otherwise, S has (strictly) greater rank than U.
  --
  --   2. If S can represent all of the values of U, C is S.
  | unsignedFitsInSigned plat t_u t_s
  = t_s
  --   3. Otherwise, C is the unsigned integer type corresponding to S.
  | otherwise
  = case t_s of
      Short    {} -> Short    Unsigned
      Int      {} -> Int      Unsigned
      Long     {} -> Long     Unsigned
      LongLong {} -> LongLong Unsigned
      PtrDiff ->
        -- Should never happen, because any unsigned type of rank strictly
        -- less than that of ptrdiff_t fits into ptrdiff_t.
        error $ unlines
          [ "integralArithmeticConversion_differentSigns: ptrdiff_t"
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
