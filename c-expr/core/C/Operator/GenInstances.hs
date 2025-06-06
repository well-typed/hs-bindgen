{-# LANGUAGE TemplateHaskellQuotes #-}

module C.Operator.GenInstances
  ( cExprInstances ) where

-- base
import Prelude
  hiding ( Num(..), Fractional(..), Integral(..) )
import Prelude qualified
import Control.Monad
  ( guard )
import Data.Bits qualified as Bits
import Foreign.C.Types

-- template-haskell
import Language.Haskell.TH qualified as TH

-- c-expr
import C.Type qualified as C
import C.Operator.Classes qualified as C
import C.Operator.Internal qualified as C
import C.Operator.TH

--------------------------------------------------------------------------------

-- | All instances for arithmetic classes on standard types, for the given
-- 'C.Platform'.
cExprInstances :: C.Platform -> TH.Q [ TH.Dec ]
cExprInstances platform = do
  concat <$> sequence [

    ----------------------------------------------------------------------------
    -- Not, Logical

    do impl <- [| \ i -> if C.notNull i then 0 else 1 |]
       withInstanceProofs
         [ genUnaryInstances ''C.Not ( Left $ TH.ConT ''CInt ) ( C.unaryLogicalType platform )
           [ ClassMethod 'C.not "singNot" 1 impl ]
         ]
    ,

    do impl1 <- [| \ i j -> if C.notNull i Prelude.&& C.notNull j then 1 else 0 |]
       impl2 <- [| \ i j -> if C.notNull i Prelude.|| C.notNull j then 1 else 0 |]
       withInstanceProofs
         [ genBinaryInstances ''C.Logical ( Left $ TH.ConT ''CInt ) ( C.binaryLogicalType platform )
           [ ClassMethod '(C.&&) "singAnd" 2 impl1
           , ClassMethod '(C.||) "singOr"  2 impl2
           ]
         ]
    ,

    ----------------------------------------------------------------------------
    -- RelEq, RelOrd

    do impl1 <- [| \ a b -> if a Prelude.== b then 1 else 0 |]
       impl2 <- [| \ a b -> if a Prelude./= b then 1 else 0 |]
       withInstanceProofs
         [ genBinaryInstances ''C.RelEq ( Left $ TH.ConT ''CInt ) ( C.binaryEqType platform )
           [ ClassMethod '(C.==) "singEq"  2 impl1
           , ClassMethod '(C.!=) "singNEq" 2 impl2
           ]
         ]
    ,

    do impl1 <- [| \ a b -> if a Prelude.>  b then 1 else 0 |]
       impl2 <- [| \ a b -> if a Prelude.>= b then 1 else 0 |]
       impl3 <- [| \ a b -> if a Prelude.<  b then 1 else 0 |]
       impl4 <- [| \ a b -> if a Prelude.<= b then 1 else 0 |]
       withInstanceProofs
         [ genBinaryInstances ''C.RelOrd ( Left $ TH.ConT ''CInt ) ( C.binaryRelType platform )
           [ ClassMethod '(C.>)  "singGT" 2 impl1
           , ClassMethod '(C.>=) "singGTE" 2 impl2
           , ClassMethod '(C.<)  "singLT" 2 impl3
           , ClassMethod '(C.<=) "singLTE" 2 impl4
           ]
         ]
    ,

    ----------------------------------------------------------------------------
    -- Plus, Minus

    withInstanceProofs
      [ genUnaryInstances ''C.Plus ( withAssoc "PlusRes" "PlusResImpl" SameArgs )
        ( C.unaryPlusType platform )
        [ ClassMethod 'C.plus "singPlus" 1 ( TH.VarE 'Prelude.id ) ]
      ]
    ,

    genUnaryTyFam platform ( TH.mkName "PlusResImpl" ) C.unaryPlusType
    ,

    withInstanceProofs
      [ genUnaryInstances ''C.Minus ( withAssoc "MinusRes" "MinusResImpl" SameArgs )
        ( C.unaryMinusType platform )
        [ ClassMethod 'C.negate "singNegate" 1 ( TH.VarE 'Prelude.negate ) ]
      ]
    ,

    genUnaryTyFam platform ( TH.mkName "MinusResImpl" ) C.unaryMinusType
    ,

    ----------------------------------------------------------------------------
    -- Add, Sub, Mult, Div, Rem

    withInstanceProofs
      [ genBinaryInstances ''C.Add ( withAssoc "AddRes" "AddResImpl" SameArgs )
        ( C.binaryAddType platform )
        [ ClassMethod '(C.+) "singAdd" 2 ( TH.VarE '(Prelude.+) ) ]
      ]
    ,

    genBinaryTyFam platform ( TH.mkName "AddResImpl" ) C.binaryAddType
    ,

    withInstanceProofs
      [ genBinaryInstances ''C.Sub ( withAssoc "SubRes" "SubResImpl" SameArgs )
        ( C.binarySubType platform )
        [ ClassMethod '(C.-) "singSub" 2 ( TH.VarE '(Prelude.-) ) ]
      ]
    ,

    genBinaryTyFam platform ( TH.mkName "SubResImpl" ) C.binarySubType
    ,

    withInstanceProofs
      [ genBinaryInstances ''C.Mult ( withAssoc "MultRes" "MultResImpl" SameArgs )
        ( C.binaryMultiplicativeType platform )
        [ ClassMethod '(C.*) "singMult" 2 ( TH.VarE '(Prelude.*) ) ]
      ]
    ,

    genBinaryTyFam platform ( TH.mkName "MultResImpl" ) C.binaryMultiplicativeType
    ,

    -- NB: this is the key usage of 'withInstanceProofs' with a non-singleton list
    withInstanceProofs
        -- division for integral types
      [ genBinaryInstances ''C.Div ( withAssoc "DivRes" "MultResImpl" SameArgs ) -- NB: re-use 'MultResImpl'
          ( \ a b ->
            do op@( resTy, _ ) <- C.integralBinaryType platform a b
               guard ( case resTy of C.Arithmetic ( C.FloatLike {} ) -> False; _ -> True )
               return op
          )
          [ ClassMethod '(C./) "singDiv" 2 ( TH.VarE 'Prelude.div ) ]
        -- division for floating-point types
      , genBinaryInstances ''C.Div ( withAssoc "DivRes" "MultResImpl" SameArgs ) -- NB: re-use 'MultResImpl'
          ( \ a b ->
            do op@( resTy, _ ) <- C.binaryMultiplicativeType platform a b
               guard ( case resTy of C.Arithmetic ( C.FloatLike {} ) -> True; _ -> False )
               return op
          )
          [ ClassMethod '(C./) "singDiv" 2 ( TH.VarE '(Prelude./) ) ]
      ]
    ,

    withInstanceProofs
      [ genBinaryInstances ''C.Rem ( withAssoc "RemRes" "BinResImpl" SameArgs ) -- NB: use 'BinResImpl'
        ( C.integralBinaryType platform )
        [ ClassMethod '(C.%) "singRem" 2 ( TH.VarE 'Prelude.rem ) ]
      ]
    ,

    genBinaryTyFam platform ( TH.mkName "BinResImpl" ) C.integralBinaryType
    ,

    ----------------------------------------------------------------------------
    -- Complement, Bitwise, Shift

    withInstanceProofs
      [ genUnaryInstances ''C.Complement ( withAssoc "ComplementRes" "ComplementResImpl" SameArgs )
        ( C.integralUnaryType platform )
        [ ClassMethod '(C..~) "singComplement" 1 ( TH.VarE 'Bits.complement ) ]
      ]
    ,

    genUnaryTyFam platform ( TH.mkName "ComplementResImpl" ) C.integralUnaryType
    ,

    withInstanceProofs
      [ genBinaryInstances ''C.Bitwise ( withAssoc "BitsRes" "BinResImpl" SameArgs ) -- NB: use 'BinResImpl'
        ( C.integralBinaryType platform )
          [ ClassMethod '(C..&.) "singBitAnd" 2 ( TH.VarE '(Bits..&.) )
          , ClassMethod '(C..|.) "singBitOr" 2 ( TH.VarE '(Bits..|.) )
          , ClassMethod '(C..^.) "singBitXor" 2 ( TH.VarE 'Bits.xor )
          ]
      ]
    ,

    do impl1 <- [| \ a i -> Bits.shiftL a ( Prelude.fromIntegral i ) |]
       impl2 <- [| \ a i -> Bits.shiftR a ( Prelude.fromIntegral i ) |]
       withInstanceProofs
         [ genBinaryInstances ''C.Shift ( withAssoc "ShiftRes" "ShiftResImpl" FirstArgOnly )
              -- NB: use 'FirstArgOnly', because the result type only depends on the
              -- first argument.
             ( C.shiftType platform )
             [ ClassMethod '(C.<<) "singShiftL" 2 impl1
             , ClassMethod '(C.>>) "singShiftR" 2 impl2
             ]
         ]
    ,

    genUnaryTyFam platform ( TH.mkName "ShiftResImpl" ) $
      -- The associated type family for Shift is unary, as the result type
      -- only depends on the shiftee type, not the type of the shift amount,
      -- which undergoes an independent arithmetic promotion.
      \ plat ty -> C.shiftType plat ty ( C.Arithmetic $ C.Integral $ C.IntLike $ C.Int C.Signed )


    ]

--------------------------------------------------------------------------------

-- | Utility function to construct a 'AssocTyFam' argument to pass
-- to 'genUnaryInstances' or 'genBinaryInstances'.
withAssoc :: String -> String -> AssocTyFamArgs -> Either TH.Type AssocTyFam
withAssoc famName implName args =
  Right $
    AssocTyFam
      { assocTyFamName     = TH.mkName famName
      , assocTyFamImplName = TH.mkName implName
      , assocTyFamArgs     = args
      }
