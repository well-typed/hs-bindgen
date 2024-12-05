{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module C.Typing.Arithmetic.TyFams
  ( PlusRes, MinusRes, ComplementRes
  , AddRes, SubRes, MultRes, BinRes, ShiftRes
  ) where

-- base
import Foreign
  ( Ptr )
import Foreign.C

-- typing-c
import C.Typing.Arithmetic
import C.Typing.Arithmetic.TH
  ( genUnaryTyFam, genBinaryTyFam )

--------------------------------------------------------------------------------


-- | Return type of unary plus operator
$( genUnaryTyFam "PlusRes"  unaryPlusType )

-- | Return type of unary minus operator
$( genUnaryTyFam "MinusRes" unaryMinusType )

-- | Return type of unary bitwise not operator
$( genUnaryTyFam "ComplementRes" integralUnaryType )

-- Binary

-- | Return type of binary addition operator
$( genBinaryTyFam "AddRes"  binaryAddType )

-- | Return type of binary subtraction operator
$( genBinaryTyFam "SubRes"  binarySubType )

-- | Return type of binary mutiplication and division operators
$( genBinaryTyFam "MultRes" binaryMultiplicativeType )

-- | Return type of division with remainder and bitwise logical operators
$( genBinaryTyFam "BinRes" integralBinaryType )

-- | Return type of bit shift operators
$( genBinaryTyFam "ShiftRes" shiftTypes )

--------------------------------------------------------------------------------
