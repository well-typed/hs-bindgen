module HsBindgen.C.Reparse.Type (
  reparsePrimType
  ) where

import Data.List (intercalate)
import Data.Text qualified as Text
import Text.Parsec

import HsBindgen.Imports
import HsBindgen.C.AST.Type
import HsBindgen.C.Reparse.Infra

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

primTypeKeyword :: Reparse Text
primTypeKeyword = choice [
      keyword "char"
    , keyword "int"
    , keyword "short"
    , keyword "long"
    , keyword "float"
    , keyword "double"
    , keyword "signed"
    , keyword "unsigned"
    , keyword "void"
    , keyword "_Bool"
    -- TODO: check that "#define bool" is actually defined to _Bool
    -- it would be better to not special-case this
    , keyword "bool", identifier "bool" -- newer LLVM treat "bool" as keyword, older as identifier

    -- The following are unsupported
    , keyword "_Complex"
    , keyword "_Decimal32"
    , keyword "_Decimal64"
    , keyword "_Decimal128"
    ]

reparsePrimType :: Reparse Type
reparsePrimType = do
    kws <- many1 primTypeKeyword
    case kws of
      -- char
      [             "char"] -> return $ TypePrim $ PrimChar (PrimSignImplicit Nothing)
      ["signed"   , "char"] -> return $ TypePrim $ PrimChar (PrimSignExplicit Signed)
      ["unsigned" , "char"] -> return $ TypePrim $ PrimChar (PrimSignExplicit Unsigned)
      -- short
      [             "short"        ] -> return $ TypePrim $ PrimIntegral PrimShort Signed
      ["signed"   , "short"        ] -> return $ TypePrim $ PrimIntegral PrimShort Signed
      ["unsigned" , "short"        ] -> return $ TypePrim $ PrimIntegral PrimShort Unsigned
      [             "short" , "int"] -> return $ TypePrim $ PrimIntegral PrimShort Signed
      ["signed"   , "short" , "int"] -> return $ TypePrim $ PrimIntegral PrimShort Signed
      ["unsigned" , "short" , "int"] -> return $ TypePrim $ PrimIntegral PrimShort Unsigned
      -- int
      [             "int"] -> return $ TypePrim $ PrimIntegral PrimInt Signed
      ["signed"   , "int"] -> return $ TypePrim $ PrimIntegral PrimInt Signed
      ["unsigned" , "int"] -> return $ TypePrim $ PrimIntegral PrimInt Unsigned
      -- long
      [             "long"        ] -> return $ TypePrim $ PrimIntegral PrimLong Signed
      ["signed"   , "long"        ] -> return $ TypePrim $ PrimIntegral PrimLong Signed
      ["unsigned" , "long"        ] -> return $ TypePrim $ PrimIntegral PrimLong Unsigned
      [             "long" , "int"] -> return $ TypePrim $ PrimIntegral PrimLong Signed
      ["signed"   , "long" , "int"] -> return $ TypePrim $ PrimIntegral PrimLong Signed
      ["unsigned" , "long" , "int"] -> return $ TypePrim $ PrimIntegral PrimLong Unsigned
      -- long
      [             "long" , "long"        ] -> return $ TypePrim $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long"        ] -> return $ TypePrim $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long"        ] -> return $ TypePrim $ PrimIntegral PrimLongLong Unsigned
      [             "long" , "long" , "int"] -> return $ TypePrim $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long" , "int"] -> return $ TypePrim $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long" , "int"] -> return $ TypePrim $ PrimIntegral PrimLongLong Unsigned
      -- float, double, long double
      [         "float" ] -> return $ TypePrim $ PrimFloating PrimFloat
      [         "double"] -> return $ TypePrim $ PrimFloating PrimDouble
      ["long" , "double"] -> return $ TypePrim $ PrimFloating PrimLongDouble
      -- void
      ["void"] -> return TypeVoid
      -- bool
      ["_Bool"] -> return $ TypePrim PrimBool
      ["bool"]  -> return $ TypePrim PrimBool
      -- invalid
      _otherwise -> unexpected $ concat [
          "Unexpected primitive type "
        , show $ intercalate " " (map Text.unpack kws)
        ]
