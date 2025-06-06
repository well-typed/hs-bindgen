module HsBindgen.C.Reparse.Type (
  reparsePrimType
  ) where

import Data.List (intercalate)
import Data.Text qualified as Text
import Text.Parsec

import HsBindgen.C.Reparse.Infra
import HsBindgen.Imports
import HsBindgen.Language.C

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

reparsePrimType :: Reparse PrimType
reparsePrimType = do
    kws <- many1 primTypeKeyword
    case kws of
      -- char
      [             "char"] -> return $ PrimChar (PrimSignImplicit Nothing)
      ["signed"   , "char"] -> return $ PrimChar (PrimSignExplicit Signed)
      ["unsigned" , "char"] -> return $ PrimChar (PrimSignExplicit Unsigned)
      -- short
      [             "short"        ] -> return $ PrimIntegral PrimShort Signed
      ["signed"   , "short"        ] -> return $ PrimIntegral PrimShort Signed
      ["unsigned" , "short"        ] -> return $ PrimIntegral PrimShort Unsigned
      [             "short" , "int"] -> return $ PrimIntegral PrimShort Signed
      ["signed"   , "short" , "int"] -> return $ PrimIntegral PrimShort Signed
      ["unsigned" , "short" , "int"] -> return $ PrimIntegral PrimShort Unsigned
      -- int
      [             "int"] -> return $ PrimIntegral PrimInt Signed
      ["signed"   , "int"] -> return $ PrimIntegral PrimInt Signed
      ["unsigned" , "int"] -> return $ PrimIntegral PrimInt Unsigned
      -- long
      [             "long"        ] -> return $ PrimIntegral PrimLong Signed
      ["signed"   , "long"        ] -> return $ PrimIntegral PrimLong Signed
      ["unsigned" , "long"        ] -> return $ PrimIntegral PrimLong Unsigned
      [             "long" , "int"] -> return $ PrimIntegral PrimLong Signed
      ["signed"   , "long" , "int"] -> return $ PrimIntegral PrimLong Signed
      ["unsigned" , "long" , "int"] -> return $ PrimIntegral PrimLong Unsigned
      -- long
      [             "long" , "long"        ] -> return $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long"        ] -> return $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long"        ] -> return $ PrimIntegral PrimLongLong Unsigned
      [             "long" , "long" , "int"] -> return $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long" , "int"] -> return $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long" , "int"] -> return $ PrimIntegral PrimLongLong Unsigned
      -- float, double, long double
      [         "float" ] -> return $ PrimFloating PrimFloat
      [         "double"] -> return $ PrimFloating PrimDouble
      ["long" , "double"] -> return $ PrimFloating PrimLongDouble
      -- void
      -- TODO: Re-enable or delete
      -- ["void"] -> return C.TypeVoid
      -- bool
      ["_Bool"] -> return $ PrimBool
      ["bool"]  -> return $ PrimBool
      -- invalid
      _otherwise -> unexpected $ concat [
          "Unexpected primitive type "
        , show $ intercalate " " (map Text.unpack kws)
        ]
