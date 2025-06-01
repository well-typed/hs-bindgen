module HsBindgen.C.Reparse.Type (
  reparsePrimType
  ) where

import Data.List (intercalate)
import Data.Text qualified as Text
import Text.Parsec

import HsBindgen.C.Reparse.Infra
import HsBindgen.Frontend.Macros.AST.C qualified as C
import HsBindgen.Imports
import HsBindgen.Language.C.Prim

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

reparsePrimType :: Reparse C.Type
reparsePrimType = do
    kws <- many1 primTypeKeyword
    case kws of
      -- char
      [             "char"] -> return $ C.TypePrim $ PrimChar (PrimSignImplicit Nothing)
      ["signed"   , "char"] -> return $ C.TypePrim $ PrimChar (PrimSignExplicit Signed)
      ["unsigned" , "char"] -> return $ C.TypePrim $ PrimChar (PrimSignExplicit Unsigned)
      -- short
      [             "short"        ] -> return $ C.TypePrim $ PrimIntegral PrimShort Signed
      ["signed"   , "short"        ] -> return $ C.TypePrim $ PrimIntegral PrimShort Signed
      ["unsigned" , "short"        ] -> return $ C.TypePrim $ PrimIntegral PrimShort Unsigned
      [             "short" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimShort Signed
      ["signed"   , "short" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimShort Signed
      ["unsigned" , "short" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimShort Unsigned
      -- int
      [             "int"] -> return $ C.TypePrim $ PrimIntegral PrimInt Signed
      ["signed"   , "int"] -> return $ C.TypePrim $ PrimIntegral PrimInt Signed
      ["unsigned" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimInt Unsigned
      -- long
      [             "long"        ] -> return $ C.TypePrim $ PrimIntegral PrimLong Signed
      ["signed"   , "long"        ] -> return $ C.TypePrim $ PrimIntegral PrimLong Signed
      ["unsigned" , "long"        ] -> return $ C.TypePrim $ PrimIntegral PrimLong Unsigned
      [             "long" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimLong Signed
      ["signed"   , "long" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimLong Signed
      ["unsigned" , "long" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimLong Unsigned
      -- long
      [             "long" , "long"        ] -> return $ C.TypePrim $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long"        ] -> return $ C.TypePrim $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long"        ] -> return $ C.TypePrim $ PrimIntegral PrimLongLong Unsigned
      [             "long" , "long" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long" , "int"] -> return $ C.TypePrim $ PrimIntegral PrimLongLong Unsigned
      -- float, double, long double
      [         "float" ] -> return $ C.TypePrim $ PrimFloating PrimFloat
      [         "double"] -> return $ C.TypePrim $ PrimFloating PrimDouble
      ["long" , "double"] -> return $ C.TypePrim $ PrimFloating PrimLongDouble
      -- void
      ["void"] -> return C.TypeVoid
      -- bool
      ["_Bool"] -> return $ C.TypePrim PrimBool
      ["bool"]  -> return $ C.TypePrim PrimBool
      -- invalid
      _otherwise -> unexpected $ concat [
          "Unexpected primitive type "
        , show $ intercalate " " (map Text.unpack kws)
        ]
