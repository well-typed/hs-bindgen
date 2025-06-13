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

-- | Primitive type or @void@
reparsePrimType :: Reparse (Either () PrimType)
reparsePrimType = do
    kws <- many1 primTypeKeyword
    case kws of
      -- void
      ["void"] -> return $ Left ()
      -- char
      [             "char"] -> primType $ PrimChar (PrimSignImplicit Nothing)
      ["signed"   , "char"] -> primType $ PrimChar (PrimSignExplicit Signed)
      ["unsigned" , "char"] -> primType $ PrimChar (PrimSignExplicit Unsigned)
      -- short
      [             "short"        ] -> primType $ PrimIntegral PrimShort Signed
      ["signed"   , "short"        ] -> primType $ PrimIntegral PrimShort Signed
      ["unsigned" , "short"        ] -> primType $ PrimIntegral PrimShort Unsigned
      [             "short" , "int"] -> primType $ PrimIntegral PrimShort Signed
      ["signed"   , "short" , "int"] -> primType $ PrimIntegral PrimShort Signed
      ["unsigned" , "short" , "int"] -> primType $ PrimIntegral PrimShort Unsigned
      -- int
      [             "int"] -> primType $ PrimIntegral PrimInt Signed
      ["signed"   , "int"] -> primType $ PrimIntegral PrimInt Signed
      ["unsigned" , "int"] -> primType $ PrimIntegral PrimInt Unsigned
      -- long
      [             "long"        ] -> primType $ PrimIntegral PrimLong Signed
      ["signed"   , "long"        ] -> primType $ PrimIntegral PrimLong Signed
      ["unsigned" , "long"        ] -> primType $ PrimIntegral PrimLong Unsigned
      [             "long" , "int"] -> primType $ PrimIntegral PrimLong Signed
      ["signed"   , "long" , "int"] -> primType $ PrimIntegral PrimLong Signed
      ["unsigned" , "long" , "int"] -> primType $ PrimIntegral PrimLong Unsigned
      -- long
      [             "long" , "long"        ] -> primType $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long"        ] -> primType $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long"        ] -> primType $ PrimIntegral PrimLongLong Unsigned
      [             "long" , "long" , "int"] -> primType $ PrimIntegral PrimLongLong Signed
      ["signed"   , "long" , "long" , "int"] -> primType $ PrimIntegral PrimLongLong Signed
      ["unsigned" , "long" , "long" , "int"] -> primType $ PrimIntegral PrimLongLong Unsigned
      -- float, double, long double
      [         "float" ] -> primType $ PrimFloating PrimFloat
      [         "double"] -> primType $ PrimFloating PrimDouble
      ["long" , "double"] -> primType $ PrimFloating PrimLongDouble
      -- bool
      ["_Bool"] -> primType $ PrimBool
      ["bool"]  -> primType $ PrimBool
      -- invalid
      _otherwise -> unexpected $ concat [
          "Unexpected primitive type "
        , show $ intercalate " " (map Text.unpack kws)
        ]
  where
    primType :: PrimType -> Reparse (Either () PrimType)
    primType = return . Right
