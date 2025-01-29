module HsBindgen.C.Reparse.Type (
    reparseTypeUse
  , reparsePrimType
  ) where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec

import HsBindgen.C.AST
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Reparse.Common

{-------------------------------------------------------------------------------
  Type use sites
-------------------------------------------------------------------------------}

-- | Reparse type use
--
-- TODO: This parser is quite minimal at the moment.
reparseTypeUse :: Reparse Type
reparseTypeUse = choice [
      reparsePrimType
    , TypeTypedef <$> reparseName
    ]

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
    ]

reparsePrimType :: Reparse Type
reparsePrimType = do
    kws <- many1 primTypeKeyword
    case kws of
      -- char
      [             "char"] -> return $ TypePrim $ PrimChar Nothing
      ["signed"   , "char"] -> return $ TypePrim $ PrimChar (Just Signed)
      ["unsigned" , "char"] -> return $ TypePrim $ PrimChar (Just Unsigned)
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

