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
reparseTypeUse :: Reparse Typ
reparseTypeUse = choice [
      TypPrim <$> reparsePrimType
    , TypElaborated <$> reparseName
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
    ]

reparsePrimType :: Reparse PrimType
reparsePrimType = do
    kws <- many1 primTypeKeyword
    case kws of
      -- char
      [             "char"] -> return $ PrimChar Nothing
      ["signed"   , "char"] -> return $ PrimChar (Just Signed)
      ["unsigned" , "char"] -> return $ PrimChar (Just Unsigned)
      -- short
      [             "short"        ] -> return $ PrimIntegral $ PrimShort Signed
      ["signed"   , "short"        ] -> return $ PrimIntegral $ PrimShort Signed
      ["unsigned" , "short"        ] -> return $ PrimIntegral $ PrimShort Unsigned
      [             "short" , "int"] -> return $ PrimIntegral $ PrimShort Signed
      ["signed"   , "short" , "int"] -> return $ PrimIntegral $ PrimShort Signed
      ["unsigned" , "short" , "int"] -> return $ PrimIntegral $ PrimShort Unsigned
      -- int
      [             "int"] -> return $ PrimIntegral $ PrimInt Signed
      ["signed"   , "int"] -> return $ PrimIntegral $ PrimInt Signed
      ["unsigned" , "int"] -> return $ PrimIntegral $ PrimInt Unsigned
      -- long
      [             "long"        ] -> return $ PrimIntegral $ PrimLong Signed
      ["signed"   , "long"        ] -> return $ PrimIntegral $ PrimLong Signed
      ["unsigned" , "long"        ] -> return $ PrimIntegral $ PrimLong Unsigned
      [             "long" , "int"] -> return $ PrimIntegral $ PrimLong Signed
      ["signed"   , "long" , "int"] -> return $ PrimIntegral $ PrimLong Signed
      ["unsigned" , "long" , "int"] -> return $ PrimIntegral $ PrimLong Unsigned
      -- long
      [             "long" , "long"        ] -> return $ PrimIntegral $ PrimLongLong Signed
      ["signed"   , "long" , "long"        ] -> return $ PrimIntegral $ PrimLongLong Signed
      ["unsigned" , "long" , "long"        ] -> return $ PrimIntegral $ PrimLongLong Unsigned
      [             "long" , "long" , "int"] -> return $ PrimIntegral $ PrimLongLong Signed
      ["signed"   , "long" , "long" , "int"] -> return $ PrimIntegral $ PrimLongLong Signed
      ["unsigned" , "long" , "long" , "int"] -> return $ PrimIntegral $ PrimLongLong Unsigned
      -- float, double, long double
      [         "float" ] -> return $ PrimFloating PrimFloat
      [         "double"] -> return $ PrimFloating PrimDouble
      ["long" , "double"] -> return $ PrimFloating PrimLongDouble
      -- void
      ["void"] -> return $ PrimVoid
      -- invalid
      _otherwise -> unexpected $ concat [
          "Unexpected primitive type "
        , show $ intercalate " " (map Text.unpack kws)
        ]

