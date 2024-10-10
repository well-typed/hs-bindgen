{-# LANGUAGE OverloadedStrings #-}

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
      [             "short"        ] -> return $ PrimShort Signed
      ["signed"   , "short"        ] -> return $ PrimShort Signed
      ["unsigned" , "short"        ] -> return $ PrimShort Unsigned
      [             "short" , "int"] -> return $ PrimShort Signed
      ["signed"   , "short" , "int"] -> return $ PrimShort Signed
      ["unsigned" , "short" , "int"] -> return $ PrimShort Unsigned
      -- int
      [             "int"] -> return $ PrimInt Signed
      ["signed"   , "int"] -> return $ PrimInt Signed
      ["unsigned" , "int"] -> return $ PrimInt Unsigned
      -- long
      [             "long"        ] -> return $ PrimLong Signed
      ["signed"   , "long"        ] -> return $ PrimLong Signed
      ["unsigned" , "long"        ] -> return $ PrimLong Unsigned
      [             "long" , "int"] -> return $ PrimLong Signed
      ["signed"   , "long" , "int"] -> return $ PrimLong Signed
      ["unsigned" , "long" , "int"] -> return $ PrimLong Unsigned
      -- long
      [             "long" , "long"        ] -> return $ PrimLongLong Signed
      ["signed"   , "long" , "long"        ] -> return $ PrimLongLong Signed
      ["unsigned" , "long" , "long"        ] -> return $ PrimLongLong Unsigned
      [             "long" , "long" , "int"] -> return $ PrimLongLong Signed
      ["signed"   , "long" , "long" , "int"] -> return $ PrimLongLong Signed
      ["unsigned" , "long" , "long" , "int"] -> return $ PrimLongLong Unsigned
      -- float, double, long double
      [         "float" ] -> return $ PrimFloat
      [         "double"] -> return $ PrimDouble
      ["long" , "double"] -> return $ PrimLongDouble
      -- void
      ["void"] -> return $ PrimVoid
      -- invalid
      _otherwise -> unexpected $ concat [
          "Unexpected primitive type "
        , show $ intercalate " " (map Text.unpack kws)
        ]

