module HsBindgen.C.Reparse.Type (
  reparsePrimType
  ) where

import Data.List (intercalate)
import Data.Text qualified as Text
import Text.Parsec

import HsBindgen.C.Reparse.Infra
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C

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
reparsePrimType :: Reparse (Either () C.PrimType)
reparsePrimType = do
    kws <- many1 primTypeKeyword
    case kws of
      -- void
      ["void"] -> return $ Left ()
      -- char
      [             "char"] -> primType $ C.PrimChar (C.PrimSignImplicit Nothing)
      ["signed"   , "char"] -> primType $ C.PrimChar (C.PrimSignExplicit C.Signed)
      ["unsigned" , "char"] -> primType $ C.PrimChar (C.PrimSignExplicit C.Unsigned)
      -- short
      [             "short"        ] -> primType $ C.PrimIntegral C.PrimShort C.Signed
      ["signed"   , "short"        ] -> primType $ C.PrimIntegral C.PrimShort C.Signed
      ["unsigned" , "short"        ] -> primType $ C.PrimIntegral C.PrimShort C.Unsigned
      [             "short" , "int"] -> primType $ C.PrimIntegral C.PrimShort C.Signed
      ["signed"   , "short" , "int"] -> primType $ C.PrimIntegral C.PrimShort C.Signed
      ["unsigned" , "short" , "int"] -> primType $ C.PrimIntegral C.PrimShort C.Unsigned
      -- int
      [             "int"] -> primType $ C.PrimIntegral C.PrimInt C.Signed
      ["signed"   , "int"] -> primType $ C.PrimIntegral C.PrimInt C.Signed
      ["unsigned" , "int"] -> primType $ C.PrimIntegral C.PrimInt C.Unsigned
      -- long
      [             "long"        ] -> primType $ C.PrimIntegral C.PrimLong C.Signed
      ["signed"   , "long"        ] -> primType $ C.PrimIntegral C.PrimLong C.Signed
      ["unsigned" , "long"        ] -> primType $ C.PrimIntegral C.PrimLong C.Unsigned
      [             "long" , "int"] -> primType $ C.PrimIntegral C.PrimLong C.Signed
      ["signed"   , "long" , "int"] -> primType $ C.PrimIntegral C.PrimLong C.Signed
      ["unsigned" , "long" , "int"] -> primType $ C.PrimIntegral C.PrimLong C.Unsigned
      -- long
      [             "long" , "long"        ] -> primType $ C.PrimIntegral C.PrimLongLong C.Signed
      ["signed"   , "long" , "long"        ] -> primType $ C.PrimIntegral C.PrimLongLong C.Signed
      ["unsigned" , "long" , "long"        ] -> primType $ C.PrimIntegral C.PrimLongLong C.Unsigned
      [             "long" , "long" , "int"] -> primType $ C.PrimIntegral C.PrimLongLong C.Signed
      ["signed"   , "long" , "long" , "int"] -> primType $ C.PrimIntegral C.PrimLongLong C.Signed
      ["unsigned" , "long" , "long" , "int"] -> primType $ C.PrimIntegral C.PrimLongLong C.Unsigned
      -- float, double
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/349>: long double
      ["float" ] -> primType $ C.PrimFloating C.PrimFloat
      ["double"] -> primType $ C.PrimFloating C.PrimDouble
      -- bool
      ["_Bool"] -> primType $ C.PrimBool
      ["bool"]  -> primType $ C.PrimBool
      -- invalid
      _otherwise -> unexpected $ concat [
          "Unexpected primitive type "
        , show $ intercalate " " (map Text.unpack kws)
        ]
  where
    primType :: C.PrimType -> Reparse (Either () C.PrimType)
    primType = return . Right
