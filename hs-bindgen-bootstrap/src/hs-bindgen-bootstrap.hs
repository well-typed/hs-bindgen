module Main (main) where

import Control.Applicative (many, (<|>))
import Data.ByteString qualified as BS
import Data.Char (isLetter)
import Data.List (stripPrefix)
import Text.Parsec qualified as P
import Text.Parsec.ByteString (Parser)

-------------------------------------------------------------------------------
-- "config"
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    bs <- BS.readFile "imports.h"
    decls <- case P.parse (whitespace *> many declP <* P.eof) "imports.h" bs of
        Left err    -> print err >> fail "parse error"
        Right decls -> return decls

    -- low-level FFI module
    do
        let contents = ffiModule decls
        putStr contents
        writeFile "hs-bindgen-libclang/src/HsBindgen/Clang/LowLevel/FFI.hs" contents

    do
        let contents = wrappers decls
        putStr contents
        writeFile "hs-bindgen-libclang/cbits/clang_wrappers_ffi.h" contents


-------------------------------------------------------------------------------
-- declarations
-------------------------------------------------------------------------------

data Decl
    = FunDecl Var [Var]
    | Comment String
  deriving Show

data Var = Var [String] String
  deriving Show

-------------------------------------------------------------------------------
-- parser
-------------------------------------------------------------------------------

whitespace :: Parser ()
whitespace = P.skipMany (P.satisfy (\c -> c == ' ' || c == '\n'))

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

cident :: Parser String
cident = lexeme $ do
    h <- P.satisfy $ \c -> c == '_' || isLetter c
    t <- many $ P.satisfy $ \c -> c == '_' || isLetter c -- or num
    return (h : t)

-- we cheat a bit, we don't recognise *.
varDecl :: Parser Var
varDecl = do
    x <- cident
    y <- cident
    go (x :) y
  where
    go :: ([String] -> [String]) -> String -> Parser Var
    go xs y = (cident >>= \z -> go (xs . (y : )) z) <|> return (Var (xs []) y)

funDeclP :: Parser Decl
funDeclP = do
    fun <- varDecl
    _ <- lexeme (P.char '(')
    args <- varDecl `P.sepBy` lexeme (P.char ',')
    _ <- lexeme (P.char ')')
    _ <- lexeme (P.char ';')
    return (FunDecl fun args)

commentP :: Parser Decl
commentP = do
    _ <- P.string "//"
    comment <- many $ P.satisfy $ \c -> c /= '\n'
    _ <- lexeme (P.char '\n')
    return (Comment comment)

declP :: Parser Decl
declP = P.choice
    [ funDeclP
    , commentP
    ]

-------------------------------------------------------------------------------
-- Haskell FFI
-------------------------------------------------------------------------------

ffiModule :: [Decl] -> String
ffiModule ds = unlines $
    header ++
    concatMap ffiDecl ds
  where
    header :: [String]
    header =
        [ "{-| this module is autogenerated with cabal run hs-bindgen-bootstrap -}"
        , "module HsBindgen.Clang.LowLevel.FFI (module HsBindgen.Clang.LowLevel.FFI) where"
        , "import Foreign.C.Types"
        , "import HsBindgen.Clang.Internal.ByValue"
        , "import HsBindgen.Clang.LowLevel.Core.Enums"
        , "import HsBindgen.Clang.LowLevel.Core.Structs"
        , "import HsBindgen.Patterns"
        ]

ffiDecl :: Decl -> [String]
ffiDecl (Comment comment) =
    [ "--" ++ comment
    , ""
    ]
ffiDecl (FunDecl (Var rtype name) args)
    | isStruct rtype
    =
    [ "foreign import capi unsafe \"clang_wrappers.h" ++ ffiName ++ "\""
    , "  " ++ name' ++ " :: " ++ foldr argumentTy (toHaskellType Res rtype) args ++ " -> IO ()"
    , ""
    ]

    | otherwise
    =
    [ "foreign import capi unsafe \"clang_wrappers.h" ++ ffiName ++ "\""
    , "  " ++ name' ++ " :: " ++ foldr argumentTy (ioType (toHaskellType Res rtype)) args
    , ""
    ]
  where
    -- name in C
    ffiName
        | isStruct rtype || any isStructVar args
        = ""

        | otherwise
        = " " ++ name

    -- name in Haskell
    name'
        | isStruct rtype || any isStructVar args
        = case stripPrefix "clang_" name of
            Nothing  -> error $ name ++ " doesn't start with clang_"
            Just sfx -> "wrap_" ++ sfx

        | otherwise
        =  case stripPrefix "clang_" name of
            Nothing  -> error $ name ++ " doesn't start with clang_"
            Just sfx -> "nowrapper_" ++ sfx

    argumentTy :: Var -> String -> String
    argumentTy (Var ty _) rest = toHaskellType Arg ty ++ " -> " ++ rest

ioType :: String -> String
ioType ty = if any (== ' ') ty then "IO (" ++ ty ++ ")" else "IO " ++ ty

-------------------------------------------------------------------------------
-- (wrapper) Header generation
-------------------------------------------------------------------------------

wrappers :: [Decl] -> String
wrappers ds = unlines $
    header ++
    concatMap wrapDecl ds
  where
    header :: [String]
    header =
        [ "/* this header is autogenerated with cabal run hs-bindgen-bootstrap */"
        ]

wrapDecl :: Decl -> [String]
wrapDecl (Comment comment) =
    [ "/*" ++ comment ++ " */"
    , ""
    ]
wrapDecl (FunDecl (Var rtype rname) args)
    | isStruct rtype
    =
    [ "static inline void " ++ rname' ++ "(" ++ foldr argumentTy (toCType Res rtype ++ " result") args ++ ") {"
    , "  *result = " ++ rname ++ "(" ++ foldr callArg "" args ++ ");"
    , "}"
    , ""
    ]

    | any isStructVar args
    =
    [ "static inline " ++ toCType Res rtype ++ " " ++ rname' ++ "(" ++ foldr argumentTy "" args ++ ") {"
    , "  return " ++ rname ++ "(" ++ foldr callArg "" args ++ ");"
    , "}"
    , ""
    ]

    | otherwise
    =
    []
  where
    rname' = case stripPrefix "clang_" rname of
        Nothing  -> error $ rname ++ " doesn't start with clang_"
        Just sfx -> "wrap_" ++ sfx

    argumentTy :: Var -> String -> String
    argumentTy (Var atype aname) rest = commaArg (toCType Arg atype ++ " " ++ aname) rest

    callArg :: Var -> String -> String
    callArg (Var atype aname) rest
        | isStruct atype = commaArg ("*" ++ aname) rest
        | otherwise      = commaArg aname rest

commaArg :: String -> String -> String
commaArg x "" = x
commaArg x y  = x ++ ", " ++ y

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

isStruct :: [String] -> Bool
isStruct ["CXType"]   = True
isStruct ["CXString"] = True
isStruct ["CXCursor"] = True
isStruct _            = False

isStructVar :: Var -> Bool
isStructVar (Var ty _) = isStruct ty

-- | result or argument
data RA = Res | Arg

haskellRA :: RA -> String
haskellRA Res = "W "
haskellRA Arg = "R "

toHaskellType :: RA -> [String] -> String
toHaskellType ra ["CXType"]                 = haskellRA ra ++ "CXType_"
toHaskellType ra ["CXString"]               = haskellRA ra ++ "CXString_"
toHaskellType ra ["CXCursor"]               = haskellRA ra ++ "CXCursor_"
toHaskellType _  ["CXTranslationUnit"]      = "CXTranslationUnit" -- typedef to a pointer, not a struct.
toHaskellType _  ["enum","CXCursorKind"]    = "SimpleEnum CXCursorKind"
toHaskellType _  ["enum","CXTypeKind"]      = "SimpleEnum CXTypeKind"
toHaskellType _  ["long","long"]            = "CLLong"
toHaskellType _  ["unsigned","long","long"] = "CULLong"
toHaskellType _  ["unsigned"]               = "CUInt"
toHaskellType _  ["int"]                    = "CInt"
toHaskellType _ ty                          = error $ "Unknown type " ++ unwords ty

cRA :: RA -> String
cRA Res = ""
cRA Arg = "const "

toCType :: RA -> [String] -> String
toCType ra ["CXType"]                 = cRA ra ++ "CXType*"
toCType ra ["CXString"]               = cRA ra ++ "CXString*"
toCType ra ["CXCursor"]               = cRA ra ++ "CXCursor*"
toCType _  ["CXTranslationUnit"]      = "CXTranslationUnit"
toCType _  ["enum","CXCursorKind"]    = "enum CXCursorKind"
toCType _  ["enum","CXTypeKind"]      = "enum CXTypeKind"
toCType _  ["long","long"]            = "long long"
toCType _  ["unsigned","long","long"] = "unsigned long long"
toCType _  ["unsigned"]               = "unsigned"
toCType _  ["int"]                    = "int"
toCType _ ty                          = error $ "Unknown type " ++ unwords ty
