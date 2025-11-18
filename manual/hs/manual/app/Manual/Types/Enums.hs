{-# OPTIONS_GHC -Wno-orphans #-}

module Manual.Types.Enums (examples) where

import Foreign as F
import Foreign.C qualified as FC
import System.IO.Unsafe
import Text.Read (readEither)

import HsBindgen.Runtime.CEnum (AsCEnum (..), AsSequentialCEnum (..))

import Manual.Tools

import Example
import Example.Unsafe

{-------------------------------------------------------------------------------
  Enum instances
-------------------------------------------------------------------------------}

deriving via AsCEnum HTTP_status instance Enum HTTP_status
deriving newtype instance Bounded HTTP_status

deriving via AsSequentialCEnum Vote instance Enum    Vote
deriving via AsSequentialCEnum Vote instance Bounded Vote

deriving via AsCEnum Descending instance Enum Descending

showCursorKind :: CXCursorKind -> String
showCursorKind = \case
    CXCursor_UnexposedExpr -> "CXCursor_UnexposedExpr"
    CXCursor_UnexposedStmt -> "CXCursor_UnexposedStmt"
    kind -> show kind

-- On Windows the underlying data type generated for `Index` is FC.CInt
-- instead of FC.CUInt.
readEitherIndexWith :: FC.CUInt -> String -> Either String Index
readEitherIndexWith upperBound x = case readEither x of
  Right (Index v) | v > upperBound -> Left $ "index out of bounds: " <> show v
  other                            -> other

{-------------------------------------------------------------------------------
  Using enums
-------------------------------------------------------------------------------}

indexTriple :: Triple -> Index -> Int
indexTriple triple ix = unsafePerformIO $
    with triple $ \ptr -> fromIntegral <$> index_triple ptr ix

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Triple -> IO ()
examples triple = do
    section "Enums"

    subsection "Simple enum"
    print (indexTriple triple A)

    subsection "Enum instances"
    print [Ok, minBound]
    putStrLn $ "After " ++ show Moved ++ " comes " ++ show (succ Moved)
    putStrLn $ "Possible votes: " ++ show ([minBound .. maxBound] :: [Example.Vote])
    print CXCursor_UnexposedExpr
    putStrLn $ showCursorKind CXCursor_UnexposedExpr
    print (succ Y, pred Y)

    subsection "Read instance"
    -- Read instance (Index).
    print $ "Read declared (A ~ Index 0): " <> show (read "A" :: Example.Index)
    print $ "Read declared but using undeclared string (Index 0): "
      <> show (read "Index 0" :: Example.Index)
    print $ "Read undeclared (Index 10): " <> show (read "Index 10" :: Example.Index)
    -- Read instance (HTTP_status).
    print $ (read "HTTP_status 200" :: Example.HTTP_status)
    print $ (read "Ok" :: Example.HTTP_status)
    print $ (read "HTTP_status 200" :: Example.HTTP_status) == (read "Ok" :: Example.HTTP_status)
    -- Read instance (overriding).
    print $ (readEitherIndexWith 100 "Index (-1)")

    subsection "Static inline function"
    -- Static inline function
    print =<< mod_10 123
