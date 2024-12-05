{-# LANGUAGE GADTs #-}

module Main where

-- base
import System.Exit

-- vec
import Data.Vec.Lazy
  ( Vec(..) )

-- typing-c
import C.Type
import C.Typing.Clang ( queryClangForResultType )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  case checkPlus of
    Right () -> return ()
    Left ( e, a ) -> do
      putStrLn $ unlines [ "checkPlus FAILED", "  " ++ e, "  " ++ a ]
      exitFailure

checkPlus :: Either ( String, String ) ()
checkPlus =
  let int = Arithmetic $ Integral $ IntLike $ Int Signed
  in
    case queryClangForResultType ( int ::: int ::: VNil ) ( \ ( a ::: b ::: VNil ) -> a ++ " + " ++ b ) of
      Just "int" -> Right ()
      Just other -> Left ( "expected: " ++ showTypeAsCType int "", "  actual: " ++ other )
      Nothing    -> Left ( "expected: " ++ showTypeAsCType int "", "  actual: <n/a>" )
