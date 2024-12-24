{-# LANGUAGE LambdaCase #-}

module HsBindgen.Clang.Args (
    ClangArgs(..)
  , CStandard(..)
  , defaultClangArgs
  , fromClangArgs
  ) where

import Control.Monad.Except

import HsBindgen.Clang.Version

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | @libclang@ command line arguments
--
-- The default standard when one is not specified depends on the Clang version
-- and has GNU extensions enabled.
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/83> (also #10 and #71).
-- We should support more of the command line arguments of @clang@.
data ClangArgs = ClangArgs {
      -- | Target architecutre
      clangTarget :: Maybe String

      -- | C standard
    , clangCStandard :: Maybe CStandard

      -- | Enable GNU extensions when 'True'
    , clangEnableGnu :: Bool

      -- | Other arguments
      --
      -- See https://clang.llvm.org/docs/ClangCommandLineReference.html
    , clangOtherArgs :: [String]
    }
  deriving stock (Show, Eq)

-- | C standard
--
-- References:
--
-- * "C Support in Clang"
--   <https://clang.llvm.org/c_status.html>
-- * "Differences between various standard modes" in the clang user manual
--   <https://clang.llvm.org/docs/UsersManual.html#differences-between-various-standard-modes>
--
-- We don't currently support @C2y@ because it requires @clang-19@ or later and
-- we have no reliable way to test for that (see 'ClangVersion').
data CStandard =
    C89
  | C99
  | C11
  | C17
  | C23
  deriving stock (Bounded, Enum, Eq, Ord, Show)

defaultClangArgs :: ClangArgs
defaultClangArgs = ClangArgs {
      clangTarget    = Nothing
    , clangCStandard = Nothing
    , clangEnableGnu = False
    , clangOtherArgs = []
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

fromClangArgs :: ClangArgs -> Either String [String]
fromClangArgs args = aux [
      ifGiven clangTarget $ \target ->
        return ["-target", target]

    , ifGiven clangCStandard $ \case
        C89
          | clangEnableGnu -> return ["-std=gnu89"]
          | otherwise      -> return ["-std=c89"]
        C99
          | clangEnableGnu -> return ["-std=gnu99"]
          | otherwise      -> return ["-std=c99"]
        C11
          | clangEnableGnu -> return $
              if clangVersion == ClangOlderThan3_2
                then ["-std=gnu1x"]
                else ["-std=gnu11"]
          | otherwise      -> return $
              if clangVersion == ClangOlderThan3_2
                then ["-std=c1x"]
                else ["-std=c11"]
        C17
          | clangVersion < Clang6 -> throwError "C17 requires clang-6 or later"
          | clangEnableGnu -> return ["-std=gnu17"]
          | otherwise      -> return ["-std=c17"]
        -- We can use @-std=c23@ in @clang-18@ or later, but we have no reliable
        -- way of testing for that.
        C23
          | clangVersion < Clang9_or_10 ->
              throwError "C23 requires clang-9 or later"
          | clangEnableGnu -> return ["-std=gnu2x"]
          | otherwise      -> return ["-std=c2x"]

    , return clangOtherArgs
    ]
  where
    ClangArgs{
        clangTarget
      , clangCStandard
      , clangEnableGnu
      , clangOtherArgs
      } = args

    aux :: [Except String [String]] -> Either String [String]
    aux = runExcept . fmap concat . sequence

    ifGiven ::
         Maybe a
      -> (a -> Except String [String])
      ->       Except String [String]
    ifGiven Nothing  _ = return []
    ifGiven (Just a) f = f a

