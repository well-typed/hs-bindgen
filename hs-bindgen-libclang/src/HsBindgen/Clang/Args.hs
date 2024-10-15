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
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/83> (also #10 and #71).
-- We should support more of the command line arguments of @clang@.
data ClangArgs = ClangArgs {
      -- | Target architecutre
      clangTarget :: Maybe String

      -- | C standard
    , clangCStandard :: Maybe CStandard

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
    C23
  | C17
  | C11
  | C99
  | C89
  deriving stock (Show, Eq)

defaultClangArgs :: ClangArgs
defaultClangArgs = ClangArgs {
      clangTarget    = Nothing
    , clangCStandard = Nothing
    , clangOtherArgs = []
    }

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

fromClangArgs :: ClangArgs -> Either String [String]
fromClangArgs args = aux [
      ifGiven clangTarget $ \target ->
        return ["-target", target]

    , ifGiven clangCStandard $ \cStandard ->
        case cStandard of
          C23 -> -- We can use @-std=c23@ in @clang-18@ or later, but we have no
                 -- reliable way of testing for that.
                 if clangVersion >= Clang9_or_10
                   then return ["-std=c2x"]
                   else throwError "C23 requires clang-9 or later"
          C17 -> if clangVersion >= Clang6
                   then return ["-std=c17"]
                   else throwError "C17 requires clang-6 or later"
          C11 -> if clangVersion == ClangOlderThan3_2
                   then return ["-std=c1x"]
                   else return ["-std=c11"]
          C99 -> return ["-std=c99"]
          C89 -> return ["-std=c89"]

    , return clangOtherArgs
    ]
  where
    ClangArgs{
        clangTarget
      , clangCStandard
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



