module HsBindgen.C.Clang.Args (
    ClangArgs
  ) where

-- | @libclang@ command line arguments
--
-- TODO: <https://github.com/well-typed/hs-bindgen/issues/83>
-- We should have a proper data type instead of @[String]@ for the arguments
-- (part of #10 and #71).
type ClangArgs = [String]

