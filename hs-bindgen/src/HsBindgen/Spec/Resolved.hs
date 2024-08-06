-- | Resolved spec
--
-- "HsBindgen.Spec" 'HsBindgen.Spec.Spec' defines a specification, but executing
-- it requires @IO@: reading header files, interacting with @libclang@, etc.
-- The generation of the Haskell module(s) however is morally pure, and we'd
-- like to keep it that way: the /resolved/ spec enables this.
--
-- Unlike "HsBindgen.Spec", this is /not/ part of the library's public API.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Spec.Resolved (Spec(..))
-- > import HsBindgen.Spec.Resolved qualified as Spec
--
-- Where there is ambiguity, the module should be qualified as @Resolved@.
module HsBindgen.Spec.Resolved (
    Spec(..)
  ) where

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Spec = Spec {
      -- | Input C header
      -- TODO: <https://github.com/well-typed/hs-bindgen/issues/20>
      --
      -- This should be the output of parsing the C header using @libclang@.
      specCHeader :: String

      -- | Name of the generated Haskell module
    , specHsModuleName :: String
    }
