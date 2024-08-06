-- | Generate Haskell bindings from C headers
--
-- This module is intended to be used when using the tool in TH mode.
module HsBindgen (generateBindingsFor) where

import Control.Monad.IO.Class (liftIO)
import Language.Haskell.Meta (toDec)
import Language.Haskell.TH

import HsBindgen.Preprocessor qualified as Preprocessor
import HsBindgen.Spec qualified as Unresolved

-- TODO: <https://github.com/well-typed/hs-bindgen/issues/11>
generateBindingsFor :: Unresolved.Spec -> Q [Dec]
generateBindingsFor spec = do
    spec' <- liftIO $ Unresolved.resolve spec
    return $ map toDec $ Preprocessor.generateDeclarations spec'
