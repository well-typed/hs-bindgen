module Clang.HighLevel.Declaration (
    -- * Declaration
    Declaration(..)
  , classifyDeclaration
  ) where

import Control.Monad.IO.Class

import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  Declaration
-------------------------------------------------------------------------------}

-- | Declaration classification
data Declaration =
    -- | Declaration and definition together
    DeclarationRegular

    -- | Forward declaration (definition elsewhere in the translation unit)
  | DeclarationForward CXCursor

    -- | Opaque declaration (definition not available in the translation unit)
  | DeclarationOpaque

-- | Classify a declaration
classifyDeclaration ::
     MonadIO m
  => CXCursor  -- ^ Declaration
  -> m Declaration
classifyDeclaration cursor = do
    defnCursor <- clang_getCursorDefinition cursor
    isDefnNull <- clang_equalCursors defnCursor nullCursor
    if isDefnNull
      then return DeclarationOpaque
      else do
        isCursorDefn <- clang_equalCursors cursor defnCursor
        return $
          if isCursorDefn
            then DeclarationRegular
            else DeclarationForward defnCursor
