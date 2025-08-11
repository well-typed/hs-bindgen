module Clang.HighLevel.Declaration (
    -- * Declaration
    Declaration(..)
  , classifyDeclaration
    -- * Other
  , classifyTentativeDefinition
  ) where

import Control.Monad.IO.Class

import Clang.Enum.Simple
import Clang.LowLevel.Core

{-------------------------------------------------------------------------------
  Declaration
-------------------------------------------------------------------------------}

-- | Declaration classification
data Declaration =
    -- | Declaration and definition together
    DeclarationRegular

    -- | Forward declaration (definition elsewhere in the translation unit)
    --
    -- TODO: can also be a redeclaration (backwards declaration)
  | DeclarationForward CXCursor

    -- | Opaque declaration (definition not available in the translation unit)
  | DeclarationOpaque
  deriving stock (Show, Eq)

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

{-------------------------------------------------------------------------------
  Other
-------------------------------------------------------------------------------}

-- | Classify whether a declaration of a global variable is a tentative
-- definition.
--
-- NOTE: this function assumes that the cursor points to a global variable
-- declaration.
--
-- A tentative definition is an external declaration without an initializer,
-- and either without a storage-class specifier or with the specifier static.
--
-- A tentative definition is a declaration that may or may not act as a
-- definition. If an actual external definition is found earlier or later in the
-- same translation unit, then the tentative definition just acts as a
-- declaration.
--
-- <https://en.cppreference.com/w/c/language/extern.html#Tentative_definitions>
classifyTentativeDefinition ::
     MonadIO m
  => CXCursor
  -> m Bool
classifyTentativeDefinition cursor = do
    initrCursor <- clang_Cursor_getVarDeclInitializer cursor
    isInitrNull <- clang_equalCursors initrCursor nullCursor
    if isInitrNull
      then do
        storage <- clang_Cursor_getStorageClass cursor
        case fromSimpleEnum storage of
          Right CX_SC_Static -> pure True
          Right CX_SC_None -> pure True
          _ -> pure False
      else pure False
