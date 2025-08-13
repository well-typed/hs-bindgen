module Clang.HighLevel.Declaration (
    -- * Declaration
    DeclarationClassification(..)
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
--
-- This classification function is suitable for declarations of functions,
-- variables, enums, structs, and unions.
--
-- Forward declarations and redeclarations can be classified as either
-- 'DefinitionElsewhere' or 'DefinitionUnavailable'.
--
-- <https://en.cppreference.com/w/c/language/struct.html#Forward_declaration>
--
-- <https://en.cppreference.com/w/c/language/declarations.html#Redeclaration>
--
-- Despite the name, a tentative definition is /not/ classified as a
-- 'Definition'. Use 'classifyTentativeDefinition' to detect whether a
-- declaration is a tentative definition.
--
-- <https://en.cppreference.com/w/c/language/extern.html#Tentative_definitions>
data DeclarationClassification =
    -- | A declaration together with a definition.
    --
    -- > int foo (void) { return 1; }; // cursor positioned here
    --
    -- <https://en.cppreference.com/w/c/language/declarations.html#Definitions>
    Definition

    -- | A declaration without definition, but the definition is available
    -- elsewhere in the translation unit.
    --
    -- > struct X; // cursor positioned here
    -- > struct X { int n; };
  | DefinitionElsewhere CXCursor

    -- | A declaration without a definition, and there is no definition
    -- available elsewhere in the translation unit.
    --
    -- > extern int x; // cursor positioned here
  | DefinitionUnavailable
  deriving stock (Show, Eq)

-- | Classify a declaration
classifyDeclaration ::
     MonadIO m
  => CXCursor  -- ^ Declaration
  -> m DeclarationClassification
classifyDeclaration cursor = do
    defnCursor <- clang_getCursorDefinition cursor
    isDefnNull <- clang_equalCursors defnCursor nullCursor
    if isDefnNull
      then return DefinitionUnavailable
      else do
        isCursorDefn <- clang_equalCursors cursor defnCursor
        return $
          if isCursorDefn
            then Definition
            else DefinitionElsewhere defnCursor

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
