-- | The parts of the C AST that we require in macro handling
--
-- Intended for qualified import:
--
-- > import HsBindgen.Frontend.Macros.AST.C qualified as C
module HsBindgen.Frontend.Macros.AST.C (
    -- * Types
    Type
  , pattern TypePrim
  , pattern TypeVoid
  , pattern TypeFun
  , pattern TypePointer
  , pattern TypeTypedef
  , pattern TypeMacroTypedef
  , pattern TypeStruct
  , pattern TypeUnion
  , pattern TypeEnum
  , pattern TypeIncompleteArray
  , pattern TypeConstArray
  ) where

import GHC.Natural (Natural)

import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (HandleMacros)
import HsBindgen.Frontend.Pass.Parse.Type.DeclId
import HsBindgen.Language.C qualified as C

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Type = C.Type HandleMacros

pattern TypePrim :: C.PrimType -> Type
pattern TypePrim prim = C.TypePrim prim

pattern TypeVoid :: Type
pattern TypeVoid = C.TypeVoid

pattern TypeFun :: [Type] -> Type -> Type
pattern TypeFun args res = C.TypeFun args res

pattern TypePointer :: Type -> Type
pattern TypePointer ty = C.TypePointer ty

pattern TypeTypedef :: C.Name -> Type
pattern TypeTypedef name = C.TypeTypedef name

pattern TypeMacroTypedef :: C.Name -> Type
pattern TypeMacroTypedef name =
    C.TypeMacroTypedef (DeclId name) C.NameOriginInSource

pattern TypeStruct :: C.Name -> Type
pattern TypeStruct name = C.TypeStruct (DeclId name) C.NameOriginInSource

pattern TypeUnion :: C.Name -> Type
pattern TypeUnion name = C.TypeUnion (DeclId name) C.NameOriginInSource

pattern TypeEnum :: C.Name -> Type
pattern TypeEnum name = C.TypeEnum (DeclId name) C.NameOriginInSource

pattern TypeIncompleteArray :: Type -> Type
pattern TypeIncompleteArray ty = C.TypeIncompleteArray ty

pattern TypeConstArray :: Natural -> Type -> Type
pattern TypeConstArray sz ty = C.TypeConstArray sz ty

{-------------------------------------------------------------------------------
  Internal auxiliary: dealing with names
-------------------------------------------------------------------------------}

pattern DeclId :: C.Name -> DeclId
pattern DeclId name <- (fromDeclId -> Just name)
  where
    DeclId name = toDeclId name

fromDeclId :: DeclId -> Maybe C.Name
fromDeclId (DeclNamed   name) = Just name
fromDeclId (DeclAnon    _   ) = Nothing
fromDeclId (DeclBuiltin name) = Just name

toDeclId :: C.Name -> DeclId
toDeclId name = DeclNamed name
