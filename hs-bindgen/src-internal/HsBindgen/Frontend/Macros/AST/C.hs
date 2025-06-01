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
  , pattern TypeStruct
  , pattern TypeUnion
  , pattern TypeEnum
  , pattern TypeIncompleteArray
  , pattern TypeConstArray
  ) where

import GHC.Natural (Natural)

import HsBindgen.Frontend.AST.Internal (CName)
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass
import HsBindgen.Frontend.Pass.HandleMacros.IsPass (HandleMacros)
import HsBindgen.Frontend.Pass.Parse.IsPass (DeclId(..))
import HsBindgen.Language.C.Prim

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Type = C.Type HandleMacros

pattern TypePrim :: PrimType -> Type
pattern TypePrim prim = C.TypePrim prim

pattern TypeVoid :: Type
pattern TypeVoid = C.TypeVoid

pattern TypeFun :: [Type] -> Type -> Type
pattern TypeFun args res = C.TypeFun args res

pattern TypePointer :: Type -> Type
pattern TypePointer ty = C.TypePointer ty

pattern TypeTypedef :: CName -> Type
pattern TypeTypedef name = C.TypeTypedef (DeclId name) NoAnn

pattern TypeStruct :: CName -> Type
pattern TypeStruct name = C.TypeStruct (DeclId name)

pattern TypeUnion :: CName -> Type
pattern TypeUnion name = C.TypeUnion (DeclId name)

pattern TypeEnum :: CName -> Type
pattern TypeEnum name = C.TypeEnum (DeclId name)

pattern TypeIncompleteArray :: Type -> Type
pattern TypeIncompleteArray ty = C.TypeIncompleteArray ty

pattern TypeConstArray :: Natural -> Type -> Type
pattern TypeConstArray sz ty = C.TypeConstArray sz ty

{-------------------------------------------------------------------------------
  Internal auxiliary: dealing with names
-------------------------------------------------------------------------------}

pattern DeclId :: CName -> DeclId
pattern DeclId name <- (fromDeclId -> Just name)
  where
    DeclId name = toDeclId name

fromDeclId :: DeclId -> Maybe CName
fromDeclId (DeclNamed name) = Just name
fromDeclId (DeclAnon _)     = Nothing

toDeclId :: CName -> DeclId
toDeclId name = DeclNamed name
