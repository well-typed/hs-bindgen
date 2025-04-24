{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsBindgen.C.Reparse.Decl
  ( TypeName
  , reparseTypeName
  , typeNameType

  , AttributeSpecifier
  , reparseAttributeSpecifier
  )
  where

-- hs-bindgen
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Tc.Macro.Type qualified as Macro
import HsBindgen.C.AST.Type qualified as C (Type)

--------------------------------------------------------------------------------

data TypeName
instance Eq TypeName
instance Show TypeName

data AttributeSpecifier
instance Eq AttributeSpecifier
instance Show AttributeSpecifier

reparseTypeName :: Macro.TypeEnv -> Reparse TypeName
typeNameType :: TypeName -> Either String C.Type

reparseAttributeSpecifier :: Reparse AttributeSpecifier
