{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HsBindgen.C.Reparse.Decl
  ( TypeName
  , reparseTypeName

  , AttributeSpecifier
  , reparseAttributeSpecifier

  , SizeExpression
  )
  where

-- hs-bindgen
import HsBindgen.C.Reparse.Infra
import HsBindgen.C.Tc.Macro.Type qualified as Macro

--------------------------------------------------------------------------------

data TypeName
instance Eq TypeName
instance Show TypeName

data AttributeSpecifier
instance Eq AttributeSpecifier
instance Show AttributeSpecifier

data SizeExpression
instance Eq SizeExpression
instance Ord SizeExpression
instance Show SizeExpression

reparseTypeName :: Macro.TypeEnv -> Reparse TypeName

reparseAttributeSpecifier :: Reparse AttributeSpecifier
