{-# LANGUAGE TemplateHaskell #-}

-- | Utility: templates for class instances
module Test.Bitfields.Unions.TH (
    showD
  , eqD
  , HasSetFun(..)
  , hasSetFunD
  , HasGetFun(..)
  , hasGetFunD
  , HasEqFun(..)
  , hasEqFunD
  ) where

import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import GHC.Records
import GHC.TypeLits
import Language.Haskell.TH qualified as TH

import HsBindgen.Runtime.Prelude

{-------------------------------------------------------------------------------
  Show
-------------------------------------------------------------------------------}

-- | Create an instance for 'Show'
showD :: TH.Name -> TH.Q [TH.Dec]
showD unionName = [d|
      instance (
            KnownSymbol fn
          , HasField fn ($unionNameT fn) (CBitfieldType ($unionNameT fn) fn)
          , Show (CBitfieldType ($unionNameT fn) fn)
          ) => Show ($unionNameT fn) where
        showsPrec d x =
          showParen (d >= 11) $
              showString $unionNameE
            . showString " {"
            . showString fieldName
            . showString " = "
            . showsPrec 0 (getField @fn x)
            . showString "}"
          where
            fieldName = symbolVal (Proxy @fn)
    |]
  where
    unionNameT = [t| $(TH.conT unionName) |]
    unionNameE = [e| $(TH.stringE (TH.nameBase unionName)) |]

{-------------------------------------------------------------------------------
  Eq
-------------------------------------------------------------------------------}

-- | Create an instance for 'Eq'
eqD :: TH.Name -> TH.Q [TH.Dec]
eqD unionName = [d|
    instance (
          HasField fn ($unionNameT fn) (CBitfieldType ($unionNameT fn) fn)
        , Eq (CBitfieldType ($unionNameT fn) fn)
        ) => Eq ($unionNameT fn) where
      x == y = getField @fn x == getField @fn y
    |]
  where
    unionNameT = [t| $(TH.conT unionName) |]

{-------------------------------------------------------------------------------
  HasSetFun
-------------------------------------------------------------------------------}

class HasSetFun u a where
  setFun :: Ptr u -> a -> IO ()

-- | Create an instance for 'HasSetFun'
hasSetFunD :: String -> TH.Name -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
hasSetFunD fieldName unionName fieldType funName  = [d|
      instance HasSetFun $unionTypeT $fieldTypeT where
        setFun ptr = $funNameE (castPtr ptr)
    |]
  where
    unionTypeT = [t| $(TH.conT unionName) $(TH.litT (TH.strTyLit fieldName)) |]
    fieldTypeT = [t| $(TH.conT fieldType) |]
    funNameE   = [e| $(TH.varE funName) |]

{-------------------------------------------------------------------------------
  HasGetFun
-------------------------------------------------------------------------------}

-- | Create an instance for 'HasGetFun'
class HasGetFun u a where
  getFun :: Ptr u -> IO a

hasGetFunD :: String -> TH.Name -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
hasGetFunD fieldName unionName fieldType funName  = [d|
      instance HasGetFun $unionTypeT $fieldTypeT where
        getFun ptr = $funNameE (castPtr ptr)
    |]
  where
    unionTypeT = [t| $(TH.conT unionName) $(TH.litT (TH.strTyLit fieldName)) |]
    fieldTypeT = [t| $(TH.conT fieldType) |]
    funNameE   = [e| $(TH.varE funName) |]

{-------------------------------------------------------------------------------
  HasEqFun
-------------------------------------------------------------------------------}

-- | Create an instance for 'HasEqFun'
class HasEqFun u a where
  eqFun :: Ptr u -> a -> IO CBool

hasEqFunD :: String -> TH.Name -> TH.Name -> TH.Name -> TH.Q [TH.Dec]
hasEqFunD fieldName unionName fieldType funName  = [d|
      instance HasEqFun $unionTypeT $fieldTypeT where
        eqFun ptr = $funNameE (castPtr ptr)
    |]
  where
    unionTypeT = [t| $(TH.conT unionName) $(TH.litT (TH.strTyLit fieldName)) |]
    fieldTypeT = [t| $(TH.conT fieldType) |]
    funNameE   = [e| $(TH.varE funName) |]
