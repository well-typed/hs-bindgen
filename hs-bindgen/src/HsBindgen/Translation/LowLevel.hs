{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Low-level translation of the C header to a Haskell module
--
-- TODO: This module is intended to implement the following milestones:
--
-- * Milestone 1: @Storable@ instances
--   <https://github.com/well-typed/hs-bindgen/milestone/2>
-- * Milestone 2: Low-level API
--   <https://github.com/well-typed/hs-bindgen/milestone/3>
module HsBindgen.Translation.LowLevel (generateDeclarations) where

import Data.Foldable
import Data.Kind
import Data.Maybe
import Data.Type.Nat
import Data.Vec.Lazy (Vec)
import Data.Vec.Lazy qualified as Vec

import HsBindgen.C.AST qualified as C
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Util.PHOAS
import HsBindgen.Hs.AST.Name
import HsBindgen.Hs.AST.Type

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

generateDeclarations :: C.Header -> [Hs.Decl f]
generateDeclarations = getList . toHs

{-------------------------------------------------------------------------------
  Translation
-------------------------------------------------------------------------------}

class ToHs (a :: Type) where
  type InHs a :: PHOAS
  toHs :: a -> InHs a f

instance ToHs C.Header where
  type InHs C.Header = List Hs.Decl
  toHs (C.Header decs) = List $ concatMap getList (map toHs decs)

instance ToHs C.Decl where
  type InHs C.Decl = List Hs.Decl
  toHs (C.DeclStruct struct) = reifyStructFields struct $ structDecs struct
  toHs (C.DeclEnum e)        = enumDecs e
  toHs _otherwise = List [] -- TODO

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

reifyStructFields ::
     C.Struct
  -> (forall n. SNatI n => Vec n C.StructField -> a)
  -> a
reifyStructFields struct k = Vec.reifyList (C.structFields struct) k

-- | Generate declarations for given C struct
--
-- This is just a first sketch so far.
--
-- TODO:
--
-- * We currently generate only the 'Storable' instance. We should also
--   generate the @data@ declaration.
-- * Name mangling
-- * Deal with untagged structs.
-- * ..
structDecs :: forall n f.
     SNatI n
  => C.Struct -> Vec n C.StructField -> List Hs.Decl f
structDecs struct fields = List
    [ Hs.DeclData $ Hs.WithStruct hs Hs.MkDataDecl
    , Hs.DeclInstance $ Hs.InstanceStorable storable
    ]
  where
    hs :: Hs.Struct n
    hs =
      let cStructName = fromMaybe "X" $ C.structTag struct
          nm@NameMangler{..} = defaultNameMangler
          typeConstrCtx = TypeConstrContext cStructName
          structName = mangleTypeConstrName typeConstrCtx
          structConstr = mangleConstrName $ ConstrContext typeConstrCtx
          mkField f =
            ( mangleVarName $ FieldVarContext typeConstrCtx True (C.fieldName f)
            , typ nm (C.fieldType f)
            )
          structFields = Vec.map mkField fields
      in  Hs.Struct{..}

    storable :: Hs.WithStruct Hs.StorableInstance f
    storable = Hs.WithStruct hs $ Hs.StorableInstance {
          Hs.storableSizeOf    = C.structSizeof struct
        , Hs.storableAlignment = C.structAlignment struct
        , Hs.storablePeek      = Hs.Lambda $ \ptr ->
                                  Hs.Ap (Hs.IntroStruct hs) $
                                    map (peek ptr) (C.structFields struct)
        , Hs.storablePoke      = Hs.Lambda $ \ptr ->
                                   Hs.ElimStruct hs $ \xs -> Hs.Seq . List $
                                     toList $ Vec.zipWith (poke ptr) fields xs
        }

    peek :: f Bound -> C.StructField -> Hs.PeekByteOff f
    peek ptr f = Hs.PeekByteOff ptr (C.fieldOffset f)

    poke :: f Bound -> C.StructField -> f Bound -> Hs.PokeByteOff f
    poke ptr f i = Hs.PokeByteOff ptr (C.fieldOffset f) i

{-------------------------------------------------------------------------------
  Enum
-------------------------------------------------------------------------------}

enumDecs :: forall f.
  C.Enu -> List Hs.Decl f
enumDecs e = List [
      Hs.DeclNewtype newtype_
    , Hs.DeclInstance $ Hs.InstanceStorable storable
    ]
  where
    newtype_ :: Hs.Newtype
    newtype_ =
      let cEnumName = fromMaybe "X" $ C.enumTag e
          NameMangler{..} = defaultNameMangler
          typeConstrCtx = TypeConstrContext cEnumName
          newtypeName = mangleTypeConstrName typeConstrCtx
          newtypeConstr = mangleConstrName $ ConstrContext typeConstrCtx
          newtypeField = mangleVarName $ EnumVarContext typeConstrCtx
          newtypeType = Hs.HsType "EnumTypeTODO"
      in Hs.Newtype {..}

    hs :: Hs.Struct (S Z)
    hs =
      let cEnumName = fromMaybe "X" $ C.enumTag e
          NameMangler{..} = defaultNameMangler
          typeConstrCtx = TypeConstrContext cEnumName
          structName = mangleTypeConstrName typeConstrCtx
          structConstr = mangleConstrName $ ConstrContext typeConstrCtx
          structFields = Vec.singleton
            ( mangleVarName $ EnumVarContext typeConstrCtx
            , Hs.HsType "EnumTypeTODO"
            )
      in  Hs.Struct{..}

    storable :: Hs.WithStruct Hs.StorableInstance f
    storable = Hs.WithStruct hs $ Hs.StorableInstance {
          Hs.storableSizeOf    = C.enumSizeof e
        , Hs.storableAlignment = C.enumAlignment e
        , Hs.storablePeek      = Hs.Lambda $ \ptr ->
                                  Hs.Ap (Hs.IntroStruct hs) $
                                    [ peek ptr ]
        , Hs.storablePoke      = Hs.Lambda $ \ptr ->
                                   Hs.ElimStruct hs $ \xs -> Hs.Seq . List $
                                     [ poke ptr (Vec.head xs) ]
        }

    peek :: f Bound -> Hs.PeekByteOff f
    peek ptr = Hs.PeekByteOff ptr 0

    poke :: f Bound -> f Bound -> Hs.PokeByteOff f
    poke ptr i = Hs.PokeByteOff ptr 0 i

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

typ :: NameMangler -> C.Typ -> Hs.HsType
typ NameMangler{..} (C.TypElaborated c) =
  Hs.HsTypRef (mangleTypeConstrName (TypeConstrContext c)) -- wrong
typ _     (C.TypStruct s)      =Hs.HsType (show (C.structTag s)) -- also wrong
typ _     (C.TypPrim p)       = case p of
  C.PrimVoid                   -> Hs.HsPrimType HsPrimVoid
  C.PrimChar Nothing           -> Hs.HsPrimType HsPrimCChar
  C.PrimChar (Just C.Signed)   -> Hs.HsPrimType HsPrimCSChar
  C.PrimChar (Just C.Unsigned) -> Hs.HsPrimType HsPrimCSChar
  C.PrimIntegral i ->
    case i of
      C.PrimInt C.Signed       -> Hs.HsPrimType HsPrimCInt
      C.PrimInt C.Unsigned     -> Hs.HsPrimType HsPrimCUInt
      C.PrimShort C.Signed     -> Hs.HsPrimType HsPrimCShort
      C.PrimShort C.Unsigned   -> Hs.HsPrimType HsPrimCUShort
      C.PrimLong C.Signed      -> Hs.HsPrimType HsPrimCLong
      C.PrimLong C.Unsigned    -> Hs.HsPrimType HsPrimCULong
      C.PrimLongLong C.Signed  -> Hs.HsPrimType HsPrimCLLong
      C.PrimLongLong C.Unsigned-> Hs.HsPrimType HsPrimCULLong
  C.PrimFloating f ->
    case f of
      C.PrimFloat              -> Hs.HsPrimType HsPrimCFloat
      C.PrimDouble             -> Hs.HsPrimType HsPrimCDouble
      C.PrimLongDouble         -> Hs.HsPrimType HsPrimCDouble -- wrong (see #247)
typ nm (C.TypPointer t)    = Hs.HsPtr (typ nm t)
