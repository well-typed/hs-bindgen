{-# LANGUAGE OverloadedStrings #-}

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
structDecs struct fields = List [
      Hs.DeclInstance $ Hs.InstanceStorable storable
    ]
  where
    hs :: Hs.Struct n
    hs = Hs.Struct {
          structName   = fromMaybe "X" (C.structTag struct)
        , structConstr = maybe "MkX" ("Mk" <>) (C.structTag struct)
        , structFields = Vec.map C.fieldName fields
        }

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
      Hs.DeclInstance $ Hs.InstanceStorable storable
    ]
  where
    hs :: Hs.Struct (S Z)
    hs = Hs.Struct {
          structName   = fromMaybe "X" (C.enumTag e)
        , structConstr = maybe "MkX" ("Mk" <>) (C.enumTag e)
        , structFields = Vec.singleton (maybe "unX" ("un" <>) (C.enumTag e))
        }

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
