{-# LANGUAGE OverloadedStrings #-}

module HsBindgen.TestTH.Examples (
    MyStruct(..)
  , decls
  ) where

import Data.Int
import Data.Nat
import Data.Vec.Lazy (Vec(..))

import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Util.PHOAS

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data MyStruct = MkMyStruct {
      myStructField1 :: Int32
    , myStructField2 :: Int32
    }

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

decls :: [Hs.Decl f]
decls = [
      Hs.DeclInstance $ Hs.InstanceStorable myStructStorableInstance
    ]

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

myStruct :: Hs.Struct ('S ('S 'Z))
myStruct = Hs.Struct {
      structName   = "MyStruct"
    , structConstr = "MkMyStruct"
    , structFields = ("myStructField1", Hs.HsType "Int") ::: ("myStructField2", Hs.HsType "Char") ::: VNil
    }

myStructStorableInstance :: Hs.WithStruct Hs.StorableInstance f
myStructStorableInstance = Hs.WithStruct myStruct $
    Hs.StorableInstance {
        storableSizeOf    = 8
      , storableAlignment = 8

      , storablePeek = Hs.Lambda $ \ptr ->
          Hs.Ap (Hs.IntroStruct myStruct) [
              Hs.PeekByteOff ptr 0
            , Hs.PeekByteOff ptr 4
            ]
      , storablePoke = Hs.Lambda $ \ptr ->
          Hs.ElimStruct myStruct $ \(f1 ::: f2 ::: VNil) -> Hs.Seq . List $ [
              Hs.PokeByteOff ptr 0 f1
            , Hs.PokeByteOff ptr 4 f2
            ]
      }
