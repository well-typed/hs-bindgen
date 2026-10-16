{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal

data Point2d

instance Marshal.StaticSize Point2d

data Value

instance Marshal.StaticSize Value

data Color

instance Marshal.StaticSize Color

data TdStruct

instance Marshal.StaticSize TdStruct

data TdChain

instance Marshal.StaticSize TdChain

data TdUnion

instance Marshal.StaticSize TdUnion

data Counter

data OpaqueFwd

data MacroPtr
