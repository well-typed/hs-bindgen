{-# LANGUAGE TemplateHaskellQuotes #-}
module Def (bar) where

import C.Expr.BuildPlatform
import Language.Haskell.TH 

bar :: ExpQ
bar = [| (C.Expr.BuildPlatform.+) |]
