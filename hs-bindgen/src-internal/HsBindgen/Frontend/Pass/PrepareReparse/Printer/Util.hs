-- | Utilities for pretty-printing
--
-- This module is intended to be imported qualified. It is also intended to only
-- be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse" module
-- hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as Printer
module HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util (
    semicolon
  ) where

semicolon :: ShowS
semicolon = showChar ';'
