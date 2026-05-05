-- | Printers for 'PreHeader'
--
-- This module is intended to be imported unqualified. It is also intended to only
-- be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse" module
-- hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Printer
--
module HsBindgen.Frontend.Pass.PrepareReparse.Printer (
    print
  , Print
  ) where

import Prelude hiding (print)

import HsBindgen.Frontend.Pass.PrepareReparse.AST (Decl (..), Include (..),
                                                   MacroName (..),
                                                   PreHeader (include, targets, undefs),
                                                   Tag (..), TagName (..),
                                                   TagType (..), Target (..),
                                                   Undef (..))
import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as Printer

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

print :: Print a => a -> ShowS
print = printIt

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

class Print a where
  printIt :: a -> ShowS

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance Print PreHeader where
  printIt header = Printer.compose [
        printIt header.include
      , printIt header.undefs
      , printIt header.targets
      ]

instance Print Include where
  printIt (Include path) = Printer.compose [
        Printer.include
      , Printer.space
      , showChar '"'
      , showString path
      , showChar '"'
      , Printer.newline
      ]

instance Print [Undef] where
  printIt xs = Printer.compose [
        printIt x . Printer.newline
      | x <- xs
      ]

instance Print Undef where
  printIt (Undef name) =
      Printer.undef . Printer.space . printIt name . Printer.newline

instance Print MacroName where
  printIt (MacroName s) = Printer.string s

instance Print [Target] where
  printIt xs = Printer.compose [
        printIt x . Printer.newline
      | x <- xs
      ]

instance Print Target where
  printIt (Target tag decl) =
    printIt tag . Printer.space . printIt decl . Printer.space . printIt tag

instance Print Decl where
  printIt (Decl code) = showString code

instance Print Tag where
  printIt (Tag typ name) = Printer.compose [
        showString "/*"
      , Printer.space
      , printIt typ
      , Printer.space
      , printIt name
      , Printer.space
      , showString "*/"
      ]

instance Print TagType where
  printIt = \case
      Field -> Printer.string "field"
      Function -> Printer.string "function"
      Typedef -> Printer.string "typedef"
      Variable -> Printer.string "variable"

instance Print TagName where
  printIt (TagName s) = Printer.string s
