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
import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as P

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
  printIt header = P.compose [
        printIt header.include
      , printIt header.undefs
      , printIt header.targets
      ]

instance Print Include where
  printIt (Include path) = P.compose [
        P.include
      , P.space
      , showChar '"'
      , showString path
      , showChar '"'
      , P.newline
      ]

instance Print [Undef] where
  printIt xs = P.compose [
        printIt x . P.newline
      | x <- xs
      ]

instance Print Undef where
  printIt (Undef name) =
      P.undef . P.space . printIt name . P.newline

instance Print MacroName where
  printIt (MacroName s) = P.string s

instance Print [Target] where
  printIt xs = P.compose [
        printIt x . P.newline
      | x <- xs
      ]

instance Print Target where
  printIt (Target tag decl) =
    printIt tag . P.space . printIt decl . P.space . printIt tag

instance Print Decl where
  printIt (Decl code) = showString code

instance Print Tag where
  printIt (Tag typ name) = P.compose [
        showString "/*"
      , P.space
      , printIt typ
      , P.space
      , printIt name
      , P.space
      , showString "*/"
      ]

instance Print TagType where
  printIt = \case
      Field -> P.string "field"
      Function -> P.string "function"
      Typedef -> P.string "typedef"
      Variable -> P.string "variable"

instance Print TagName where
  printIt (TagName s) = P.string s
