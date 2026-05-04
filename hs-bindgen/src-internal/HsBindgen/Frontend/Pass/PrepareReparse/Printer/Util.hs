-- | Utilities for pretty-printing
--
-- This module is intended to be imported qualified. It is also intended to only
-- be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse" module
-- hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as Printer
--
module HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util (
    -- * Combinators
    compose
  , string
    -- * Whitespace
  , space
  , newline
    -- * Interpunction
  , semicolon
  , dot
    -- * C directives
  , undef
  , include
    -- * C names
  , name
  , fieldName
  ) where

import Data.Text qualified as Text
import GHC.Show (showSpace)

import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming (CDeclName (text), CScopedName (text),
                                  DeclId (name))
import HsBindgen.Frontend.Pass.TypecheckMacros.IsPass (TypecheckMacros)

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

compose :: [ShowS] -> ShowS
compose xs = foldr (.) id xs

string :: String -> ShowS
string = showString

{-------------------------------------------------------------------------------
  Whitespace
-------------------------------------------------------------------------------}

space :: ShowS
space = showSpace

newline :: ShowS
newline = showChar '\n'

{-------------------------------------------------------------------------------
  Interpunction
-------------------------------------------------------------------------------}

semicolon :: ShowS
semicolon = showChar ';'

dot :: ShowS
dot = showChar '.'

{-------------------------------------------------------------------------------
  C directives
-------------------------------------------------------------------------------}

undef :: ShowS
undef = showString "#undef"

include :: ShowS
include = showString "#include"

{-------------------------------------------------------------------------------
  C names
-------------------------------------------------------------------------------}

name :: C.DeclInfo TypecheckMacros -> ShowS
name info = showString (Text.unpack info.id.name.text)

fieldName :: C.FieldInfo TypecheckMacros -> ShowS
fieldName info = showString (Text.unpack info.name.text)
