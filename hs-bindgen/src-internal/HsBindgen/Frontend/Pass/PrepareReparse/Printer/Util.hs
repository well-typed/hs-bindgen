-- | Utilities for pretty-printing
--
-- This module is intended to be imported qualified. It is also intended to only
-- be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse" module
-- hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Printer.Util qualified as P
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
    -- * C directives
  , undef
  , include
    -- * C names
  , name
  , declId
  , scopedName
  ) where

import Data.Text qualified as Text
import GHC.Show (showSpace)

import HsBindgen.IR.C qualified as C

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

name :: C.DeclId -> ShowS
name did = showString (Text.unpack did.name.text)

-- | User-facing syntax, which marks an unnamed declaration
declId :: C.DeclId -> ShowS
declId = showString . Text.unpack . C.renderDeclId

scopedName :: C.ScopedName -> ShowS
scopedName n = showString (Text.unpack n.text)
