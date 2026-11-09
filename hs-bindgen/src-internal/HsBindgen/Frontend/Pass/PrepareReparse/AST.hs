-- | A simplified intermediate AST for a header file containing only
-- declarations that are intended to be run through the reparser
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.AST
--
module HsBindgen.Frontend.Pass.PrepareReparse.AST (
    -- * Before preprocessing
    PreHeader (..)
  , Include (..)
  , Undef (..)
  , MacroName (..)
    -- * After preprocessing
  , PostHeader (..)
    -- * Common
  , Target (..)
  , Decl (..)
  , Tag (..)
  , TagType (..)
  ) where

import Prelude hiding (print)

import HsBindgen.IR.C qualified as C

{-------------------------------------------------------------------------------
  Before preprocessing
-------------------------------------------------------------------------------}

-- | A header file for reparse targets before macro prepocessing
--
-- The goal of the @PrepareReparse@ pass is to expand macro invocations in
-- reparse targets that would make the reparser fail. In general, all macro
-- definitions that were not parsed as types have to be expanded. To achieve
-- this expanding of only select macro definitions, we include the original
-- header file, we undefine all macro definitions that were parsed as types, and
-- then we include all reparse targets. This 'PreHeader' can then be printed to
-- a file, and that file can preprocessed using the @clang@ executable.
--
-- Example:
--
-- > PreHeader {
-- >    include = Include "foo/bar/baz.h"
-- >  , undefs = [ Undef (MacroName "A") ]
-- >  , targets = [
-- >        Target (Tag Function "myFunction") (Decl "void myFunction (A x);")
-- >      , Target (Tag (Field "myStructField") "struct MyStruct") (Decl "B myStructField;")
-- >      ]
-- >  }
--
-- In the above example, @A@ was parsed as a type, and @B@ was not.
data PreHeader = PreHeader {
    include :: Include
  , undefs :: [Undef]
  , targets :: [Target]
  }

-- | An include directive
newtype Include = Include FilePath
  deriving stock (Show, Eq)

-- | A macro undefinition
data Undef = Undef MacroName
  deriving stock (Show, Eq)

-- | A macro name
newtype MacroName = MacroName String
  deriving stock (Show, Eq)

{-------------------------------------------------------------------------------
  After preprocessing
-------------------------------------------------------------------------------}

-- | A header file for reparse targets after macro prepocessing
--
-- After running the @clang@ executable on a printed 'PreHeader' (and cutting
-- out all code from the included original header file), we are left with a file
-- that contains all reparse targets but with all the relevant macro invocations
-- expanded. These textual reparse targets can then be parsed as a 'PostHeader'.
--
-- Example:
--
-- > PostHeader {
-- >    targets = [
-- >        Target (Tag Function "myFunction") (Decl "void myFunction (A x);")
-- >      , Target (Tag (Field "myStructField") "struct MyStruct") (Decl "void myStructField;")
-- >      ]
-- >  }
--
-- In the above example, @A@ was parsed as a type, and @B@ was not so it was
-- expanded.
data PostHeader = PostHeader {
    targets :: [Target]
  }

{-------------------------------------------------------------------------------
  Common
-------------------------------------------------------------------------------}

-- | A /reparse/ target is a declaration that should be run through the reparser
data Target = Target Tag Decl
  deriving stock (Show, Eq)

-- | The raw text of a declaration
newtype Decl = Decl String
  deriving stock (Show, Eq)

-- | A tag records the origin of a reparse target: the kind of target, and the
-- declaration it belongs to
--
-- Tags are used in the printer to signal the start and end of a reparse target,
-- which makes it easier to parse the printer output later. The preprocessed
-- output is mapped back to its declaration by tag, so distinct targets must
-- have distinct tags. A 'C.DeclId' distinguishes a declaration from an unnamed
-- one with the same name, and a field from its namesakes in other structs or
-- unions.
--
-- Examples, writing a 'C.DeclId' in its user-facing syntax, and their printed
-- form:
--
-- > Tag Function "myFunction"                       -- /* function myFunction */
-- > Tag (Field "myStructField") "struct MyStruct"   -- /* field myStructField struct MyStruct */
-- > Tag (Field "x") "struct @a_b"                   -- /* field x struct @a_b */
--
data Tag = Tag TagType C.DeclId
  deriving stock (Show, Eq, Ord)

-- | Reparse targets can be declarations of these kinds:
data TagType =
    -- | A field of the struct or union
    Field C.ScopedName
  | Function
  | Typedef
    -- | A global variable
  | Variable
  deriving stock (Show, Eq, Ord)
