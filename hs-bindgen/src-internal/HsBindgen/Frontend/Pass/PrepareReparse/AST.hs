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
  , TagName (..)
  ) where

import Prelude hiding (print)

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
-- >        Target (Tag Function (TagName "myFunction")) (Decl "void myFunction (A x);")
-- >      , Target Tag Field (TagName "MyStruct.myStructField") (Decl "B myStructField;")
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
-- >        Target (Tag Function (TagName "myFunction")) (Decl "void myFunction (A x);")
-- >      , Target Tag Field (TagName "MyStruct.myStructField") (Decl "void myStructField;")
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

-- | A tag records the origins of a reparser target: the kind of declaration,
-- and a unique name for the declaration
--
-- Tags are used in the printer to signal the start and end of a reparse target,
-- which makes it easier to parse the printer output later. Moreover, the tag
-- name is used to assign unique tag names to fields, because field names are
-- not unique in general.
--
-- Examples:
--
-- > Tag Function (TagName "myFunction")
-- > Tag Field (TagName "MyStruct.myStructField")
--
data Tag = Tag TagType TagName
  deriving stock (Show, Eq, Ord)

-- | Reparse targets can be declarations of these kinds:
data TagType =
    -- | A struct or union field
    Field
  | Function
  | Typedef
    -- | A global variable
  | Variable
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | A unique name for a reparse target
--
-- This is generally just the name of the declaration itself, but for fields we
-- pair the name of the field with the name of the enclosing struct or union.
-- Field names are not unique in /general/. They are only unique within a single
-- struct or union.
--
-- Examples:
--
-- > TagName "myFunction"
-- > TagName "MyStruct.myStructField"
--
newtype TagName = TagName String
  deriving stock (Show, Eq, Ord)
