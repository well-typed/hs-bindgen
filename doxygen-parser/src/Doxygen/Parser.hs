-- | Doxygen XML parser for C\/C++ header documentation
--
-- This module provides a high-level interface to invoke the @doxygen@ binary
-- on C\/C++ header files and parse the resulting XML output into a typed
-- Haskell AST.  The @doxygen@ binary must be available on @PATH@ (or
-- configured via 'Config').
--
-- Implementation details are in "Doxygen.Parser.Internal".
--
module Doxygen.Parser (
    -- * Configuration
    Config(..)
  , defaultConfig
    -- * Parsing
  , parse
  , Result(..)
    -- * State type
  , Doxygen -- Opaque
  , emptyDoxygen
    -- * Lookup keys
  , DoxygenKey(..)
  , lookupComment
    -- * Group sections
  , lookupGroupMembership
  , lookupGroupInfo
    -- * Errors
  , DoxygenException(..)
    -- * Comment types (from "Doxygen.Parser.Types")
  , Comment(..)
  , Block(..)
  , Inline(..)
  , Param(..)
  , ParamListKind(..)
  , ParamDirection(..)
  , SimpleSectKind(..)
    -- * Warnings (from "Doxygen.Parser.Warning")
  , Warning(..)
  , Context(..)
  , Degradation(..)
  ) where

import Doxygen.Parser.Internal
import Doxygen.Parser.Types
import Doxygen.Parser.Warning
