{-# OPTIONS_GHC -Wno-orphans #-}

module HsBindgen.Clang.Doxygen.Instances () where

import HsBindgen.Clang.Doxygen.Enums
import HsBindgen.Patterns

#include <clang-c/Documentation.h>

{-------------------------------------------------------------------------------
  CXCommentKind
-------------------------------------------------------------------------------}

instance IsSimpleEnum CXCommentKind where
  simpleToC CXComment_Null                 = #const CXComment_Null
  simpleToC CXComment_Text                 = #const CXComment_Text
  simpleToC CXComment_InlineCommand        = #const CXComment_InlineCommand
  simpleToC CXComment_HTMLStartTag         = #const CXComment_HTMLStartTag
  simpleToC CXComment_HTMLEndTag           = #const CXComment_HTMLEndTag
  simpleToC CXComment_Paragraph            = #const CXComment_Paragraph
  simpleToC CXComment_BlockCommand         = #const CXComment_BlockCommand
  simpleToC CXComment_ParamCommand         = #const CXComment_ParamCommand
  simpleToC CXComment_TParamCommand        = #const CXComment_TParamCommand
  simpleToC CXComment_VerbatimBlockCommand = #const CXComment_VerbatimBlockCommand
  simpleToC CXComment_VerbatimBlockLine    = #const CXComment_VerbatimBlockLine
  simpleToC CXComment_VerbatimLine         = #const CXComment_VerbatimLine
  simpleToC CXComment_FullComment          = #const CXComment_FullComment

  simpleFromC (#const CXComment_Null)                 = Just CXComment_Null
  simpleFromC (#const CXComment_Text)                 = Just CXComment_Text
  simpleFromC (#const CXComment_InlineCommand)        = Just CXComment_InlineCommand
  simpleFromC (#const CXComment_HTMLStartTag)         = Just CXComment_HTMLStartTag
  simpleFromC (#const CXComment_HTMLEndTag)           = Just CXComment_HTMLEndTag
  simpleFromC (#const CXComment_Paragraph)            = Just CXComment_Paragraph
  simpleFromC (#const CXComment_BlockCommand)         = Just CXComment_BlockCommand
  simpleFromC (#const CXComment_ParamCommand)         = Just CXComment_ParamCommand
  simpleFromC (#const CXComment_TParamCommand)        = Just CXComment_TParamCommand
  simpleFromC (#const CXComment_VerbatimBlockCommand) = Just CXComment_VerbatimBlockCommand
  simpleFromC (#const CXComment_VerbatimBlockLine)    = Just CXComment_VerbatimBlockLine
  simpleFromC (#const CXComment_VerbatimLine)         = Just CXComment_VerbatimLine
  simpleFromC (#const CXComment_FullComment)          = Just CXComment_FullComment

  simpleFromC _otherwise = Nothing
