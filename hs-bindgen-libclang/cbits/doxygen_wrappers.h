#ifndef DOXYGEN_WRAPPERS_H
#define DOXYGEN_WRAPPERS_H

/**
 * Wrappers for the Doxygen API
 *
 * All functions that we use are wrapped.  Prefix `clang_` of the actual
 * function name is replaced with `wrap_`.  This allows us to import the wrapped
 * function via FFI and define the Haskell function with the actual function
 * name within the same module.
 *
 * Wrapper functions of functions that return values of primitive types keep the
 * same API.  Wrapper functions of functions that return values of non-primitive
 * types use a `result` parameter instead.
 *
 * The LLVM codebase capitalizes parameter names.  Wrapper functions keep this
 * convention for such parameters, while `result` is not capitalized.
 */

#include <clang-c/Documentation.h>

/**
 * Top-level
 */

static inline void wrap_Cursor_getParsedComment(const CXCursor* C, CXComment* result) {
    *result = clang_Cursor_getParsedComment(*C);
}

static inline enum CXCommentKind wrap_Comment_getKind(const CXComment* Comment) {
    return clang_Comment_getKind(*Comment);
}

static inline unsigned wrap_Comment_getNumChildren(const CXComment* Comment) {
    return clang_Comment_getNumChildren(*Comment);
}

static inline void wrap_Comment_getChild(const CXComment* Comment, unsigned ChildIdx, CXComment* result) {
    *result = clang_Comment_getChild(*Comment, ChildIdx);
}

static inline unsigned wrap_Comment_isWhitespace(const CXComment* Comment) {
    return clang_Comment_isWhitespace(*Comment);
}

static inline unsigned wrap_InlineContentComment_hasTrailingNewline(const CXComment* Comment) {
    return clang_InlineContentComment_hasTrailingNewline(*Comment);
}

/**
 * Comment type 'CXComment_Text'
 */

static inline void wrap_TextComment_getText(const CXComment* Comment, CXString*  result) {
    *result = clang_TextComment_getText(*Comment);
}

/**
 * Comment type 'CXComment_InlineCommand'
 */

static inline void wrap_InlineCommandComment_getCommandName(const CXComment* Comment, CXString*  result) {
    *result = clang_InlineCommandComment_getCommandName(*Comment);
}

static inline enum CXCommentInlineCommandRenderKind wrap_InlineCommandComment_getRenderKind(const CXComment* Comment) {
    return clang_InlineCommandComment_getRenderKind(*Comment);
}

static inline unsigned wrap_InlineCommandComment_getNumArgs(const CXComment* Comment) {
    return clang_InlineCommandComment_getNumArgs(*Comment);
}

static inline void wrap_InlineCommandComment_getArgText(const CXComment* Comment, unsigned ArgIdx, CXString* result) {
    *result = clang_InlineCommandComment_getArgText(*Comment, ArgIdx);
}

/**
 * Comment type 'CXComment_HTMLStartTag' and 'CXComment_HTMLEndTag'
 */

static inline void wrap_HTMLTagComment_getTagName(const CXComment* Comment, CXString*  result) {
    *result = clang_HTMLTagComment_getTagName(*Comment);
}

static inline unsigned wrap_HTMLStartTagComment_isSelfClosing(const CXComment* Comment) {
    return clang_HTMLStartTagComment_isSelfClosing(*Comment);
}

static inline unsigned wrap_HTMLStartTag_getNumAttrs(const CXComment* Comment) {
    return clang_HTMLStartTag_getNumAttrs(*Comment);
}

static inline void wrap_HTMLStartTag_getAttrName(const CXComment* Comment, unsigned AttrIdx, CXString*  result) {
    *result = clang_HTMLStartTag_getAttrName(*Comment, AttrIdx);
}

static inline void wrap_HTMLStartTag_getAttrValue(const CXComment* Comment, unsigned AttrIdx, CXString*  result) {
    *result = clang_HTMLStartTag_getAttrValue(*Comment, AttrIdx);
}

static inline void wrap_HTMLTagComment_getAsString(const CXComment* Comment, CXString*  result) {
    *result = clang_HTMLTagComment_getAsString(*Comment);
}

/**
 * Comment type 'CXComment_BlockCommand'
 */

static inline void wrap_BlockCommandComment_getCommandName(const CXComment* Comment, CXString*  result) {
    *result = clang_BlockCommandComment_getCommandName(*Comment);
}

static inline unsigned wrap_BlockCommandComment_getNumArgs(const CXComment* Comment) {
    return clang_BlockCommandComment_getNumArgs(*Comment);
}

static inline void wrap_BlockCommandComment_getArgText(const CXComment* Comment, unsigned ArgIdx, CXString*  result) {
    *result = clang_BlockCommandComment_getArgText(*Comment, ArgIdx);
}

static inline void wrap_BlockCommandComment_getParagraph(const CXComment* Comment, CXComment* result) {
    *result = clang_BlockCommandComment_getParagraph(*Comment);
}

/**
 * Comment type 'CXComment_ParamCommand'
 */

static inline void wrap_ParamCommandComment_getParamName(const CXComment* Comment, CXString*  result) {
    *result = clang_ParamCommandComment_getParamName(*Comment);
}

static inline unsigned wrap_ParamCommandComment_isParamIndexValid(const CXComment* Comment) {
    return clang_ParamCommandComment_isParamIndexValid(*Comment);
}

static inline unsigned wrap_ParamCommandComment_getParamIndex(const CXComment* Comment) {
    return clang_ParamCommandComment_getParamIndex(*Comment);
}

static inline unsigned wrap_ParamCommandComment_isDirectionExplicit(const CXComment* Comment) {
    return clang_ParamCommandComment_isDirectionExplicit(*Comment);
}

static inline enum CXCommentParamPassDirection wrap_ParamCommandComment_getDirection(const CXComment* Comment) {
    return clang_ParamCommandComment_getDirection(*Comment);
}

/**
 * Comment type 'CXComment_TParamCommand'
 */

static inline void wrap_TParamCommandComment_getParamName(const CXComment* Comment, CXString*  result) {
    *result = clang_TParamCommandComment_getParamName(*Comment);
}

static inline unsigned wrap_TParamCommandComment_isParamPositionValid(const CXComment* Comment) {
    return clang_TParamCommandComment_isParamPositionValid(*Comment);
}

static inline unsigned wrap_TParamCommandComment_getDepth(const CXComment* Comment) {
    return clang_TParamCommandComment_getDepth(*Comment);
}

static inline unsigned wrap_TParamCommandComment_getIndex(const CXComment* Comment, unsigned Depth) {
    return clang_TParamCommandComment_getIndex(*Comment, Depth);
}

/**
 * Comment type 'CXComment_VerbatimBlockLine'
 */

static inline void wrap_VerbatimBlockLineComment_getText(const CXComment* Comment, CXString*  result) {
    *result = clang_VerbatimBlockLineComment_getText(*Comment);
}

/**
 * Comment type 'CXComment_VerbatimLine'
 */

static inline void wrap_VerbatimLineComment_getText(const CXComment* Comment, CXString*  result) {
    *result = clang_VerbatimLineComment_getText(*Comment);
}

/**
 * Comment type 'CXComment_FullComment'
 */

static inline void wrap_FullComment_getAsHTML(const CXComment* Comment, CXString*  result) {
    *result = clang_FullComment_getAsHTML(*Comment);
}

static inline void wrap_FullComment_getAsXML(const CXComment* Comment, CXString*  result) {
    *result = clang_FullComment_getAsXML(*Comment);
}

#endif