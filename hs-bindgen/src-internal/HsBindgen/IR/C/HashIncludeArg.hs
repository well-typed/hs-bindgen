-- | @#include@ argument
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.HashIncludeArg qualified as C
module HsBindgen.IR.C.HashIncludeArg (
    -- * HashIncludeArg
    HashIncludeArg(..)
  , hashIncludeArg
  , UncheckedHashIncludeArg
  , hashIncludeArgWithTrace
    -- ** Trace message
  , HashIncludeArgMsg(..)
  ) where

import System.FilePath qualified as FilePath
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  HashIncludeArg
-------------------------------------------------------------------------------}

-- | @#include@ argument
--
-- We only support bracket includes, using @#include <PATH>@ syntax.  This type
-- represents the @PATH@ to a header.
--
-- A @#include@ argument is generally relative to a directory in the C include
-- search path.  We issue a 'Notice' if an absolute path is used, because it is
-- almost always a mistake.
--
-- A @#include@ argument is C syntax.  Forward slashes (@/@) are used to
-- separate directories, even on Windows.  Backslashes are interpreted as
-- characters in a directory or filename, not directory separators.  We issue a
-- 'Notice' if a backslash is used, because it is almost always a mistake.
newtype HashIncludeArg = HashIncludeArg { path :: FilePath }
  deriving stock (Show)
  deriving newtype (Eq, IsString, Ord)

-- | Construct a t'HashIncludeArg', returning trace messages
hashIncludeArg :: FilePath -> ([HashIncludeArgMsg], HashIncludeArg)
hashIncludeArg fp = (hashIncludeArgMsgs fp, HashIncludeArg fp)

-- | Unchecked @#include@ argument
--
-- We need to emit trace messages monadically, so we do not check values within
-- the pure parser.
type UncheckedHashIncludeArg = FilePath

-- | Construct a t'HashIncludeArg', emitting trace messages
hashIncludeArgWithTrace ::
     Tracer HashIncludeArgMsg
  -> UncheckedHashIncludeArg
  -> IO HashIncludeArg
hashIncludeArgWithTrace tracer fp = do
    let (msgs, arg) = hashIncludeArg fp
    mapM_ (traceWith tracer . withCallStack) msgs
    return arg

{-------------------------------------------------------------------------------
  Trace messages
-------------------------------------------------------------------------------}

-- | @#include@ argument trace message
data HashIncludeArgMsg =
    HashIncludeArgBackslash   FilePath
  | HashIncludeArgNotRelative FilePath
  deriving stock (Show)

instance PrettyForTrace HashIncludeArgMsg where
  prettyForTrace = \case
    HashIncludeArgBackslash arg ->
      PP.string $ "#include argument contains a backslash: " ++ arg
    HashIncludeArgNotRelative arg ->
      PP.string $ "#include argument not relative: " ++ arg

instance IsTrace Level HashIncludeArgMsg where
  getDefaultLogLevel = const Notice
  getSource          = const HsBindgen
  getTraceId         = const "hash-include-arg"

hashIncludeArgMsgs :: FilePath -> [HashIncludeArgMsg]
hashIncludeArgMsgs fp = catMaybes [
      if '\\' `elem` fp
        then Just (HashIncludeArgBackslash fp)
        else Nothing
    , if FilePath.isRelative fp
        then Nothing
        else Just (HashIncludeArgNotRelative fp)
    ]
