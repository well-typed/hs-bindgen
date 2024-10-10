-- | Definition of the source location types
--
-- Intended for unqualified import (unlike "HsBindgen.Clang.Util.SourceLoc").
-- We introduce this split so that these type can be exported (unqualified)
-- from "HsBindgen.C.AST".
module HsBindgen.Clang.Util.SourceLoc.Type (
    -- * Definition
    SourcePath(..)
  , SingleLoc(..)
  , MultiLoc(..)
  , Range(..)
    -- * Comparisons
  , compareSingleLoc
  , rangeContainsLoc
  ) where

import Control.Monad
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Paths as reported by @libclang@
--
-- Clang uses UTF-8 internally for everything, including paths, which is why
-- this is 'Text', not @OsPath@. There might still be differences between
-- platforms of course (such as directory separators).
newtype SourcePath = SourcePath {
      getSourcePath :: Text
    }
  deriving newtype (Eq, Ord)

-- | A /single/ location in a file
--
-- See 'MultiLoc' for additional discussion.
data SingleLoc = SingleLoc {
      singleLocPath   :: !SourcePath
    , singleLocLine   :: !Int
    , singleLocColumn :: !Int
    }
  deriving stock (Eq, Ord, Generic)

-- | Multiple related source locations
--
-- 'Core.CXSourceLocation' in @libclang@ corresponds to @SourceLocation@ in
-- @clang@, which can actually correspond to /multiple/ source locations in a
-- file; for example, in a header file such as
--
-- > #define M1 int
-- >
-- > struct ExampleStruct {
-- >   M1 m1;
-- >   ^
-- > };
--
-- then the source location at the caret (@^@) has an \"expansion location\",
-- which is the position at the caret, and a \"spelling location\", which
-- corresponds to the location of the @int@ token in the macro definition.
--
-- References:
--
-- * <https://clang.llvm.org/doxygen/classclang_1_1SourceLocation.html>
-- * <https://clang.llvm.org/doxygen/classclang_1_1SourceManager.html>
--   (@getExpansionLoc@, @getSpellingLoc@, @getDecomposedSpellingLoc@)
data MultiLoc = MultiLoc {
      -- | Expansion location
      --
      -- If the location refers into a macro expansion, this corresponds to the
      -- location of the macro expansion.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gadee4bea0fa34550663e869f48550eb1f>
      multiLocExpansion :: !SingleLoc

      -- | Presumed location
      --
      -- The given source location as specified in a @#line@ directive.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga03508d9c944feeb3877515a1b08d36f9>
    , multiLocPresumed :: !(Maybe SingleLoc)

      -- | Spelling location
      --
      -- If the location refers into a macro instantiation, this corresponds to
      -- the /original/ location of the spelling in the source file.
      --
      -- /WARNING/: This field is only populated correctly from @llvm >= 191.0@;
      -- prior to that this is equal to 'multiLocFile'.
      -- See <https://github.com/llvm/llvm-project/pull/72400>.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#ga01f1a342f7807ea742aedd2c61c46fa0>
    , multiLocSpelling :: !(Maybe SingleLoc)

      -- | File location
      --
      -- If the location refers into a macro expansion, this corresponds to the
      -- location of the macro expansion.
      -- If the location points at a macro argument, this corresponds to the
      -- location of the use of the argument.
      --
      -- See <https://clang.llvm.org/doxygen/group__CINDEX__LOCATIONS.html#gae0ee9ff0ea04f2446832fc12a7fd2ac8>
    , multiLocFile :: !(Maybe SingleLoc)
    }
  deriving stock (Eq, Ord, Generic)

-- | Range
--
-- 'Core.CXSourceRange' corresponds to @SourceRange@ in @clang@
-- <https://clang.llvm.org/doxygen/classclang_1_1SourceLocation.html>,
-- and therefore to @Range MultiLoc@; see 'MultiLoc' for additional discussion.
data Range a = Range {
      rangeStart :: !a
    , rangeEnd   :: !a
    }
  deriving stock (Eq, Ord, Generic)
  deriving stock (Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  Comparisons
-------------------------------------------------------------------------------}

-- | Compare locations
--
-- Returns 'Nothing' if the locations aren't in the same file.
compareSingleLoc :: SingleLoc -> SingleLoc -> Maybe Ordering
compareSingleLoc a b = do
    guard $ singleLocPath a == singleLocPath b
    return $
      compare
        (singleLocLine a, singleLocColumn a)
        (singleLocLine b, singleLocColumn b)

-- | Check if a location falls within the given range
--
-- Treats the range as half-open, with an inclusive lower bound and exclusive
-- upper bound (following 'CXSourceRange').
--
-- Returns 'Nothing' if the three locations are not all in the same file.
rangeContainsLoc :: Range SingleLoc -> SingleLoc -> Maybe Bool
rangeContainsLoc Range{rangeStart, rangeEnd} loc = do
    afterStart <- (/= LT) <$> compareSingleLoc loc rangeStart
    beforeEnd  <- (== LT) <$> compareSingleLoc loc rangeEnd
    return $ afterStart && beforeEnd

{-------------------------------------------------------------------------------
  Show instances

  Technically speaking the validity of these instances depends on 'IsString'
  instances which we do not (yet?) define.
-------------------------------------------------------------------------------}

instance Show SourcePath        where show = show . getSourcePath
instance Show SingleLoc         where show = show . prettySingleLoc True
instance Show MultiLoc          where show = show . prettyMultiLoc  True
instance Show (Range SingleLoc) where show = show . prettyRangeSingleLoc
instance Show (Range MultiLoc)  where show = show . prettyRangeMultiLoc

deriving stock instance {-# OVERLAPPABLE #-} Show a => Show (Range a)

{-------------------------------------------------------------------------------
  Pretty-printing

  These instances mimick the behaviour of @SourceLocation::print@ and
  @SourceRange::print@ in @clang@.
-------------------------------------------------------------------------------}

type ShowFile = Bool

prettySingleLoc :: ShowFile -> SingleLoc -> String
prettySingleLoc showFile loc =
    intercalate ":" . concat $ [
        [ Text.unpack (getSourcePath singleLocPath) | showFile ]
      , [ show singleLocLine
        , show singleLocColumn
        ]
      ]
  where
    SingleLoc{singleLocPath, singleLocLine, singleLocColumn} = loc

prettyMultiLoc :: ShowFile -> MultiLoc -> String
prettyMultiLoc showFile multiLoc =
    intercalate " " . concat $ [
        [ prettySingleLoc showFile multiLocExpansion ]
      , [ "<Presumed=" ++ aux loc ++ ">" | Just loc <- [multiLocPresumed] ]
      , [ "<Spelling=" ++ aux loc ++ ">" | Just loc <- [multiLocSpelling] ]
      , [ "<File="     ++ aux loc ++ ">" | Just loc <- [multiLocFile]     ]
      ]
  where
    MultiLoc{
        multiLocExpansion
      , multiLocPresumed
      , multiLocSpelling
      , multiLocFile} = multiLoc

    aux :: SingleLoc -> [Char]
    aux loc =
        prettySingleLoc
          (singleLocPath loc /= singleLocPath multiLocExpansion)
          loc

prettyRangeSingleLoc :: Range SingleLoc -> String
prettyRangeSingleLoc = prettySourceRangeWith
      singleLocPath
      prettySingleLoc

prettyRangeMultiLoc :: Range MultiLoc -> String
prettyRangeMultiLoc =
    prettySourceRangeWith
      (singleLocPath . multiLocExpansion)
      prettyMultiLoc

prettySourceRangeWith ::
     (a -> SourcePath)
  -> (ShowFile -> a -> String)
  -> Range a -> String
prettySourceRangeWith path pretty Range{rangeStart, rangeEnd} = concat [
      "<"
    , pretty True rangeStart
    , "-"
    , pretty (path rangeStart /= path rangeEnd) rangeEnd
    , ">"
    ]

{-------------------------------------------------------------------------------
  PrettyVal instances

  These just piggy-back on the 'Show' instances.
-------------------------------------------------------------------------------}

instance PrettyVal SourcePath   where prettyVal = prettyVal . show
instance PrettyVal SingleLoc where prettyVal = prettyVal . show
instance PrettyVal MultiLoc  where prettyVal = prettyVal . show

instance Show a => PrettyVal (Range a) where
  prettyVal = prettyVal. show
