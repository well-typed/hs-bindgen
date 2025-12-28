{-# LANGUAGE CPP #-}

-- | Simple pretty-printing library
--
-- This library wraps the @pretty@ library.  Instead of using 'PP.Doc' directly,
-- type 'CtxDoc' threads a 'Context'.  Keeping track of the current indentation
-- and maximum line length enables the implementation of 'renderedLines', which
-- can be used to format documentation comments for the current indentation.
-- Keeping track of a unique name index enables rendering of unique names with
-- 'withFreshName'.
--
-- Some of the API functions provided by this library differ in behavior from
-- @pretty@ API functions of similar/same names.  In particular, '($+$)'
-- vertically joins documents with blank lines, while '($$)' vertically joins
-- documents without blank lines, behaving like the @pretty@ '(PP.$+$)'
-- function.  The list equivalents of these functions are similarly different.
--
-- The underlying @pretty@ library gives very little control over indentation.
-- If we would like to have better indentation, we should either switch to a
-- different underlying library or write our own.
--
-- Intended for qualified import
--
-- > import Text.SimplePrettyPrint (Pretty(..), CtxDoc)
-- > import Text.SimplePrettyPrint qualified as PP
--
-- There are also operators that can be imported, though you may prefer to use
-- 'hsep' or 'hcat' instead of '(><)', 'vsep' or 'vcat' instead of '($$)', etc.

-- > import Text.SimplePrettyPrint ((><), (<+>), ($$), ($+$))
module Text.SimplePrettyPrint (
    CtxDoc -- opaque
    -- * 'CtxDoc' features
  , withFreshName
  , ifFits
    -- * Rendering
  , runCtxDoc
  , renderCtxDoc
    -- ** Context
  , Context -- opaque
  , mkContext
  , defaultContext
  , debugContext
    -- * Pretty class
  , Pretty(..)
  , renderPretty
    -- * Construction
    -- ** Primitives
  , empty
  , char
  , string
  , show
  , text
  , renderedLines
    -- ** Horizontal and vertical composition
  , (><)
  , hcat
  , (<+>)
  , hsep
  , ($$)
  , vcat
  , ($+$)
  , vsep
  , cat
  , fcat
  , sep
  , fsep
    -- ** Bracketing
  , parens
  , parensWhen
  , vparensWhen
  , singleQuotes
    -- ** Lists
  , hlist
  , vlist
    -- ** Indentation
  , nest
  , hang
  , hangs
  , hangs'
  ) where

import Prelude hiding (show)
import Prelude qualified

import Data.List qualified as List
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Text.PrettyPrint.HughesPJ qualified as PP

#if MIN_VERSION_base(4,19,0)
import GHC.TypeError (ErrorMessage (..), Unsatisfiable)
#endif

{-------------------------------------------------------------------------------
  Context
-------------------------------------------------------------------------------}

-- | Pretty-printing context
data Context = Context {
      -- | Current indentation (number of spaces)
      indentation :: !Int
      -- | Maximum number of columns per line, when possible
    , maxLineCols :: !Int
      -- | Unique name index
    , uniqueNameIdx :: !Int
    }

-- | Construct an initial 'Context' with the specified line length
mkContext :: Int -> Context
mkContext maxLineCols = Context {
      indentation   = 0
    , maxLineCols   = maxLineCols
    , uniqueNameIdx = 0
    }

-- | Default pretty-printing context
defaultContext :: Context
defaultContext = mkContext 80

-- | Add to the indentation in a 'Context'
indentContext :: Int -> Context -> Context
indentContext n ctx = ctx{indentation = ctx.indentation + n}

-- | Get the next unique name index (and the updated context)
getUniqueNameIdx :: Context -> (Int, Context)
getUniqueNameIdx ctx =
    let i = ctx.uniqueNameIdx
    in  (i, ctx{uniqueNameIdx = i + 1})

{-------------------------------------------------------------------------------
  CtxDoc
-------------------------------------------------------------------------------}

-- | Contextualized document
newtype CtxDoc = CtxDoc (Context -> PP.Doc)

-- | Create a document with the specified string
instance IsString CtxDoc where
  fromString = string

-- | Not valid Haskell syntax, may be used for debugging purposes
instance Show CtxDoc where
  show = renderCtxDoc defaultContext

-- | Run a 'CtxDoc' with the specified context
runCtxDoc :: Context -> CtxDoc -> PP.Doc
runCtxDoc ctx (CtxDoc f) = f ctx

-- | Create a unique name with the specified hint
withFreshName :: String -> (CtxDoc -> CtxDoc) -> CtxDoc
withFreshName nameHint k = CtxDoc $ \ctx ->
    let (i, ctx') = getUniqueNameIdx ctx
    in  runCtxDoc ctx' . k $ CtxDoc (\_ -> PP.text (nameHint ++ Prelude.show i))

-- | Render a 'CtxDoc'
renderCtxDoc :: Context -> CtxDoc -> String
renderCtxDoc ctx = PP.renderStyle style . runCtxDoc ctx
  where
    style :: PP.Style
    style = PP.style {PP.lineLength = ctx.maxLineCols}

{-------------------------------------------------------------------------------
  Pretty
-------------------------------------------------------------------------------}

-- | Types that can be pretty-printed
class Pretty a where
  -- | Create a document for the specified value with @0@ precedence
  pretty :: a -> CtxDoc
  pretty = prettyPrec 0

  -- | Create a document for the specified value with the specified precedence
  prettyPrec :: Int -> a -> CtxDoc
  prettyPrec _prec = pretty

  {-# MINIMAL pretty | prettyPrec #-}

#if MIN_VERSION_base(4,19,0)
instance Unsatisfiable (Text "Don't pretty Strings, use fromString") => Pretty [a]
#endif

-- | Render a 'Pretty' value
renderPretty :: Pretty a => Context -> a -> String
renderPretty ctx = renderCtxDoc ctx . pretty

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | The empty document
empty :: CtxDoc
empty = CtxDoc $ const PP.empty

-- | Create a document with the specified character
char :: Char -> CtxDoc
char = CtxDoc . const . PP.char

-- | Create a document with the specified string
string :: String -> CtxDoc
string = CtxDoc . const . PP.text

-- | Create a document with the result of 'show'
show :: Show a => a -> CtxDoc
show = string . Prelude.show

-- | Create a document with the specified 'Text'
text :: Text.Text -> CtxDoc
text = string . Text.unpack

-- | Horizontally join two documents
--
-- Note that 'Semigroup' '(<>)' is /not/ used because it is right-associative.
infixl 6 ><
(><) :: CtxDoc -> CtxDoc -> CtxDoc
dL >< dR = CtxDoc $ \ctx ->
    runCtxDoc ctx dL PP.<> runCtxDoc ctx dR

-- | Horizontally join documents
hcat :: [CtxDoc] -> CtxDoc
hcat ds = CtxDoc $ \ctx ->
    PP.hcat $ map (runCtxDoc ctx) ds

-- | Horizontally join two documents, separating by space
infixl 6 <+>
(<+>) :: CtxDoc -> CtxDoc -> CtxDoc
dL <+> dR = CtxDoc $ \ctx ->
    runCtxDoc ctx dL PP.<+> runCtxDoc ctx dR

-- | Horizontally join documents, separating by space
hsep :: [CtxDoc] -> CtxDoc
hsep ds = CtxDoc $ \ctx ->
    PP.hsep $ map (runCtxDoc ctx) ds

-- | Vertically join two documents
infixl 5 $$
($$) :: CtxDoc -> CtxDoc -> CtxDoc
dL $$ dR = CtxDoc $ \ctx ->
    runCtxDoc ctx dL PP.$+$ runCtxDoc ctx dR

-- | Vertically join documents
vcat :: [CtxDoc] -> CtxDoc
vcat = \case
    [] -> empty
    ds -> CtxDoc $ \ctx -> foldl1 aux $ map (runCtxDoc ctx) ds
  where
    -- If there are empty spaces, aka newlines we won't create extra
    -- unneeded whitespaces
    aux :: PP.Doc -> PP.Doc -> PP.Doc
    aux dL dR
        | dL == "" && dR == "" = PP.nest (-100) dL PP.$+$ PP.nest (-100) dR
        | dL == ""             = PP.nest (-100) dL PP.$+$ dR
        | dR == ""             = dL PP.$+$ PP.nest (-100) dR
        | otherwise            = dL PP.$+$ dR

-- | Vertically join two documents, separating by a blank line
infixl 5 $+$
($+$) :: CtxDoc -> CtxDoc -> CtxDoc
dL $+$ dR = CtxDoc $ \ctx ->
    runCtxDoc ctx dL PP.$+$ PP.nest (-100) "" PP.$+$ runCtxDoc ctx dR

-- | Vertically join documents, separating by blank lines
--
-- If there the context is nested the new line won't create unneded empty
-- spaces.
vsep :: [CtxDoc] -> CtxDoc
vsep = \case
    [] -> empty
    ds -> CtxDoc $ \ctx -> foldl1 aux $ map (runCtxDoc ctx) ds
  where
    aux :: PP.Doc -> PP.Doc -> PP.Doc
    aux dL dR
        | PP.isEmpty dL = dR
        | PP.isEmpty dR = dL
        | otherwise     = dL PP.$+$ PP.nest (-100) "" PP.$+$ dR

-- | Horizontally /or/ vertically join documents, depending on if there is
-- room on the line
cat :: [CtxDoc] -> CtxDoc
cat ds = CtxDoc $ \ctx -> PP.cat $ map (runCtxDoc ctx) ds

-- | Horizontally /or/ vertically join documents, depending on if there is
-- room on the line, \"paragraph fill\" version
fcat :: [CtxDoc] -> CtxDoc
fcat ds = CtxDoc $ \ctx -> PP.fcat $ map (runCtxDoc ctx) ds

-- | Horizontally /or/ vertically join documents, depending on if there is
-- room on the line, horizontally separating by space
sep :: [CtxDoc] -> CtxDoc
sep ds = CtxDoc $ \ctx -> PP.sep $ map (runCtxDoc ctx) ds

-- | Horizontally /or/ vertically join documents, depending on if there is
-- room on the line, horizontally separating by space, \"paragraph fill\"
-- version
fsep :: [CtxDoc] -> CtxDoc
fsep ds = CtxDoc $ \ctx -> PP.fsep $ map (runCtxDoc ctx) ds

-- | Parenthesize a document horizontally
parens :: CtxDoc -> CtxDoc
parens d = hcat [char '(', d, char ')']

-- | Surround a document with single quotes horizontally
singleQuotes :: CtxDoc -> CtxDoc
singleQuotes d = hcat [char '\'', d, char '\'']

-- | Parenthesize a document horizontally when true
parensWhen :: Bool -> CtxDoc -> CtxDoc
parensWhen = \case
    False -> id
    True  -> parens

-- | Format a list horizontally
hlist :: String -> String -> [CtxDoc] -> CtxDoc
hlist cL cR ds =
    hcat $ string cL : (List.intersperse (string ", ") ds) ++ [string cR]

-- | Parenthesize a document vertically
vparens :: CtxDoc -> CtxDoc
vparens d = nest 2 (char '(' <+> d) $$ char ')'

-- | Parenthesize a document vertically when true
vparensWhen :: Bool -> CtxDoc -> CtxDoc
vparensWhen = \case
    False -> id
    True  -> vparens

-- | Format a list vertically (one item per line)
vlist :: String -> String -> [CtxDoc] -> CtxDoc
vlist cL cR = \case
    [] -> string cL >< string cR
    (d:ds') -> vcat
      $ (string cL <+> d)
      : [char ',' <+> d' | d' <- ds']
      ++ [string cR]

-- | Nest/indent a document by the specified number of spaces
nest :: Int -> CtxDoc -> CtxDoc
nest n d = CtxDoc $ \ctx ->
    PP.nest n $ runCtxDoc (indentContext n ctx) d

-- | Hang a document below another with the specified indentation
hang :: CtxDoc -> Int -> CtxDoc -> CtxDoc
hang dA n dB = dA $$ nest n dB

-- | Hang multiple documents below another with the specified indentation,
-- separated by blank lines
hangs :: CtxDoc -> Int -> [CtxDoc] -> CtxDoc
hangs dA n dBs = dA $$ vsep (nest n <$> dBs)

-- | Hang multiple documents below another with the specified indentation,
-- do __not__ seprate by blank lines
hangs' :: CtxDoc -> Int -> [CtxDoc] -> CtxDoc
hangs' dA n dBs = dA $$ vcat (nest n <$> dBs)

-- | Select a 'CtxDoc' depending on if a rendered 'CtxDoc' fits within the
-- maximum line length
ifFits :: CtxDoc -> CtxDoc -> CtxDoc -> CtxDoc
ifFits condD thenD elseD = CtxDoc $ \ctx ->
    -- TODO compute column width, do not just count chars with length
    if length (renderCtxDoc ctx condD) <= ctx.maxLineCols
      then runCtxDoc ctx thenD
      else runCtxDoc ctx elseD

-- | Create a document with rendered lines
--
-- The function is passed the maximum width of each line, computed as the
-- maximum line length minus the current indentation.  It must return a list
-- of lines, using an empty string to represent a blank line.  None of the
-- strings may contain newline characters.
--
-- The lines are /automatically/ indented according to the current indentation.
renderedLines :: (Int -> [String]) -> CtxDoc
renderedLines f = CtxDoc $ \ctx ->
    case f (max 0 (ctx.maxLineCols - ctx.indentation)) of
      [] -> PP.empty
      ss -> foldl1 (PP.$+$) $ map PP.text ss

-- | Create a document with context information, for debugging
debugContext :: CtxDoc
debugContext = CtxDoc $ \ctx -> PP.parens $
    PP.hsep ["Context", PP.int ctx.indentation, PP.int ctx.maxLineCols]
