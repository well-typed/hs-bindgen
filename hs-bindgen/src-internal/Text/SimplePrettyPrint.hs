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
module Text.SimplePrettyPrint where

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
      ctxIndentation :: !Int
      -- | Maximum number of columns per line, when possible
    , ctxMaxLineCols :: !Int
      -- | Unique name index
    , ctxUniqueNameIdx :: !Int
    }

-- | Construct an initial 'Context' with the specified line length
mkContext :: Int -> Context
mkContext maxLineCols = Context {
      ctxIndentation   = 0
    , ctxMaxLineCols   = maxLineCols
    , ctxUniqueNameIdx = 0
    }

-- | Default pretty-printing context
defaultContext :: Context
defaultContext = mkContext 80

-- | Add to the indentation in a 'Context'
indentContext :: Int -> Context -> Context
indentContext n ctx = ctx {
      ctxIndentation = ctxIndentation ctx + n
    }

-- | Get the next unique name index (and the updated context)
getUniqueNameIdx :: Context -> (Int, Context)
getUniqueNameIdx ctx =
    let i = ctxUniqueNameIdx ctx
    in  (i, ctx { ctxUniqueNameIdx = i + 1 })

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
    in  runCtxDoc ctx' . k $ CtxDoc (\_ -> PP.text (nameHint ++ show i))

-- | Render a 'CtxDoc'
renderCtxDoc :: Context -> CtxDoc -> String
renderCtxDoc ctx@Context{..} = PP.renderStyle style . runCtxDoc ctx
  where
    style :: PP.Style
    style = PP.style {
      PP.lineLength = ctxMaxLineCols
    }

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
showToCtxDoc :: Show a => a -> CtxDoc
showToCtxDoc = string . show

-- | Create a document with the specified 'Text'
textToCtxDoc :: Text.Text -> CtxDoc
textToCtxDoc = string . Text.unpack

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
    ds -> CtxDoc $ \ctx -> foldl1 (PP.$+$) $ map (runCtxDoc ctx) ds

-- | Vertically join two documents, separating by a blank line
infixl 5 $+$
($+$) :: CtxDoc -> CtxDoc -> CtxDoc
dL $+$ dR = CtxDoc $ \ctx ->
    runCtxDoc ctx dL PP.$+$ "" PP.$+$ runCtxDoc ctx dR

-- | Vertically join documents, separating by blank lines
vsep :: [CtxDoc] -> CtxDoc
vsep = \case
    [] -> empty
    ds -> CtxDoc $ \ctx -> foldl1 aux $ map (runCtxDoc ctx) ds
  where
    aux :: PP.Doc -> PP.Doc -> PP.Doc
    aux dL dR
        | PP.isEmpty dL = dR
        | PP.isEmpty dR = dL
        | otherwise     = dL PP.$+$ "" PP.$+$ dR

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

-- | Parenthesize a document horizontally when true
parensWhen :: Bool -> CtxDoc -> CtxDoc
parensWhen = \case
    False -> id
    True  -> parens

-- | Format a list horizontally
hlist :: Char -> Char -> [CtxDoc] -> CtxDoc
hlist cL cR ds =
    hcat $ char cL : (List.intersperse (string ", ") ds) ++ [char cR]

-- | Parenthesize a document vertically
vparens :: CtxDoc -> CtxDoc
vparens d = nest 2 (char '(' <+> d) $$ char ')'

-- | Parenthesize a document vertically when true
vparensWhen :: Bool -> CtxDoc -> CtxDoc
vparensWhen = \case
    False -> id
    True  -> vparens

-- | Format a list vertically (one item per line)
vlist :: Char -> Char -> [CtxDoc] -> CtxDoc
vlist cL cR = \case
    [] -> char cL >< char cR
    (d:ds') -> vcat
      $ (char cL <+> d)
      : [char ',' <+> d' | d' <- ds']
      ++ [char cR]

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
ifFits condD thenD elseD = CtxDoc $ \ctx@Context{..} ->
    -- TODO compute column width, do not just count chars with length
    if length (renderCtxDoc ctx condD) <= ctxMaxLineCols
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
renderedLines f = CtxDoc $ \Context{..} ->
    case f (max 0 (ctxMaxLineCols - ctxIndentation)) of
      [] -> PP.empty
      ss -> foldl1 (PP.$+$) $ map PP.text ss

-- | Create a document with context information, for debugging
debugContext :: CtxDoc
debugContext = CtxDoc $ \Context{..} -> PP.parens $
    PP.hsep ["Context", PP.int ctxIndentation, PP.int ctxMaxLineCols]
