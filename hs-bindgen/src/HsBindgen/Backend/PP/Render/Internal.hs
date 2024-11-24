-- | Simple pretty-printing library
--
-- This library wraps the @pretty@ library.  Instead of using 'PP.Doc' directly,
-- type 'CtxDoc' threads a 'Context' that keeps track of the current indentation
-- and maximum line length.  This enables the implementation of 'renderedLines',
-- which can be used to format documentation comments for the current
-- indentation.
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
module HsBindgen.Backend.PP.Render.Internal where
import HsBindgen.Imports
import Text.PrettyPrint.HughesPJ qualified as PP
import HsBindgen.NameHint

{-------------------------------------------------------------------------------
  Context
-------------------------------------------------------------------------------}

-- | Pretty-printing context
data Context = Context {
      -- | Current indentation (number of spaces)
      ctxIndentation :: !Int
      -- | Maximum number of columns per line, when possible
    , ctxMaxLineCols :: !Int

      -- | Name unique
    , ctxNameUnique :: !Int
    }

-- | Construct an initial 'Context' with the specified line length
mkContext :: Int -> Context
mkContext maxLineCols = Context {
      ctxIndentation = 0
    , ctxMaxLineCols = maxLineCols
    , ctxNameUnique = 0
    }

-- | Default pretty-printing context
defaultContext :: Context
defaultContext = mkContext 80

-- | Add to the indentation in a 'Context'
indentContext :: Int -> Context -> Context
indentContext n ctx = ctx {
      ctxIndentation = ctxIndentation ctx + n
    }

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

withFreshName :: NameHint -> (CtxDoc -> CtxDoc) -> CtxDoc
withFreshName (NameHint n) k = CtxDoc $ \ctx -> do
    let i = ctxNameUnique ctx
    let CtxDoc next = k (CtxDoc $ \_ -> PP.text (n ++ show i))
    next ctx { ctxNameUnique = i + 1 }

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

instance Pretty Natural where
  pretty = string . show

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
    aux dL dR = dL PP.$+$ "" PP.$+$ dR

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
