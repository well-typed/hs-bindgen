module HsBindgen.Preprocessor.Render (
    RenderOptions(..)
  , defaultRenderOptions
  , render
  ) where

import Language.Haskell.Exts (Module)
import Language.Haskell.Exts.Pretty qualified as Pretty

import HsBindgen.Annotation (Ann)

{-------------------------------------------------------------------------------
  Options

  We abstract away from the options offered by @language-src-exts@.
-------------------------------------------------------------------------------}

-- | Rendering options
data RenderOptions = RenderOptions {
      renderLineLength :: Int
    }
  deriving stock (Show)

defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions {
      renderLineLength = 80
    }

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Pretty-print generated bindings
render :: RenderOptions -> Module Ann -> String
render opts = Pretty.prettyPrintStyleMode (toStyle opts) (toMode opts)

{-------------------------------------------------------------------------------
  Internal: translate our options to @haskell-src-exts@

  We construct the records explicitly (rather than using a record update) so
  that we get compilation errors when the record changes, forcing us to make
  an explicit decision about the new situation.
-------------------------------------------------------------------------------}

toStyle :: RenderOptions -> Pretty.Style
toStyle opts = Pretty.Style {
      mode           = Pretty.PageMode
    , lineLength     = renderLineLength opts
    , ribbonsPerLine = Pretty.ribbonsPerLine def
    }
  where
    def :: Pretty.Style
    def = Pretty.style

toMode :: RenderOptions -> Pretty.PPHsMode
toMode _opts = Pretty.PPHsMode {
      classIndent   = Pretty.classIndent   def
    , doIndent      = Pretty.doIndent      def
    , multiIfIndent = Pretty.multiIfIndent def
    , caseIndent    = Pretty.caseIndent    def
    , letIndent     = Pretty.letIndent     def
    , whereIndent   = Pretty.whereIndent   def
    , onsideIndent  = Pretty.onsideIndent  def
    , spacing       = True
    , layout        = Pretty.PPOffsideRule
    , linePragmas   = noLinePragmas
    }
  where
    def :: Pretty.PPHsMode
    def = Pretty.defaultMode

    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/74>
    -- For now we don't include @LINE@ pragmas.
    noLinePragmas :: Bool
    noLinePragmas = False
