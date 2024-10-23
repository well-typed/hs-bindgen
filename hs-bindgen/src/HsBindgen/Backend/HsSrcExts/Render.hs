-- | Render generated Haskell code
--
-- Intended for qualified import.
--
-- > import HsBindgen.Backend.HsSrcExts.Render (HsRenderOpts(..))
-- > import HsBindgen.Backend.HsSrcExts.Render qualified as Backend.E
module HsBindgen.Backend.HsSrcExts.Render (
    HsRenderOpts(..)
  , render
  , renderIO
  ) where

import Data.Default
import Language.Haskell.Exts (Module)
import Language.Haskell.Exts.Pretty qualified as Pretty
import System.IO

import HsBindgen.Backend.HsSrcExts (Ann)

{-------------------------------------------------------------------------------
  Options

  We abstract away from the options offered by @language-src-exts@.
-------------------------------------------------------------------------------}

-- | Rendering options
data HsRenderOpts = HsRenderOpts {
      hsLineLength :: Int
    }
  deriving stock (Show)

instance Default HsRenderOpts where
  def = HsRenderOpts {
        hsLineLength = 80
      }

{-------------------------------------------------------------------------------
  Rendering
-------------------------------------------------------------------------------}

-- | Pretty-print generated bindings
render :: HsRenderOpts -> Module Ann -> String
render opts = Pretty.prettyPrintStyleMode (toStyle opts) (toMode opts)

-- | Write pretty-printed bindings to the specified file (or @stdout@)
renderIO :: HsRenderOpts -> Maybe FilePath -> Module Ann -> IO ()
renderIO opts Nothing   modl = putStrLn $ render opts modl
renderIO opts (Just fp) modl = withFile fp WriteMode $ \h ->
                                 hPutStrLn h $ render opts modl

{-------------------------------------------------------------------------------
  Internal: translate our options to @haskell-src-exts@

  We construct the records explicitly (rather than using a record update) so
  that we get compilation errors when the record changes, forcing us to make
  an explicit decision about the new situation.
-------------------------------------------------------------------------------}

toStyle :: HsRenderOpts -> Pretty.Style
toStyle opts = Pretty.Style {
      mode           = Pretty.PageMode
    , lineLength     = hsLineLength opts
    , ribbonsPerLine = Pretty.ribbonsPerLine defStyle
    }
  where
    defStyle :: Pretty.Style
    defStyle = Pretty.style

toMode :: HsRenderOpts -> Pretty.PPHsMode
toMode _opts = Pretty.PPHsMode {
      classIndent   = Pretty.classIndent   defMode
    , doIndent      = Pretty.doIndent      defMode
    , multiIfIndent = Pretty.multiIfIndent defMode
    , caseIndent    = Pretty.caseIndent    defMode
    , letIndent     = Pretty.letIndent     defMode
    , whereIndent   = Pretty.whereIndent   defMode
    , onsideIndent  = Pretty.onsideIndent  defMode
    , spacing       = True
    , layout        = Pretty.PPOffsideRule
    , linePragmas   = noLinePragmas
    }
  where
    defMode :: Pretty.PPHsMode
    defMode = Pretty.defaultMode

    -- TODO: <https://github.com/well-typed/hs-bindgen/issues/74>
    -- For now we don't include @LINE@ pragmas.
    noLinePragmas :: Bool
    noLinePragmas = False
