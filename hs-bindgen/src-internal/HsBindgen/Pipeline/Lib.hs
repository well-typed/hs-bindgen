module HsBindgen.Pipeline.Lib (
    -- * Translation pipeline components
    parseCHeaders
  , genHsDecls
  , genSHsDecls
  , genModule
  , genPP
  , genPPString
  , genTH
  , genExtensions

    -- * Preprocessor API
  , translateCHeaders
  , preprocessPure
  , preprocessIO

    -- * Test generation
  , genTests
  ) where

import Language.Haskell.TH qualified as TH

import HsBindgen.Backend.Extensions
import HsBindgen.Backend.PP.Render (HsRenderOpts (..))
import HsBindgen.Backend.PP.Render qualified as Backend.PP
import HsBindgen.Backend.PP.Translation (HsModuleOpts (..))
import HsBindgen.Backend.PP.Translation qualified as Backend.PP
import HsBindgen.Backend.TH.Translation qualified as Backend.TH
import HsBindgen.BindingSpec (ExternalBindingSpec, PrescriptiveBindingSpec)
import HsBindgen.C.Parser qualified as C
import HsBindgen.Config (Config (..))
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.RootHeader
import HsBindgen.GenTests qualified as GenTests
import HsBindgen.Guasi
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Hs.Translation qualified as Hs
import HsBindgen.Imports
import HsBindgen.ModuleUnique
import HsBindgen.SHs.AST qualified as SHs
import HsBindgen.SHs.Simplify (simplifySHs)
import HsBindgen.SHs.Translation qualified as SHs
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Translation pipeline components
-------------------------------------------------------------------------------}

-- | Parse C headers
parseCHeaders ::
      Tracer IO TraceMsg
   -> Config
   -> ExternalBindingSpec
   -> PrescriptiveBindingSpec
   -> [HashIncludeArg]
   -> IO C.TranslationUnit
parseCHeaders = C.parseCHeaders

-- | Generate @Hs@ declarations
genHsDecls :: ModuleUnique -> Config -> [C.Decl] -> [Hs.Decl]
genHsDecls mu Config{..} = Hs.generateDeclarations configTranslation mu

-- | Generate @SHs@ declarations
genSHsDecls :: [Hs.Decl] -> [SHs.SDecl]
genSHsDecls = simplifySHs . SHs.translateDecls

-- | Generate a preprocessor 'Backend.PP.HsModule'
genModule :: Config -> [SHs.SDecl] -> Backend.PP.HsModule
genModule Config{..} = Backend.PP.translateModule configHsModuleOpts

-- | Generate bindings source code, written to a file or @STDOUT@
genPP :: Config -> Maybe FilePath -> Backend.PP.HsModule -> IO ()
genPP Config{..} fp = Backend.PP.renderIO configHsRenderOpts fp

-- | Generate bindings source code
genPPString :: Config -> Backend.PP.HsModule -> String
genPPString Config{..} = Backend.PP.render configHsRenderOpts

-- | Generate Template Haskell declarations
genTH :: Guasi q => [SHs.SDecl] -> q [TH.Dec]
genTH = fmap concat . traverse Backend.TH.mkDecl

-- | Generate set of required extensions
genExtensions :: [SHs.SDecl] -> Set TH.Extension
genExtensions = foldMap requiredExtensions

{-------------------------------------------------------------------------------
  Preprocessor API
-------------------------------------------------------------------------------}

-- | Parse a C header and generate @Hs@ declarations
translateCHeaders
  :: ModuleUnique
  -> Tracer IO TraceMsg
  -> Config
  -> ExternalBindingSpec
  -> PrescriptiveBindingSpec
  -> [HashIncludeArg]
  -> IO [Hs.Decl]
translateCHeaders mu tracer config extSpec pSpec hashIncludeArgs = do
    C.TranslationUnit{unitDecls} <-
      parseCHeaders tracer config extSpec pSpec hashIncludeArgs
    return $ genHsDecls mu config unitDecls

-- | Generate bindings for the given C header
preprocessPure :: Config -> [Hs.Decl] -> String
preprocessPure config = genPPString config . genModule config . genSHsDecls

-- | Generate bindings for the given C header
preprocessIO ::
     Config
  -> Maybe FilePath     -- ^ Output file or 'Nothing' for @STDOUT@
  -> [Hs.Decl]
  -> IO ()
preprocessIO config fp = genPP config fp . genModule config . genSHsDecls

{-------------------------------------------------------------------------------
  Test generation
-------------------------------------------------------------------------------}

-- | Generate tests
genTests :: Config -> [HashIncludeArg] -> FilePath -> [Hs.Decl] -> IO ()
genTests Config{..} hashIncludeArgs testDir decls =
    GenTests.genTests
      hashIncludeArgs
      decls
      (hsModuleOptsName configHsModuleOpts)
      (hsLineLength configHsRenderOpts)
      testDir
