{-# LANGUAGE CPP #-}

module HsBindgen.Guasi (
    Guasi (..)
    -- * State
  , GuasiState(..)
  , emptyGuasiState
  ) where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Text.SimplePrettyPrint (pretty)

import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.HsModule.Pretty.Comment (CommentKind (..))
import HsBindgen.Config (FieldNamingStrategy (..))
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

-- | An intermediate class between 'TH.Quote' and 'TH.Quasi'
-- which doesn't provide reification functionality of 'TH.Quasi',
-- but has a bit more than 'TH.Quote'.
class TH.Quote g => Guasi g where
    getGuasi :: g GuasiState
    putGuasi :: GuasiState -> g ()
    modifyGuasi :: (GuasiState -> GuasiState) -> g ()
    modifyGuasi f = getGuasi >>= putGuasi . f

    addCSource :: String -> g ()
    addDependentFile :: FilePath -> g ()

    extsEnabled :: g [TH.Extension]

    -- | Look up a name in the type namespace of the environment.
    --
    -- Returns 'Nothing' if the name is not in scope.
    lookupTypeName :: String -> g (Maybe TH.Name)

    reportError :: String -> g ()

    -- | Attach a documentation to a declaration with provided name /in the
    --   current module/.
    --
    -- Ideally we'd use @withDecDoc@ instead, but this gets confused by existing
    -- functions in scope <https://gitlab.haskell.org/ghc/ghc/-/issues/26817>.
    putLocalDoc :: Hs.SingNamespace ns => Hs.Name ns -> HsDoc.Comment -> g ()
    -- | Attach a documentation to a field of a parent declaration /in the
    --   current module/.
    --
    -- We handle documentation for fields in a special way because with
    -- duplicate record fields, Template Haskell gets confused, reporting
    -- "ambiguous occurrence" errors.
    putLocalFieldDoc ::
      FieldNamingStrategy -> Hs.Name Hs.NsConstr -> Hs.Name Hs.NsVar -> HsDoc.Comment -> g ()

data GuasiState = GuasiState {
      missingModules :: Set Hs.ModuleName
    }
    deriving (Eq, Show, Generic)

emptyGuasiState :: GuasiState
emptyGuasiState = GuasiState { missingModules = Set.empty }

instance Guasi TH.Q where
    getGuasi = fromMaybe emptyGuasiState <$> TH.getQ
    putGuasi = TH.putQ

    addCSource = TH.addForeignSource TH.LangC
    addDependentFile = TH.addDependentFile

    extsEnabled = TH.extsEnabled

    lookupTypeName = TH.lookupTypeName

    reportError = TH.reportError

    putLocalDoc :: forall ns. Hs.SingNamespace ns => Hs.Name ns -> HsDoc.Comment -> TH.Q ()
    putLocalDoc nm comment = do
        loc <- TH.location
        let pkg = TH.PkgName $ TH.loc_package loc
            mdl = TH.ModName $ TH.loc_module loc
            globalName :: TH.Name
            globalName = TH.Name
              (TH.OccName nmStr)
              (TH.NameG thNs pkg mdl)
        TH.addModFinalizer $
          TH.putDoc (TH.DeclDoc globalName) (show $ pretty $ THComment comment)
      where
        nmStr :: String
        nmStr = Text.unpack $ Hs.getName nm

        thNs :: TH.NameSpace
        thNs = case Hs.namespaceOf (Hs.singNamespace :: Hs.SNamespace ns) of
          Hs.NsVar        -> TH.VarName
          Hs.NsConstr     -> TH.DataName
          Hs.NsTypeConstr -> TH.TcClsName

#if __GLASGOW_HASKELL__ >=908
    -- Here we have to go out of the way. We need to rigurously disambiguate
    -- field names when attaching documentation to fields without prefixes
    -- (`--omit-field-prefixes`). However, we can only do so with
    -- `template-haskell >= 2.21`, shipping with GHC 9.8.
    putLocalFieldDoc _fns parent field comment = do
        loc <- TH.location
        let pkg = TH.PkgName $ TH.loc_package loc
            mdl = TH.ModName $ TH.loc_module loc
            globalName :: TH.Name
            globalName = TH.Name
              (TH.OccName fieldStr)
              (TH.NameG thNs pkg mdl)
        TH.addModFinalizer $
          TH.putDoc (TH.DeclDoc globalName) (show $ pretty $ THComment comment)
      where
        fieldStr, parentStr :: String
        fieldStr  = Text.unpack $ Hs.getName field
        parentStr = Text.unpack $ Hs.getName parent

        thNs :: TH.NameSpace
        thNs = TH.FldName parentStr
#else
    -- For older versions of GHC, we only provide field documentation when
    -- fields are prefixed and have unique names.
    putLocalFieldDoc fns _parent field comment =
        case fns of
          OmitFieldPrefixes -> pure ()
          AddFieldPrefixes  -> putLocalDoc field comment
#endif
