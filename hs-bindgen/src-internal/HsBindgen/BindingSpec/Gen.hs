-- | Binding specification generation
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Gen qualified as BindingSpec
module HsBindgen.BindingSpec.Gen (
    -- * Public API
    genBindingSpec

    -- * Internal API
  , genBindingSpecYaml
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as HsOrigin
import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.V1 (UnresolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Config.ClangArgs qualified as ClangArgs
import HsBindgen.Errors
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Generate binding specification
--
-- The format is determined by filename extension.  The following formats are
-- supported:
--
-- * YAML (@.yaml@ extension)
-- * JSON (@.json@ extension)
genBindingSpec ::
     ClangArgs.Target
  -> Hs.ModuleName
  -> FilePath
  -> GetMainHeaders
  -> [(C.QualName, SourcePath)]
  -> [Hs.Decl]
  -> IO ()
genBindingSpec target hsModuleName path getMainHeaders omitTypes =
      BindingSpec.writeFile path
    . genBindingSpec' target hsModuleName getMainHeaders omitTypes

{-------------------------------------------------------------------------------
  Internal API (for tests)
-------------------------------------------------------------------------------}

-- | Generate binding specification
genBindingSpecYaml ::
     ClangArgs.Target
  -> Hs.ModuleName
  -> GetMainHeaders
  -> [(C.QualName, SourcePath)]
  -> [Hs.Decl]
  -> ByteString
genBindingSpecYaml target hsModuleName getMainHeaders omitTypes =
      BindingSpec.encodeYaml
    . genBindingSpec' target hsModuleName getMainHeaders omitTypes

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- TODO aliases
genBindingSpec' ::
     ClangArgs.Target
  -> Hs.ModuleName
  -> GetMainHeaders
  -> [(C.QualName, SourcePath)]
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec'
    target
    hsModuleName
    getMainHeaders
    omitTypes = foldr aux omitSpec
  where
    omitSpec :: UnresolvedBindingSpec
    omitSpec = BindingSpec.BindingSpec {
        -- TODO AnyTarget if bindings are not target-specific
        BindingSpec.bindingSpecTarget = BindingSpec.SpecificTarget target
      , BindingSpec.bindingSpecModule = hsModuleName
      , BindingSpec.bindingSpecCTypes = Map.fromListWith (++) [
            (cQualName, [(getMainHeaders' path, Omit)])
          | (cQualName, path) <- omitTypes
          ]
      , BindingSpec.bindingSpecHsTypes = Map.empty
      }

    getMainHeaders' :: SourcePath -> Set HashIncludeArg
    getMainHeaders' =
        either
          (\s -> panicPure ("auxStruct: getMainHeaders: " ++ s))
          (Set.fromList . NonEmpty.toList)
      . getMainHeaders

    aux ::
         Hs.Decl
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    aux = \case
      Hs.DeclData struct      -> insertType $ auxStruct    struct
      Hs.DeclEmpty edata      -> insertType $ auxEmptyData edata
      Hs.DeclNewtype ntype    -> insertType $ auxNewtype   ntype
      Hs.DeclPatSyn{}         -> id
      Hs.DeclDefineInstance{} -> id
      Hs.DeclDeriveInstance{} -> id
      Hs.DeclForeignImport{}  -> id
      Hs.DeclFunction{}       -> id
      Hs.DeclMacroExpr{}      -> id
      Hs.DeclUnionGetter{}    -> id
      Hs.DeclUnionSetter{}    -> id
      Hs.DeclSimple{}         -> id

    insertType ::
         ( (C.QualName, Set HashIncludeArg, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    insertType ((cQualName, headers, cTypeSpec), (hsId, hsTypeSpec)) spec =
      spec {
          BindingSpec.bindingSpecCTypes =
            Map.insertWith (++) cQualName [(headers, Require cTypeSpec)] $
              BindingSpec.bindingSpecCTypes spec
        , BindingSpec.bindingSpecHsTypes =
            Map.insert hsId hsTypeSpec $ BindingSpec.bindingSpecHsTypes spec
        }

    auxStruct ::
         Hs.Struct n
      -> ( (C.QualName, Set HashIncludeArg, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
    auxStruct hsStruct = case Hs.structOrigin hsStruct of
      Nothing -> panicPure "auxStruct: structOrigin is Nothing"
      Just originDecl ->
        let declInfo = HsOrigin.declInfo originDecl
            cQualName = getCQualName declInfo $
              case HsOrigin.declKind originDecl of
                HsOrigin.Struct{} -> C.NameKindTagged C.TagKindStruct
            hsIdentifier = Hs.Identifier $ Hs.getName (Hs.structName hsStruct)
            cTypeSpec = BindingSpec.CTypeSpec {
                cTypeSpecIdentifier = Just hsIdentifier
              }
            hsTypeSpec = BindingSpec.HsTypeSpec {
                hsTypeSpecInstances =
                  mkInstSpecs
                    ( maybe Map.empty BindingSpec.hsTypeSpecInstances
                    . C.declSpecHs
                    $ HsOrigin.declSpec originDecl
                    )
                    (Hs.structInstances hsStruct)
              }
        in  ( (cQualName, getHeaders declInfo, cTypeSpec)
            , (hsIdentifier, hsTypeSpec)
            )

    auxEmptyData ::
         Hs.EmptyData
      -> ( (C.QualName, Set HashIncludeArg, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
    auxEmptyData edata =
      let originDecl = Hs.emptyDataOrigin edata
          declInfo = HsOrigin.declInfo originDecl
          cQualName = getCQualName declInfo $
            case HsOrigin.declKind originDecl of
              HsOrigin.Opaque cNameKind -> cNameKind
          hsIdentifier = Hs.Identifier $ Hs.getName (Hs.emptyDataName edata)
          cTypeSpec = BindingSpec.CTypeSpec {
              cTypeSpecIdentifier = Just hsIdentifier
            }
          hsTypeSpec = def
      in  ( (cQualName, getHeaders declInfo, cTypeSpec)
          , (hsIdentifier, hsTypeSpec)
          )

    auxNewtype ::
         Hs.Newtype
      -> ( (C.QualName, Set HashIncludeArg, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
    auxNewtype hsNewtype =
      let originDecl = Hs.newtypeOrigin hsNewtype
          declInfo = HsOrigin.declInfo originDecl
          cQualName = getCQualName declInfo $
            case HsOrigin.declKind originDecl of
              HsOrigin.Enum{}    -> C.NameKindTagged C.TagKindEnum
              HsOrigin.Typedef{} -> C.NameKindOrdinary
              HsOrigin.Union{}   -> C.NameKindTagged C.TagKindUnion
              HsOrigin.Macro{}   -> C.NameKindOrdinary
          hsIdentifier = Hs.Identifier $ Hs.getName (Hs.newtypeName hsNewtype)
          cTypeSpec = BindingSpec.CTypeSpec {
              cTypeSpecIdentifier = Just hsIdentifier
            }
          hsTypeSpec = BindingSpec.HsTypeSpec {
              hsTypeSpecInstances =
                mkInstSpecs
                  ( maybe Map.empty BindingSpec.hsTypeSpecInstances
                  . C.declSpecHs
                  $ HsOrigin.declSpec originDecl
                  )
                  (Hs.newtypeInstances hsNewtype)
            }
      in  ( (cQualName, getHeaders declInfo, cTypeSpec)
          , (hsIdentifier, hsTypeSpec)
          )

    getHeaders :: C.DeclInfo -> Set HashIncludeArg
    getHeaders = getMainHeaders' . singleLocPath . C.declLoc

getCQualName :: C.DeclInfo -> C.NameKind -> C.QualName
getCQualName declInfo cNameKind = case C.declOrigin declInfo of
    C.NameOriginInSource -> C.QualName cName cNameKind
    C.NameOriginGenerated{} ->
      let cName' = fromMaybe cName (listToMaybe (C.declAliases declInfo))
      in  C.QualName cName' C.NameKindOrdinary
    C.NameOriginRenamedFrom fromCName -> C.QualName fromCName cNameKind
  where
    cName :: C.Name
    cName = C.nameC (C.declId declInfo)

-- TODO strategy
-- TODO constraints
mkInstSpecs ::
     Map Hs.TypeClass (Omittable BindingSpec.InstanceSpec)
  -> Set Hs.TypeClass
  -> Map Hs.TypeClass (Omittable BindingSpec.InstanceSpec)
mkInstSpecs specMap insts = Map.fromList $
    [ (cls, Require def)
    | cls <- Set.toList insts
    ]
    ++
    [ (cls, Omit)
    | (cls, Omit) <- Map.toList specMap
    ]
