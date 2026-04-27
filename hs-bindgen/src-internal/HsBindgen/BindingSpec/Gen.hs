-- | Binding specification generation
--
-- Intended for qualified import.
--
-- > import HsBindgen.BindingSpec.Gen qualified as BindingSpec
module HsBindgen.BindingSpec.Gen (
    -- * Public API
    genBindingSpec
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Data.Set qualified as Set
import Data.Vec.Lazy qualified as Vec

import Clang.HighLevel.Types
import Clang.Paths

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as HsOrigin
import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.V1 (CTypeSpec (..), UnresolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Errors
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndex)
import HsBindgen.Frontend.Analysis.DeclIndex qualified as DeclIndex
import HsBindgen.Frontend.Analysis.IncludeGraph (IncludeGraph)
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Naming as C
import HsBindgen.Frontend.Pass.Final
import HsBindgen.Frontend.Pass.ResolveBindingSpecs.IsPass qualified as ResolveBindingSpecs
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Instances qualified as Inst
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Generate binding specification
genBindingSpec ::
     Format
  -> Hs.ModuleName
  -> IncludeGraph
  -> DeclIndex
  -> GetMainHeaders
  -> [(C.DeclId, SourcePath)]
  -> [(C.DeclId, (SourcePath, Hs.Name Hs.NsTypeConstr))]
  -> [Hs.Decl]
  -> ByteString
genBindingSpec
  format
  hsModuleName
  includeGraph
  declIndex
  getMainHeaders
  omitTypes
  squashedTypes =
      BindingSpec.encode compareCDeclId format
    . genBindingSpec' hsModuleName getMainHeaders omitTypes squashedTypes
  where
    compareCDeclId :: C.DeclId -> C.DeclId -> Ordering
    compareCDeclId cDeclIdL cDeclIdR = Ord.comparing aux cDeclIdL cDeclIdR

    aux :: C.DeclId -> (Int, Int, Int, Text)
    aux cDeclId =
      case Maybe.listToMaybe (DeclIndex.lookupLoc cDeclId declIndex) of
        Just sloc ->
          ( fromMaybe maxBound (Map.lookup sloc.singleLocPath orderMap)
          , sloc.singleLocLine
          , sloc.singleLocColumn
          , C.renderDeclId cDeclId
          )
        Nothing -> (maxBound, maxBound, maxBound, C.renderDeclId cDeclId)

    orderMap :: Map SourcePath Int
    orderMap = IncludeGraph.toOrderMap includeGraph

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1436>
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1549>
-- Once squashing is configurable (#1436), we should deal with aliases (#1549).
genBindingSpec' ::
     Hs.ModuleName
  -> GetMainHeaders
  -> [(C.DeclId, SourcePath)]
  -> [(C.DeclId, (SourcePath, Hs.Name Hs.NsTypeConstr))]
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec' hsModuleName getMainHeaders omitTypes squashedTypes =
    foldr aux spec0
  where
    spec0 :: UnresolvedBindingSpec
    spec0 = BindingSpec.BindingSpec {
        moduleName = hsModuleName
      , cTypes     = Map.fromListWith (++) $
          [ (cDeclId, [(getMainHeaders' path, Omit)])
          | (cDeclId, path) <- omitTypes
          ] ++
          [ let headers   = getMainHeaders' sourcePath
                cTypeSpec :: CTypeSpec
                cTypeSpec = CTypeSpec $ Just hsName
            in  (cDeclId, [(headers, Require cTypeSpec)])
          | (cDeclId, (sourcePath, hsName)) <- squashedTypes
          ]
      , hsTypes = Map.empty
      }

    getMainHeaders' :: SourcePath -> Set HashIncludeArg
    getMainHeaders' =
        either
          (\s -> panicPure ("Could not get main headers: " ++ s))
          (Set.fromList . NonEmpty.toList)
      . getMainHeaders

    aux ::
         Hs.Decl
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    aux = \case
      Hs.DeclTypSyn typSyn          -> insertType $ auxTypSyn    typSyn
      Hs.DeclData struct            -> insertType $ auxStruct    struct
      Hs.DeclEmpty edata            -> insertType $ auxEmptyData edata
      Hs.DeclNewtype ntype          ->
        case ntype.origin.kind of
          HsOrigin.Aux{} -> id
          _otherwise     -> insertType $ auxNewtype ntype
      Hs.DeclPatSyn{}               -> id
      Hs.DeclDefineInstance{}       -> id
      Hs.DeclDeriveInstance{}       -> id
      Hs.DeclForeignImport{}        -> id
      Hs.DeclForeignImportWrapper{} -> id
      Hs.DeclForeignImportDynamic{} -> id
      Hs.DeclFunction{}             -> id
      Hs.DeclMacroExpr{}            -> id
      Hs.DeclUnionGetter{}          -> id
      Hs.DeclUnionSetter{}          -> id
      Hs.DeclVar{}                  -> id

    insertType ::
         ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Name Hs.NsTypeConstr, BindingSpec.HsTypeSpec)
         )
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    insertType ((declInfo, cTypeSpec), (hsName, hsTypeSpec)) spec =
      spec
        & #cTypes %~
            Map.insertWith (++)
              declInfo.id.cName
              [(getHeaders declInfo, Require cTypeSpec)]
        & #hsTypes %~
            Map.insert hsName hsTypeSpec

    auxTypSyn ::
         Hs.TypSyn
      -> ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Name Hs.NsTypeConstr, BindingSpec.HsTypeSpec)
         )
    auxTypSyn typSyn =
      let cTypeSpec = BindingSpec.CTypeSpec {
              hsName = Just typSyn.name
            }
          hsTypeSpec = BindingSpec.HsTypeSpec {
              hsRep     = Just BindingSpec.HsTypeRepTypeAlias
            , instances = Map.empty
            }
      in  ( (typSyn.origin.info, cTypeSpec)
          , (typSyn.name, hsTypeSpec)
          )

    auxStruct ::
         Hs.Struct n
      -> ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Name Hs.NsTypeConstr, BindingSpec.HsTypeSpec)
         )
    auxStruct hsStruct = case hsStruct.origin of
      Nothing -> panicPure "Origin of structure unavailable"
      Just originDecl ->
        let cTypeSpec = BindingSpec.CTypeSpec {
                hsName = Just hsStruct.name
              }
            hsRecordRep = BindingSpec.HsRecordRep {
                constructor = Just hsStruct.constr
              , fields      = Just [
                    field.name
                  | field <- Vec.toList hsStruct.fields
                  ]
              }
            hsTypeSpec = BindingSpec.HsTypeSpec {
                hsRep     = Just $ BindingSpec.HsTypeRepRecord hsRecordRep
              , instances =
                  mkInstSpecs
                    (maybe Map.empty (.instances) $ originDecl.spec.hsSpec)
                    hsStruct.instances
              }
        in  ( (originDecl.info, cTypeSpec)
            , (hsStruct.name, hsTypeSpec)
            )

    auxEmptyData ::
         Hs.EmptyData
      -> ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Name Hs.NsTypeConstr, BindingSpec.HsTypeSpec)
         )
    auxEmptyData edata =
      let originDecl   = edata.origin
          cTypeSpec = BindingSpec.CTypeSpec {
              hsName = Just edata.name
            }
          hsTypeSpec = BindingSpec.HsTypeSpec {
              hsRep     = Just BindingSpec.HsTypeRepEmptyData
            , instances = Map.empty
            }
      in  ( (originDecl.info, cTypeSpec)
          , (edata.name, hsTypeSpec)
          )

    auxNewtype ::
         Hs.Newtype
      -> ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Name Hs.NsTypeConstr, BindingSpec.HsTypeSpec)
         )
    auxNewtype hsNewtype =
      let originDecl   = hsNewtype.origin
          cTypeSpec    = BindingSpec.CTypeSpec {
              hsName = Just hsNewtype.name
            }
          hsNewtypeRep = BindingSpec.HsNewtypeRep {
              constructor = Just hsNewtype.constr
            , field       = Just hsNewtype.field.name
            }
          hsTypeSpec = BindingSpec.HsTypeSpec {
              hsRep     = Just $ BindingSpec.HsTypeRepNewtype hsNewtypeRep
            , instances =
                mkInstSpecs
                  (maybe Map.empty (.instances) $ originDecl.spec.hsSpec)
                  hsNewtype.instances
            }
      in  ( (originDecl.info, cTypeSpec)
          , (hsNewtype.name, hsTypeSpec)
          )

    getHeaders :: C.DeclInfo Final -> Set HashIncludeArg
    getHeaders info = getMainHeaders' $ singleLocPath info.loc

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1766>
-- We should allow users to specify strategies for instance deriving.
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/648>
-- It should be possible to generate instances with constraints.
mkInstSpecs ::
     Map Inst.TypeClass (Omittable BindingSpec.InstanceSpec)
  -> Set Inst.TypeClass
  -> Map Inst.TypeClass (Omittable BindingSpec.InstanceSpec)
mkInstSpecs specMap insts = Map.fromList $
    [ (cls, Require def)
    | cls <- Set.toList insts
    ]
    ++
    [ (cls, Omit)
    | (cls, Omit) <- Map.toList specMap
    ]
