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
import HsBindgen.Backend.Hs.Name qualified as Hs
import HsBindgen.Backend.Hs.Origin qualified as HsOrigin
import HsBindgen.BindingSpec.Private.Common
import HsBindgen.BindingSpec.Private.V1 (UnresolvedBindingSpec)
import HsBindgen.BindingSpec.Private.V1 qualified as BindingSpec
import HsBindgen.Config.ClangArgs qualified as ClangArgs
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
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | Generate binding specification
genBindingSpec ::
     Format
  -> ClangArgs.Target
  -> Hs.ModuleName
  -> IncludeGraph
  -> DeclIndex
  -> GetMainHeaders
  -> [(C.DeclId, SourcePath)]
  -> [(C.DeclId, (SourcePath, Hs.Identifier))]
  -> [Hs.Decl]
  -> ByteString
genBindingSpec
  format
  target
  hsModuleName
  includeGraph
  declIndex
  getMainHeaders
  omitTypes
  squashedTypes =
      BindingSpec.encode compareCDeclId format
    . genBindingSpec' target hsModuleName getMainHeaders omitTypes squashedTypes
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

-- TODO aliases
genBindingSpec' ::
     ClangArgs.Target
  -> Hs.ModuleName
  -> GetMainHeaders
  -> [(C.DeclId, SourcePath)]
  -> [(C.DeclId, (SourcePath, Hs.Identifier))]
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec'
    target
    hsModuleName
    getMainHeaders
    omitTypes
    squashedTypes = foldr aux spec0
  where
    spec0 :: UnresolvedBindingSpec
    spec0 = BindingSpec.BindingSpec {
        -- TODO AnyTarget if bindings are not target-specific
        BindingSpec.bindingSpecTarget = BindingSpec.SpecificTarget target
      , BindingSpec.bindingSpecModule = hsModuleName
      , BindingSpec.bindingSpecCTypes = Map.fromListWith (++) $
          [ (cDeclId, [(getMainHeaders' path, Omit)])
          | (cDeclId, path) <- omitTypes
          ] ++
          [ let headers   = getMainHeaders' sourcePath
                cTypeSpec = def{ BindingSpec.cTypeSpecIdentifier = Just hsId }
            in  (cDeclId, [(headers, Require cTypeSpec)])
          | (cDeclId, (sourcePath, hsId)) <- squashedTypes
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
      Hs.DeclNewtype ntype    ->
        case ntype.newtypeOrigin.declKind of
          HsOrigin.Aux{} -> id
          _otherwise     -> insertType $ auxNewtype ntype
      Hs.DeclPatSyn{}         -> id
      Hs.DeclDefineInstance{} -> id
      Hs.DeclDeriveInstance{} -> id
      Hs.DeclForeignImport{}  -> id
      Hs.DeclFunction{}       -> id
      Hs.DeclMacroExpr{}      -> id
      Hs.DeclUnionGetter{}    -> id
      Hs.DeclUnionSetter{}    -> id
      Hs.DeclVar{}            -> id

    insertType ::
         ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    insertType ((declInfo, cTypeSpec), (hsId, hsTypeSpec)) spec = spec{
        BindingSpec.bindingSpecCTypes =
          Map.insertWith (++)
            declInfo.declId.cName
            [(getHeaders declInfo, Require cTypeSpec)]
            (BindingSpec.bindingSpecCTypes spec)
      , BindingSpec.bindingSpecHsTypes =
          Map.insert
            hsId
            hsTypeSpec
            (BindingSpec.bindingSpecHsTypes spec)
      }

    auxStruct ::
         Hs.Struct n
      -> ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
    auxStruct hsStruct = case Hs.structOrigin hsStruct of
      Nothing -> panicPure "auxStruct: structOrigin is Nothing"
      Just originDecl ->
        let declInfo = HsOrigin.declInfo originDecl
            hsIdentifier = Hs.Identifier $ Hs.getName (Hs.structName hsStruct)
            cTypeSpec = BindingSpec.CTypeSpec {
                cTypeSpecIdentifier = Just hsIdentifier
              , cTypeSpecRep        = Nothing  -- TODO implement
              }
            hsRecordRep = BindingSpec.HsRecordRep {
                hsRecordRepConstructor = Just $ Hs.Identifier $ Hs.getName $ Hs.structConstr hsStruct
              , hsRecordRepFields = Just [
                    Hs.Identifier $ Hs.getName $ Hs.fieldName field
                  | field <- Vec.toList $ Hs.structFields hsStruct
                  ]
              }
            hsTypeSpec = BindingSpec.HsTypeSpec {
                hsTypeSpecRep = Just $ BindingSpec.HsTypeRepRecord hsRecordRep
              , hsTypeSpecInstances =
                  mkInstSpecs
                    ( maybe Map.empty BindingSpec.hsTypeSpecInstances $
                       (HsOrigin.declSpec originDecl).hsSpec
                    )
                    (Hs.structInstances hsStruct)
              }
        in  ( (declInfo, cTypeSpec)
            , (hsIdentifier, hsTypeSpec)
            )

    auxEmptyData ::
         Hs.EmptyData
      -> ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
    auxEmptyData edata =
      let originDecl = Hs.emptyDataOrigin edata
          declInfo = HsOrigin.declInfo originDecl
          hsIdentifier = Hs.Identifier $ Hs.getName (Hs.emptyDataName edata)
          cTypeSpec = BindingSpec.CTypeSpec {
              cTypeSpecIdentifier = Just hsIdentifier
            , cTypeSpecRep        = Nothing  -- TODO implement
            }
          hsTypeSpec = BindingSpec.HsTypeSpec {
              hsTypeSpecRep       = Just BindingSpec.HsTypeRepOpaque
            , hsTypeSpecInstances = Map.empty
            }
      in  ( (declInfo, cTypeSpec)
          , (hsIdentifier, hsTypeSpec)
          )

    auxNewtype ::
         Hs.Newtype
      -> ( (C.DeclInfo Final, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
    auxNewtype hsNewtype =
      let originDecl = Hs.newtypeOrigin hsNewtype
          declInfo = HsOrigin.declInfo originDecl
          hsIdentifier = Hs.Identifier $ Hs.getName (Hs.newtypeName hsNewtype)
          cTypeSpec = BindingSpec.CTypeSpec {
              cTypeSpecIdentifier = Just hsIdentifier
            , cTypeSpecRep        = Nothing  -- TODO implement
            }
          hsNewtypeRep = BindingSpec.HsNewtypeRep {
              hsNewtypeRepConstructor = Just $ Hs.Identifier $ Hs.getName $ Hs.newtypeConstr hsNewtype
            , hsNewtypeRepField = Just $ Hs.Identifier $ Hs.getName $ Hs.fieldName $ Hs.newtypeField hsNewtype
            , hsNewtypeRepFFIType = Hs.newtypeFFIType hsNewtype
            }
          hsTypeSpec = BindingSpec.HsTypeSpec {
              hsTypeSpecRep = Just $ BindingSpec.HsTypeRepNewtype hsNewtypeRep
            , hsTypeSpecInstances =
                mkInstSpecs
                  ( maybe Map.empty BindingSpec.hsTypeSpecInstances $
                      (HsOrigin.declSpec originDecl).hsSpec
                  )
                  (Hs.newtypeInstances hsNewtype)
            }
      in  ( (declInfo, cTypeSpec)
          , (hsIdentifier, hsTypeSpec)
          )

    getHeaders :: C.DeclInfo Final -> Set HashIncludeArg
    getHeaders = getMainHeaders' . singleLocPath . C.declLoc

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
