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
import Data.Set qualified as Set

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
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Frontend.Naming as C
import HsBindgen.Frontend.ProcessIncludes
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
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
  -> GetMainHeaders
  -> [(C.DeclName, SourcePath)]
  -> [Hs.Decl]
  -> UnresolvedBindingSpec
genBindingSpec target hsModuleName getMainHeaders omitTypes =
  genBindingSpec' target hsModuleName getMainHeaders omitTypes

{-------------------------------------------------------------------------------
  Internal API (for tests)
-------------------------------------------------------------------------------}

-- | Generate binding specification
genBindingSpecYaml ::
     ClangArgs.Target
  -> Hs.ModuleName
  -> GetMainHeaders
  -> [(C.DeclName, SourcePath)]
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
  -> [(C.DeclName, SourcePath)]
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
            (cDeclName, [(getMainHeaders' path, Omit)])
          | (cDeclName, path) <- omitTypes
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
      Hs.DeclVar{}            -> id
      Hs.DeclPragma{}         -> id

    insertType ::
         ( (C.DeclInfo, BindingSpec.CTypeSpec)
         , (Hs.Identifier, BindingSpec.HsTypeSpec)
         )
      -> UnresolvedBindingSpec
      -> UnresolvedBindingSpec
    insertType ((declInfo, cTypeSpec), (hsId, hsTypeSpec)) spec =
        case getCDeclName declInfo.declId of
          Nothing        -> spec
          Just cDeclName -> spec{
              BindingSpec.bindingSpecCTypes =
                Map.insertWith (++)
                  cDeclName
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
      -> ( (C.DeclInfo, BindingSpec.CTypeSpec)
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
            hsTypeSpec = BindingSpec.HsTypeSpec {
                hsTypeSpecRep = Nothing -- TODO implement
              , hsTypeSpecInstances =
                  mkInstSpecs
                    ( maybe Map.empty BindingSpec.hsTypeSpecInstances
                    . C.declSpecHs
                    $ HsOrigin.declSpec originDecl
                    )
                    (Hs.structInstances hsStruct)
              }
        in  ( (declInfo, cTypeSpec)
            , (hsIdentifier, hsTypeSpec)
            )

    auxEmptyData ::
         Hs.EmptyData
      -> ( (C.DeclInfo, BindingSpec.CTypeSpec)
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
      -> ( (C.DeclInfo, BindingSpec.CTypeSpec)
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
          hsTypeSpec = BindingSpec.HsTypeSpec {
              hsTypeSpecRep = Nothing -- TODO implement
            , hsTypeSpecInstances =
                mkInstSpecs
                  ( maybe Map.empty BindingSpec.hsTypeSpecInstances
                  . C.declSpecHs
                  $ HsOrigin.declSpec originDecl
                  )
                  (Hs.newtypeInstances hsNewtype)
            }
      in  ( (declInfo, cTypeSpec)
          , (hsIdentifier, hsTypeSpec)
          )

    getHeaders :: C.DeclInfo -> Set HashIncludeArg
    getHeaders = getMainHeaders' . singleLocPath . C.declLoc

getCDeclName :: C.FinalDeclId -> Maybe C.DeclName
getCDeclName declId =
    case declId.origDeclId of
      C.OrigDeclId orig ->
        Just $ auxOrig orig
      C.AuxForDecl _parent ->
        -- TODO <https://github.com/well-typed/hs-bindgen/issues/1379>
        Nothing
  where
    auxOrig :: PrelimDeclId -> C.DeclName
    auxOrig = \case
        PrelimDeclIdNamed cName       -> cName
        PrelimDeclIdAnon _anonId kind ->
          -- Anonymous declarations can only arise in very specific situations:
          --
          -- * Anonymous declarations without any use sites are removed
          -- * Anonymous declarations inside typedefs are given a name, but their
          --   /original/ declaration is set to be the typedef instead.
          --
          -- However, anonymous structs inside other structs or unions will
          -- remain anonymous. For these we instead use the name assigned by
          -- the @NameAnon@ pass.
          --
          -- TODO <https://github.com/well-typed/hs-bindgen/issues/844>
          -- This is WIP.
          C.DeclName ("@" <> declId.name.text) kind

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
