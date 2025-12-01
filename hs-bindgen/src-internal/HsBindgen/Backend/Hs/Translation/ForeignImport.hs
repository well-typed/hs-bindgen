-- | Generate Haskell foreign imports (using the 'HasBaseForeignType' class)
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    vanillaForeignImportDecs
  , foreignImportDecs
  , hasBaseForeignTypeDecs
  ) where

import Data.Set qualified as Set

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

vanillaForeignImportDecs ::
     Hs.Name 'Hs.NsVar
  -> HsType
  -> [Hs.FunctionParameter]
  -> C.Name
  -> CallConv
  -> Origin.ForeignImport
  -> Maybe HsDoc.Comment
  -> Safety
  -> [Hs.Decl]
vanillaForeignImportDecs name resultType parameters origName callConv origin comment safety =
    [ Hs.DeclForeignImport $ Hs.ForeignImportDecl
        { foreignImportName         = name
        , foreignImportResultType   = resultType
        , foreignImportParameters   = parameters
        , foreignImportOrigName     = origName
        , foreignImportCallConv     = callConv
        , foreignImportOrigin       = origin
        , foreignImportComment      = comment
        , foreignImportSafety       = safety
        }
    ]

foreignImportDecs ::
     Hs.InstanceMap
  -> Hs.Name 'Hs.NsVar
  -> HsType
  -> [Hs.FunctionParameter]
  -> C.Name
  -> CallConv
  -> Origin.ForeignImport
  -> Maybe HsDoc.Comment
  -> Safety
  -> [Hs.Decl]
foreignImportDecs instsMap name resultType parameters origName callConv origin comment safety
  | Hs.HasBaseForeignType `elem`
      Hs.getInstances instsMap Nothing (Set.singleton Hs.HasBaseForeignType) [hsFunType]
  = [ Hs.DeclForeignImport foreignImportDecl
    , Hs.DeclFunction funDecl
    ]
  | otherwise
  = panicPure "Can not find an IsForeignType instance!"
  where
    hsFunType :: HsType
    hsFunType =
        foldr
          (\arg rest -> Hs.HsFun (Hs.functionParameterType arg) rest)
          resultType
          parameters

    foreignImportDecl :: Hs.ForeignImportDecl
    foreignImportDecl =  Hs.ForeignImportDecl
        { foreignImportName         = name'
        , foreignImportResultType   = resultType'
        , foreignImportParameters   = parameters'
        , foreignImportOrigName     = origName
        , foreignImportCallConv     = callConv
        , foreignImportOrigin       = origin
        , foreignImportComment      = Just $
            HsDoc.title [ HsDoc.TextContent "This is an internal function." ]
        , foreignImportSafety       = safety
        }

    name' = name <> "_base"
    resultType' = HsBaseForeignType hsFunType
    parameters' = []

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { functionDeclName       = name
        , functionDeclResultType = resultType
        , functionDeclParameters = parameters
        , functionDeclOrigin     = origin
        , functionDeclComment    = comment
        , functionDeclBody       = EGlobal HasBaseForeignType_fromBaseForeignType `EApp` EFree name'
        }

hasBaseForeignTypeDecs ::
     Set Hs.TypeClass
  -> Hs.Newtype
  -> [Hs.Decl]
hasBaseForeignTypeDecs insts nt =
     [mk | Hs.HasBaseForeignType `elem` insts]
  where
    mk :: Hs.Decl
    mk = Hs.DeclDeriveInstance
              Hs.DeriveInstance {
                deriveInstanceStrategy = Hs.DeriveNewtype
              , deriveInstanceClass    = Hs.HasBaseForeignType
              , deriveInstanceName     = Hs.newtypeName nt
              , deriveInstanceComment  = Nothing
              }
