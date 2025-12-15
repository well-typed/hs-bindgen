-- | Generate Haskell foreign imports (using the 'HasBaseForeignType' class)
module HsBindgen.Backend.Hs.Translation.ForeignImport (
    foreignImportDynamicDecs
  , foreignImportWrapperDecs
  , foreignImportDecs
  ) where

import Data.Set qualified as Set
import DeBruijn.Idx (Idx (..))

import HsBindgen.Backend.Hs.AST qualified as Hs
import HsBindgen.Backend.Hs.AST.Type qualified as Hs
import HsBindgen.Backend.Hs.CallConv
import HsBindgen.Backend.Hs.Haddock.Documentation qualified as HsDoc
import HsBindgen.Backend.Hs.Origin qualified as Origin
import HsBindgen.Backend.Hs.Translation.Instances qualified as Hs
import HsBindgen.Backend.Hs.Translation.State (TranslationState)
import HsBindgen.Backend.Hs.Translation.State qualified as State
import HsBindgen.Backend.Hs.Translation.Type qualified as Hs
import HsBindgen.Backend.SHs.AST
import HsBindgen.Errors (panicPure)
import HsBindgen.Frontend.AST.External qualified as C
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs

-- | Generate a foreign import declaration.
--
-- The actual code that is generated is a pair of an internal "base" foreign
-- import declaration and a wrapper function.
--
-- For example:
--
-- > newtype A = A CInt
-- > newtype B = B CChar
-- > foreign import ccall safe "foo" foo_base :: CInt -> IO CChar
-- > foo :: A -> IO B
foreignImportDecs ::
     TranslationState
  -> Hs.Name Hs.NsVar
  -> Hs.HsType
  -> [Hs.FunctionParameter Hs.HsType]
  -> C.DeclName
  -> CallConv
  -> Origin.ForeignImport
  -> Maybe HsDoc.Comment
  -> Safety
  -> [Hs.Decl]
foreignImportDecs transState name resultType parameters origName callConv origin comment safety
  | Hs.HasBaseForeignType `elem`
      Hs.getInstances transState.instanceMap Nothing (Set.singleton Hs.HasBaseForeignType) [hsFunType]
  = [ Hs.DeclForeignImport foreignImportDecl
    , Hs.DeclFunction funDecl
    ]
  | otherwise
  = panicPure "foreignImportDecs: can not find a HasBaseForeignType instance"
  where
    hsFunType :: Hs.HsType
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

    name' :: Hs.Name 'Hs.NsVar
    name' = name <> "_base"

    resultType' :: Hs.HsBaseForeignType
    resultType' = Hs.toBaseForeignType transState.newtypeMap resultType

    parameters' :: [Hs.FunctionParameter Hs.HsBaseForeignType]
    parameters' = [
          param' { Hs.functionParameterComment = Nothing }
        | param <- parameters
        , let param' = fmap (Hs.toBaseForeignType transState.newtypeMap) param
        ]

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { functionDeclName       = name
        , functionDeclResultType = resultType
        , functionDeclParameters = parameters
        , functionDeclOrigin     = origin
        , functionDeclComment    = comment
        , functionDeclBody       = EGlobal HasBaseForeignType_fromBaseForeignType `EApp` EFree name'
        }

{-------------------------------------------------------------------------------
  Dynamic wrapper
-------------------------------------------------------------------------------}

-- | Generate a so-called dynamic wrapper that turns a Haskell function into a C
-- function pointer.
--
-- > foreign import ccall "wrapper"
-- >   mkCallback :: IO () -> IO (FunPtr (IO ()))
--
-- For more information on this type of wrapper, see section "8.5.1 Standard C
-- Calls" from the "Haskell 2010 Language" report.
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1620008.5.1>
--
-- NOTE: like 'foreignImportDecs', this uses an internal "base" function with a wrapper.
foreignImportWrapperDecs ::
     TranslationState
  -> Hs.Name Hs.NsVar
  -> Hs.HsType
  -> C.Type
  -> Maybe HsDoc.Comment
  -> [Hs.Decl]
foreignImportWrapperDecs transState name hsType origin comment
  | Hs.HasBaseForeignType `elem`
      Hs.getInstances transState.instanceMap Nothing (Set.singleton Hs.HasBaseForeignType) [hsType]
  = [ Hs.DeclForeignImportWrapper foreignImportWrapperDecl
    , Hs.DeclFunction funDecl
    ]
  | otherwise
  = panicPure "foreignImportWrapperDecs: can not find a HasBaseForeignType instance"
  where
    foreignImportWrapperDecl :: Hs.ForeignImportWrapperDecl
    foreignImportWrapperDecl =  Hs.ForeignImportWrapperDecl
        { foreignImportWrapperName        = name'
        , foreignImportWrapperForeignType = hsType'
        , foreignImportWrapperOrigin      = origin
        , foreignImportWrapperComment     = Just $
            HsDoc.title [ HsDoc.TextContent "This is an internal function." ]
        }

    name' :: Hs.Name Hs.NsVar
    name' = name <> "_base"

    hsType' :: Hs.HsBaseForeignType
    hsType' = Hs.toBaseForeignType transState.newtypeMap hsType

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { functionDeclName       = name
        , functionDeclResultType = Hs.HsIO $ Hs.HsFunPtr hsType
        , functionDeclParameters = [
              Hs.FunctionParameter{
                  functionParameterName    = Nothing
                , functionParameterType    = hsType
                , functionParameterComment = Nothing
                }
            ]
        , functionDeclOrigin     = Origin.ToFunPtr origin
        , functionDeclComment    = comment
        , functionDeclBody       =
            ELam "fun" $
            EGlobal Functor_fmap `EApp`
            EGlobal HasBaseForeignType_castFunPtrFromBaseForeignType `EApp`
            (EFree name' `EApp`
            (EGlobal HasBaseForeignType_toBaseForeignType `EApp`
            EBound IZ
            ))
        }

{-------------------------------------------------------------------------------
  Dynamic import
-------------------------------------------------------------------------------}

-- | Generate a so-called dynamic import that turns a C function pointer into a
-- corresponding Haskell function.
--
-- > foreign import ccall "dynamic"
-- >   mkFun :: FunPtr (CInt -> IO ()) -> (CInt -> IO ())
--
-- For more information on this type of wrapper, see section "8.5.1 Standard C
-- Calls" from the "Haskell 2010 Language" report.
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1620008.5.1>
--
-- NOTE: like 'foreignImportDecs', this uses an internal "base" function with a
-- wrapper.
foreignImportDynamicDecs ::
     TranslationState
  -> Hs.Name Hs.NsVar
  -> Hs.HsType
  -> C.Type
  -> Maybe HsDoc.Comment
  -> [Hs.Decl]
foreignImportDynamicDecs transState name hsType origin comment
  | Hs.HasBaseForeignType `elem`
      Hs.getInstances instsMap Nothing (Set.singleton Hs.HasBaseForeignType) [hsType]
  = [ Hs.DeclForeignImportDynamic foreignImportDynamicDecl
    , Hs.DeclFunction funDecl
    ]
  | otherwise
  = panicPure "foreignImportDynamicDecs: can not find a HasBaseForeignType instance"
  where
    instsMap = State.instanceMap transState

    foreignImportDynamicDecl :: Hs.ForeignImportDynamicDecl
    foreignImportDynamicDecl =  Hs.ForeignImportDynamicDecl
        { foreignImportDynamicName        = name'
        , foreignImportDynamicForeignType = hsType'
        , foreignImportDynamicOrigin      = origin
        , foreignImportDynamicComment     = Just $
            HsDoc.title [ HsDoc.TextContent "This is an internal function." ]
        }

    name' :: Hs.Name Hs.NsVar
    name' = name <> "_base"

    hsType' :: Hs.HsBaseForeignType
    hsType' = Hs.toBaseForeignType transState.newtypeMap hsType

    funDecl :: Hs.FunctionDecl
    funDecl = Hs.FunctionDecl
        { functionDeclName       = name
        , functionDeclResultType = hsType
        , functionDeclParameters = [
              Hs.FunctionParameter{
                  functionParameterName    = Nothing
                , functionParameterType    = Hs.HsFunPtr hsType
                , functionParameterComment = Nothing
                }
            ]
        , functionDeclOrigin     = Origin.ToFunPtr origin
        , functionDeclComment    = comment
        , functionDeclBody       =
            ELam "funPtr" $
            EGlobal HasBaseForeignType_fromBaseForeignType `EApp`
            (EFree name' `EApp`
            (EGlobal HasBaseForeignType_castFunPtrToBaseForeignType `EApp`
            EBound IZ
            ))
        }
