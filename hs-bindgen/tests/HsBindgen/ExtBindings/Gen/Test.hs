module HsBindgen.ExtBindings.Gen.Test (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Vec.Lazy qualified as Vec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Clang.CNameSpelling
import Clang.Paths
import HsBindgen.C.AST qualified as C
import HsBindgen.ExtBindings
import HsBindgen.ExtBindings.Gen
import HsBindgen.Hs.AST qualified as Hs
import HsBindgen.Imports

import Hidden

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "HsBindgen.ExtBindings.Gen"
    [ testGenExtBindings
    ]

testGenExtBindings :: TestTree
testGenExtBindings = testGroup "genExtBindings"
    [ testCase "empty" $
        genExtBindings testH testPackageName testModuleName []
          @?= emptyUnresolvedExtBindings
    , testCase "struct-struct" $ do
        let decl = structStruct tagFoo ["foo_t"]
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["struct foo", "foo_t"]
    , testCase "struct-struct-typedef" $ do
        let decl = structStruct typedefFoo []
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["foo"]
    , testCase "struct-enum" $ do
        let decl = structEnum tagFoo ["foo_t"]
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["enum foo", "foo_t"]
    , testCase "struct-enum-typedef" $ do
        let decl = structEnum typedefFoo []
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["foo"]
    , testCase "opaque-struct" $ do
        let decl = opaqueStruct ["foo_t"]
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["struct foo", "foo_t"]
    , testCase "opaque-enum" $ do
        let decl = opaqueEnum ["foo_t"]
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["enum foo", "foo_t"]
    , testCase "newtype-enum" $ do
        let decl = newtypeEnum tagFoo ["foo_t"]
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["enum foo", "foo_t"]
    , testCase "newtype-enum-typedef" $ do
        let decl = newtypeEnum typedefFoo []
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["foo"]
    , testCase "newtype-typedef" $
        genExtBindings testH testPackageName testModuleName [newtypeTypedef]
          @?= fooUnresolvedExtBindings ["foo"]
    , testCase "newtype-union" $ do
        let decl = newtypeUnion tagFoo ["foo_t"]
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["union foo", "foo_t"]
    , testCase "newtype-union-typedef" $ do
        let decl = newtypeUnion typedefFoo []
        genExtBindings testH testPackageName testModuleName [decl]
          @?= fooUnresolvedExtBindings ["foo"]
    , testCase "newtype-macro" $
        genExtBindings testH testPackageName testModuleName [newtypeMacro]
          @?= fooUnresolvedExtBindings ["foo"]
    , testCase "pat-syn" $ do
        let decl = Hs.DeclPatSyn hidden
        genExtBindings testH testPackageName testModuleName [decl]
          @?= emptyUnresolvedExtBindings
    , testCase "define-instance" $ do
        let decl = Hs.DeclDefineInstance hidden
        genExtBindings testH testPackageName testModuleName [decl]
          @?= emptyUnresolvedExtBindings
    , testCase "derive-instance" $ do
        let decl = Hs.DeclDeriveInstance hidden hidden hidden
        genExtBindings testH testPackageName testModuleName [decl]
          @?= emptyUnresolvedExtBindings
    , testCase "foreign-import" $ do
        let decl = Hs.DeclForeignImport hidden
        genExtBindings testH testPackageName testModuleName [decl]
          @?= emptyUnresolvedExtBindings
    , testCase "var" $ do
        let decl = Hs.DeclVar hidden
        genExtBindings testH testPackageName testModuleName [decl]
          @?= emptyUnresolvedExtBindings
    , testCase "union-getter" $ do
        let decl = Hs.DeclUnionGetter hidden hidden hidden
        genExtBindings testH testPackageName testModuleName [decl]
          @?= emptyUnresolvedExtBindings
    , testCase "union-setter" $ do
        let decl = Hs.DeclUnionSetter hidden hidden hidden
        genExtBindings testH testPackageName testModuleName [decl]
          @?= emptyUnresolvedExtBindings
    ]
  where
    tagFoo, typedefFoo :: C.DeclPath
    tagFoo     = C.DeclPathName "foo" C.DeclPathCtxtTop
    typedefFoo = C.DeclPathAnon $ C.DeclPathCtxtTypedef "foo"

{-------------------------------------------------------------------------------
  Test data
-------------------------------------------------------------------------------}

testH :: CHeaderIncludePath
testH = CHeaderQuoteIncludePath "test.h"

testHSet :: Set CHeaderIncludePath
testHSet = Set.singleton testH

testPackageName :: HsPackageName
testPackageName = HsPackageName "test"

testModuleName :: HsModuleName
testModuleName = HsModuleName "Test"

mkExtIdentifier :: Text -> ExtIdentifier
mkExtIdentifier t = ExtIdentifier
    { extIdentifierPackage    = testPackageName
    , extIdentifierModule     = testModuleName
    , extIdentifierIdentifier = HsIdentifier t
    }

emptyUnresolvedExtBindings :: UnresolvedExtBindings
emptyUnresolvedExtBindings = UnresolvedExtBindings
    { unresolvedExtBindingsTypes = Map.empty
    }

fooUnresolvedExtBindings :: [Text] -> UnresolvedExtBindings
fooUnresolvedExtBindings names = UnresolvedExtBindings
    { unresolvedExtBindingsTypes = Map.fromList
        [ (CNameSpelling name, [(testHSet, mkExtIdentifier "Foo")])
        | name <- names
        ]
    }

structStruct :: C.DeclPath -> [C.CName] -> Hs.Decl
structStruct declPath aliases = Hs.DeclData Hs.Struct
    { Hs.structName   = "Foo"
    , Hs.structConstr = hidden
    , Hs.structFields = Vec.singleton hidden
    , Hs.structOrigin = Hs.StructOriginStruct C.Struct
        { C.structDeclPath  = declPath
        , C.structAliases   = aliases
        , C.structSizeof    = hidden
        , C.structAlignment = hidden
        , C.structFields    = hidden
        , C.structFlam      = hidden
        , C.structSourceLoc = hidden
        }
    }

opaqueStruct :: [C.CName] -> Hs.Decl
opaqueStruct aliases = Hs.DeclEmpty Hs.EmptyData
    { Hs.emptyDataName   = "Foo"
    , Hs.emptyDataOrigin = Hs.EmptyDataOriginOpaqueStruct C.OpaqueStruct
        { C.opaqueStructTag       = "foo"
        , C.opaqueStructAliases   = aliases
        , C.opaqueStructSourceLoc = hidden
        }
    }

newtypeUnion :: C.DeclPath -> [C.CName] -> Hs.Decl
newtypeUnion declPath aliases = Hs.DeclNewtype Hs.Newtype
    { Hs.newtypeName   = "Foo"
    , Hs.newtypeConstr = hidden
    , Hs.newtypeField  = hidden
    , Hs.newtypeOrigin = Hs.NewtypeOriginUnion C.Union
        { C.unionDeclPath  = declPath
        , C.unionAliases   = aliases
        , C.unionSizeof    = hidden
        , C.unionAlignment = hidden
        , C.unionFields    = hidden
        , C.unionSourceLoc = hidden
        }
    }

structEnum :: C.DeclPath -> [C.CName] -> Hs.Decl
structEnum declPath aliases = Hs.DeclData Hs.Struct
    { Hs.structName   = "Foo"
    , Hs.structConstr = hidden
    , Hs.structFields = Vec.singleton hidden
    , Hs.structOrigin = Hs.StructOriginEnum $ cEnumFoo declPath aliases
    }

newtypeEnum :: C.DeclPath -> [C.CName] -> Hs.Decl
newtypeEnum declPath aliases = Hs.DeclNewtype Hs.Newtype
    { Hs.newtypeName   = "Foo"
    , Hs.newtypeConstr = hidden
    , Hs.newtypeField  = hidden
    , Hs.newtypeOrigin = Hs.NewtypeOriginEnum $ cEnumFoo declPath aliases
    }

cEnumFoo :: C.DeclPath -> [C.CName] -> C.Enu
cEnumFoo declPath aliases = C.Enu
    { C.enumDeclPath  = declPath
    , C.enumAliases   = aliases
    , C.enumType      = hidden
    , C.enumSizeof    = hidden
    , C.enumAlignment = hidden
    , C.enumValues    = hidden
    , C.enumSourceLoc = hidden
    }

opaqueEnum :: [C.CName] -> Hs.Decl
opaqueEnum aliases = Hs.DeclEmpty Hs.EmptyData
    { Hs.emptyDataName   = "Foo"
    , Hs.emptyDataOrigin = Hs.EmptyDataOriginOpaqueEnum C.OpaqueEnum
        { C.opaqueEnumTag       = "foo"
        , C.opaqueEnumAliases   = aliases
        , C.opaqueEnumSourceLoc = hidden
        }
    }

newtypeTypedef :: Hs.Decl
newtypeTypedef = Hs.DeclNewtype Hs.Newtype
    { Hs.newtypeName   = "Foo"
    , Hs.newtypeConstr = hidden
    , Hs.newtypeField  = hidden
    , Hs.newtypeOrigin = Hs.NewtypeOriginTypedef C.Typedef
        { C.typedefName      = "foo"
        , C.typedefType      = hidden
        , C.typedefSourceLoc = hidden
        }
    }

newtypeMacro :: Hs.Decl
newtypeMacro = Hs.DeclNewtype Hs.Newtype
    { Hs.newtypeName   = "Foo"
    , Hs.newtypeConstr = hidden
    , Hs.newtypeField  = hidden
    , Hs.newtypeOrigin = Hs.NewtypeOriginMacro C.Macro
        { C.macroLoc  = hidden
        , C.macroName = "foo"
        , C.macroArgs = hidden
        , C.macroBody = hidden
        }
    }
