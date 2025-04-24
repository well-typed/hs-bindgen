[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "I",
      newtypeConstr = HsName
        "@NsConstr"
        "I",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_I",
        fieldType = HsPrimType
          HsPrimCInt,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_in_fundecl.h:5:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "I",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral PrimInt Signed)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "I"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "I"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "C",
      newtypeConstr = HsName
        "@NsConstr"
        "C",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_C",
        fieldType = HsPrimType
          HsPrimCChar,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_in_fundecl.h:6:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "C",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimChar
                    (PrimSignImplicit Nothing))))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "C"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "C"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "F",
      newtypeConstr = HsName
        "@NsConstr"
        "F",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_F",
        fieldType = HsPrimType
          HsPrimCFloat,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_in_fundecl.h:7:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "F",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimFloating PrimFloat)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveNewtype
    Floating
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveNewtype
    Fractional
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFloat
    (HsName "@NsTypeConstr" "F"),
  DeclNewtypeInstance
    DeriveNewtype
    RealFrac
    (HsName "@NsTypeConstr" "F"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "L",
      newtypeConstr = HsName
        "@NsConstr"
        "L",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_L",
        fieldType = HsPrimType
          HsPrimCLong,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_in_fundecl.h:8:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "L",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral PrimLong Signed)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "L"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "L"),
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "S",
      newtypeConstr = HsName
        "@NsConstr"
        "S",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_S",
        fieldType = HsPrimType
          HsPrimCShort,
        fieldOrigin = FieldOriginNone},
      newtypeOrigin =
      NewtypeOriginMacro
        Macro {
          macroLoc = MultiLoc {
            multiLocExpansion =
            "macro_in_fundecl.h:9:9",
            multiLocPresumed = Nothing,
            multiLocSpelling = Nothing,
            multiLocFile = Nothing},
          macroName = CName "S",
          macroArgs = [],
          macroBody = TypeMacro
            (TypeName
              (TypeSpecifier
                (TypePrim
                  (PrimIntegral
                    PrimShort
                    Signed)))
              []
              Declarator {
                declaratorPointer = Pointers [],
                directDeclarator =
                IdentifierDeclarator
                  AbstractName
                  []})}},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveStock
    Ord
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveStock
    Read
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    Enum
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    Ix
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    Bounded
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    Bits
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    FiniteBits
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    Integral
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    Num
    (HsName "@NsTypeConstr" "S"),
  DeclNewtypeInstance
    DeriveNewtype
    Real
    (HsName "@NsTypeConstr" "S"),
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "quux",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "F"))
        (HsFun
          (HsPrimType HsPrimCChar)
          (HsIO
            (HsPrimType HsPrimCChar))),
      foreignImportCRes = TypePrim
        (PrimChar
          (PrimSignImplicit Nothing)),
      foreignImportCArgs = [
        TypeTypedef (CName "F"),
        TypePrim
          (PrimChar
            (PrimSignImplicit Nothing))],
      foreignImportOrigName = "quux",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "quux",
          functionArgs = [
            TypeTypedef (CName "F"),
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))],
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:12:6"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "wam",
      foreignImportType = HsFun
        (HsPrimType HsPrimCFloat)
        (HsFun
          (HsPtr
            (HsTypRef
              (HsName "@NsTypeConstr" "C")))
          (HsIO
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "C"))))),
      foreignImportCRes = TypePointer
        (TypeTypedef (CName "C")),
      foreignImportCArgs = [
        TypePrim
          (PrimFloating PrimFloat),
        TypePointer
          (TypeTypedef (CName "C"))],
      foreignImportOrigName = "wam",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "wam",
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeTypedef (CName "C"))],
          functionRes = TypePointer
            (TypeTypedef (CName "C")),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:13:4"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo1",
      foreignImportType = HsFun
        (HsPrimType HsPrimCFloat)
        (HsFun
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))))
          (HsIO
            (HsPtr
              (HsPrimType HsPrimCChar)))),
      foreignImportCRes = TypePointer
        (TypePrim
          (PrimChar
            (PrimSignImplicit
              (Just Signed)))),
      foreignImportCArgs = [
        TypePrim
          (PrimFloating PrimFloat),
        TypePointer
          (TypeFun
            [
              TypePrim
                (PrimIntegral PrimInt Signed)]
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))],
      foreignImportOrigName = "foo1",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "foo1",
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit
                  (Just Signed)))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:16:7"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo2",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "F"))
        (HsFun
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))))
          (HsIO
            (HsPtr
              (HsPrimType HsPrimCChar)))),
      foreignImportCRes = TypePointer
        (TypePrim
          (PrimChar
            (PrimSignImplicit Nothing))),
      foreignImportCArgs = [
        TypeTypedef (CName "F"),
        TypePointer
          (TypeFun
            [
              TypePrim
                (PrimIntegral PrimInt Signed)]
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))],
      foreignImportOrigName = "foo2",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "foo2",
          functionArgs = [
            TypeTypedef (CName "F"),
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionRes = TypePointer
            (TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:17:7"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "foo3",
      foreignImportType = HsFun
        (HsPrimType HsPrimCFloat)
        (HsFun
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCInt)
              (HsIO (HsPrimType HsPrimCInt))))
          (HsIO
            (HsPtr
              (HsTypRef
                (HsName
                  "@NsTypeConstr"
                  "C"))))),
      foreignImportCRes = TypePointer
        (TypeTypedef (CName "C")),
      foreignImportCArgs = [
        TypePrim
          (PrimFloating PrimFloat),
        TypePointer
          (TypeFun
            [
              TypePrim
                (PrimIntegral PrimInt Signed)]
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))],
      foreignImportOrigName = "foo3",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "foo3",
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))],
          functionRes = TypePointer
            (TypeTypedef (CName "C")),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:18:4"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar1",
      foreignImportType = HsFun
        (HsPrimType HsPrimCLong)
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportCRes = TypePointer
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimShort Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportCArgs = [
        TypePrim
          (PrimIntegral PrimLong Signed)],
      foreignImportOrigName = "bar1",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "bar1",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimLong Signed)],
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral PrimInt Signed))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:21:7"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar2",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "L"))
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportCRes = TypePointer
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimShort Signed)]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportCArgs = [
        TypeTypedef (CName "L")],
      foreignImportOrigName = "bar2",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "bar2",
          functionArgs = [
            TypeTypedef (CName "L")],
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral PrimInt Signed))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:22:7"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar3",
      foreignImportType = HsFun
        (HsPrimType HsPrimCLong)
        (HsIO
          (HsFunPtr
            (HsFun
              (HsTypRef
                (HsName "@NsTypeConstr" "S"))
              (HsIO
                (HsPrimType HsPrimCInt))))),
      foreignImportCRes = TypePointer
        (TypeFun
          [TypeTypedef (CName "S")]
          (TypePrim
            (PrimIntegral PrimInt Signed))),
      foreignImportCArgs = [
        TypePrim
          (PrimIntegral PrimLong Signed)],
      foreignImportOrigName = "bar3",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "bar3",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimLong Signed)],
          functionRes = TypePointer
            (TypeFun
              [TypeTypedef (CName "S")]
              (TypePrim
                (PrimIntegral PrimInt Signed))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:23:7"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "bar4",
      foreignImportType = HsFun
        (HsPrimType HsPrimCLong)
        (HsIO
          (HsFunPtr
            (HsFun
              (HsPrimType HsPrimCShort)
              (HsIO
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "I")))))),
      foreignImportCRes = TypePointer
        (TypeFun
          [
            TypePrim
              (PrimIntegral PrimShort Signed)]
          (TypeTypedef (CName "I"))),
      foreignImportCArgs = [
        TypePrim
          (PrimIntegral PrimLong Signed)],
      foreignImportOrigName = "bar4",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "bar4",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimLong Signed)],
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypeTypedef (CName "I"))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:24:5"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "baz1",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportCRes = TypePointer
        (TypeConstArray
          2
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportCArgs = [
        TypePrim
          (PrimIntegral PrimInt Signed)],
      foreignImportOrigName = "baz1",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "baz1",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:27:7"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "baz2",
      foreignImportType = HsFun
        (HsTypRef
          (HsName "@NsTypeConstr" "I"))
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsPrimType HsPrimCInt))))),
      foreignImportCRes = TypePointer
        (TypeConstArray
          2
          (TypeConstArray
            3
            (TypePrim
              (PrimIntegral
                PrimInt
                Signed)))),
      foreignImportCArgs = [
        TypeTypedef (CName "I")],
      foreignImportOrigName = "baz2",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "baz2",
          functionArgs = [
            TypeTypedef (CName "I")],
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:35:7"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "baz3",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO
          (HsPtr
            (HsConstArray
              2
              (HsConstArray
                3
                (HsTypRef
                  (HsName
                    "@NsTypeConstr"
                    "I")))))),
      foreignImportCRes = TypePointer
        (TypeConstArray
          2
          (TypeConstArray
            3
            (TypeTypedef (CName "I")))),
      foreignImportCArgs = [
        TypePrim
          (PrimIntegral PrimInt Signed)],
      foreignImportOrigName = "baz3",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName "baz3",
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypeTypedef (CName "I")))),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:43:5"}},
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args_no_void",
      foreignImportType = HsIO
        (HsTypRef
          (HsName "@NsTypeConstr" "I")),
      foreignImportCRes = TypeTypedef
        (CName "I"),
      foreignImportCArgs = [],
      foreignImportOrigName =
      "no_args_no_void",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      ForeignImportDeclOriginFunction
        Function {
          functionName = CName
            "no_args_no_void",
          functionArgs = [],
          functionRes = TypeTypedef
            (CName "I"),
          functionHeader =
          "macro_in_fundecl.h",
          functionSourceLoc =
          "macro_in_fundecl.h:53:3"}}]
