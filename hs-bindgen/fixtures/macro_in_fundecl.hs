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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:5:9",
          declId = NamePair {
            nameC = CName "I",
            nameHsIdent = HsIdentifier "I"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "I",
              newtypeField = HsName
                "@NsVar"
                "un_I"},
            macroType = TypePrim
              (PrimIntegral PrimInt Signed)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable]},
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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:6:9",
          declId = NamePair {
            nameC = CName "C",
            nameHsIdent = HsIdentifier "C"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "C",
              newtypeField = HsName
                "@NsVar"
                "un_C"},
            macroType = TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable]},
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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:7:9",
          declId = NamePair {
            nameC = CName "F",
            nameHsIdent = HsIdentifier "F"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "F",
              newtypeField = HsName
                "@NsVar"
                "un_F"},
            macroType = TypePrim
              (PrimFloating PrimFloat)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Read,
          Show,
          Floating,
          Fractional,
          Num,
          Real,
          RealFloat,
          RealFrac,
          Storable]},
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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:8:9",
          declId = NamePair {
            nameC = CName "L",
            nameHsIdent = HsIdentifier "L"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "L",
              newtypeField = HsName
                "@NsVar"
                "un_L"},
            macroType = TypePrim
              (PrimIntegral PrimLong Signed)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable]},
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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "macro_in_fundecl.h:9:9",
          declId = NamePair {
            nameC = CName "S",
            nameHsIdent = HsIdentifier "S"},
          declOrigin = NameOriginInSource,
          declAliases = []},
        declKind = Macro
          CheckedMacroType {
            macroTypeNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "S",
              newtypeField = HsName
                "@NsVar"
                "un_S"},
            macroType = TypePrim
              (PrimIntegral
                PrimShort
                Signed)},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [
          Eq,
          Ord,
          Enum,
          Ix,
          Bounded,
          Read,
          Show,
          Bits,
          FiniteBits,
          Integral,
          Num,
          Real,
          Storable]},
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
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "char testmodule_quux (F arg1, char arg2) { return quux(arg1, arg2); }",
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
      foreignImportOrigName =
      "testmodule_quux",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeMacroTypedef
              NamePair {
                nameC = CName "F",
                nameHsIdent = HsIdentifier "F"}
              NameOriginInSource,
            TypePrim
              (PrimChar
                (PrimSignImplicit Nothing))],
          functionRes = TypePrim
            (PrimChar
              (PrimSignImplicit Nothing)),
          functionHeader =
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "C *testmodule_wam (float arg1, C *arg2) { return wam(arg1, arg2); }",
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
      foreignImportOrigName =
      "testmodule_wam",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimFloating PrimFloat),
            TypePointer
              (TypeMacroTypedef
                NamePair {
                  nameC = CName "C",
                  nameHsIdent = HsIdentifier "C"}
                NameOriginInSource)],
          functionRes = TypePointer
            (TypeMacroTypedef
              NamePair {
                nameC = CName "C",
                nameHsIdent = HsIdentifier "C"}
              NameOriginInSource),
          functionHeader =
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "char *testmodule_foo1 (float arg1, signed int (*arg2) (signed int arg1)) { return foo1(arg1, arg2); }",
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
      foreignImportOrigName =
      "testmodule_foo1",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
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
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "char *testmodule_foo2 (F arg1, signed int (*arg2) (signed int arg1)) { return foo2(arg1, arg2); }",
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
      foreignImportOrigName =
      "testmodule_foo2",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeMacroTypedef
              NamePair {
                nameC = CName "F",
                nameHsIdent = HsIdentifier "F"}
              NameOriginInSource,
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
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "C *testmodule_foo3 (float arg1, signed int (*arg2) (signed int arg1)) { return foo3(arg1, arg2); }",
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
      foreignImportOrigName =
      "testmodule_foo3",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
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
            (TypeMacroTypedef
              NamePair {
                nameC = CName "C",
                nameHsIdent = HsIdentifier "C"}
              NameOriginInSource),
          functionHeader =
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "signed int (*testmodule_bar1) (signed short arg1) (signed long arg1) { return bar1(arg1); }",
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
      foreignImportOrigName =
      "testmodule_bar1",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
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
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "signed int (*testmodule_bar2) (signed short arg1) (L arg1) { return bar2(arg1); }",
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
      foreignImportOrigName =
      "testmodule_bar2",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeMacroTypedef
              NamePair {
                nameC = CName "L",
                nameHsIdent = HsIdentifier "L"}
              NameOriginInSource],
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypePrim
                (PrimIntegral PrimInt Signed))),
          functionHeader =
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "signed int (*testmodule_bar3) (S arg1) (signed long arg1) { return bar3(arg1); }",
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
      foreignImportOrigName =
      "testmodule_bar3",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimLong Signed)],
          functionRes = TypePointer
            (TypeFun
              [
                TypeMacroTypedef
                  NamePair {
                    nameC = CName "S",
                    nameHsIdent = HsIdentifier "S"}
                  NameOriginInSource]
              (TypePrim
                (PrimIntegral PrimInt Signed))),
          functionHeader =
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "I (*testmodule_bar4) (signed short arg1) (signed long arg1) { return bar4(arg1); }",
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
      foreignImportOrigName =
      "testmodule_bar4",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimLong Signed)],
          functionRes = TypePointer
            (TypeFun
              [
                TypePrim
                  (PrimIntegral PrimShort Signed)]
              (TypeMacroTypedef
                NamePair {
                  nameC = CName "I",
                  nameHsIdent = HsIdentifier "I"}
                NameOriginInSource)),
          functionHeader =
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "signed int *testmodule_baz1[2][3] (signed int arg1) { return baz1(arg1); }",
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
      foreignImportOrigName =
      "testmodule_baz1",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
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
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "signed int *testmodule_baz2[2][3] (I arg1) { return baz2(arg1); }",
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
      foreignImportOrigName =
      "testmodule_baz2",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypeMacroTypedef
              NamePair {
                nameC = CName "I",
                nameHsIdent = HsIdentifier "I"}
              NameOriginInSource],
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
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "I *testmodule_baz3[2][3] (signed int arg1) { return baz3(arg1); }",
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
      foreignImportOrigName =
      "testmodule_baz3",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionRes = TypePointer
            (TypeConstArray
              2
              (TypeConstArray
                3
                (TypeMacroTypedef
                  NamePair {
                    nameC = CName "I",
                    nameHsIdent = HsIdentifier "I"}
                  NameOriginInSource))),
          functionHeader =
          "macro_in_fundecl.h"}},
  DeclInlineCInclude
    "macro_in_fundecl.h",
  DeclInlineC
    "I testmodule_no_args_no_void (void) { return no_args_no_void(); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "no_args_no_void",
      foreignImportType = HsIO
        (HsTypRef
          (HsName "@NsTypeConstr" "I")),
      foreignImportOrigName =
      "testmodule_no_args_no_void",
      foreignImportHeader =
      "macro_in_fundecl.h",
      foreignImportDeclOrigin =
      Function
        Function {
          functionArgs = [],
          functionRes = TypeMacroTypedef
            NamePair {
              nameC = CName "I",
              nameHsIdent = HsIdentifier "I"}
            NameOriginInSource,
          functionHeader =
          "macro_in_fundecl.h"}}]
