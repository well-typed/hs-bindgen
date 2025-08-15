[
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Toggle",
      newtypeConstr = HsName
        "@NsConstr"
        "Toggle",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Toggle",
        fieldType = HsBlock
          (HsIO (HsPrimType HsPrimCBool)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:3:16",
          declId = NamePair {
            nameC = Name "Toggle",
            nameHsIdent = HsIdentifier
              "Toggle"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Toggle",
              newtypeField = HsName
                "@NsVar"
                "un_Toggle"},
            typedefType = TypeBlock
              (TypeFun
                []
                (TypePrim PrimBool))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "Toggle hs_bindgen_test_iterator_4f34fce61cc68c9f (_Bool arg1) { return makeToggle(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "makeToggle",
      foreignImportType = HsFun
        (HsPrimType HsPrimCBool)
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "Toggle"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_4f34fce61cc68c9f",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim PrimBool],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Toggle",
                nameHsIdent = HsIdentifier
                  "Toggle"})},
      foreignImportComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "_Bool hs_bindgen_test_iterator_bfb4e32e3a824c7e (Toggle arg1) { return toggleNext(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "toggleNext",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Toggle"))
        (HsIO (HsPrimType HsPrimCBool)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_bfb4e32e3a824c7e",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Toggle",
                  nameHsIdent = HsIdentifier
                    "Toggle"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            PrimBool},
      foreignImportComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void hs_bindgen_test_iterator_8d23fba933ba9584 (Toggle arg1) { releaseToggle(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "releaseToggle",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Toggle"))
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_8d23fba933ba9584",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Toggle",
                  nameHsIdent = HsIdentifier
                    "Toggle"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Counter",
      newtypeConstr = HsName
        "@NsConstr"
        "Counter",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Counter",
        fieldType = HsBlock
          (HsIO (HsPrimType HsPrimCInt)),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:10:14",
          declId = NamePair {
            nameC = Name "Counter",
            nameHsIdent = HsIdentifier
              "Counter"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Counter",
              newtypeField = HsName
                "@NsVar"
                "un_Counter"},
            typedefType = TypeBlock
              (TypeFun
                []
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "Counter hs_bindgen_test_iterator_5b455070cb6127b9 (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "makeCounter",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPrimType HsPrimCInt)
          (HsIO
            (HsTypRef
              (HsName
                "@NsTypeConstr"
                "Counter")))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_5b455070cb6127b9",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "Counter",
                nameHsIdent = HsIdentifier
                  "Counter"})},
      foreignImportComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "signed int hs_bindgen_test_iterator_1eb9473844c466c6 (Counter arg1) { return counterNext(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "counterNext",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Counter"))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_1eb9473844c466c6",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Counter",
                  nameHsIdent = HsIdentifier
                    "Counter"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void hs_bindgen_test_iterator_4bd3562b992f2f1c (Counter arg1) { releaseCounter(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "releaseCounter",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "Counter"))
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_4bd3562b992f2f1c",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "Counter",
                  nameHsIdent = HsIdentifier
                    "Counter"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Nothing},
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "VarCounter",
      newtypeConstr = HsName
        "@NsConstr"
        "VarCounter",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_VarCounter",
        fieldType = HsBlock
          (HsFun
            (HsPrimType HsPrimCInt)
            (HsIO (HsPrimType HsPrimCInt))),
        fieldOrigin = GeneratedField,
        fieldComment = Nothing},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:17:14",
          declId = NamePair {
            nameC = Name "VarCounter",
            nameHsIdent = HsIdentifier
              "VarCounter"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h",
          declComment = Nothing},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "VarCounter",
              newtypeField = HsName
                "@NsVar"
                "un_VarCounter"},
            typedefType = TypeBlock
              (TypeFun
                [
                  TypePrim
                    (PrimIntegral PrimInt Signed)]
                (TypePrim
                  (PrimIntegral
                    PrimInt
                    Signed)))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [],
      newtypeComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "VarCounter hs_bindgen_test_iterator_0fc005ef62990438 (signed int arg1) { return makeVarCounter(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "makeVarCounter",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsIO
          (HsTypRef
            (HsName
              "@NsTypeConstr"
              "VarCounter"))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_0fc005ef62990438",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeTypedef
            (TypedefRegular
              NamePair {
                nameC = Name "VarCounter",
                nameHsIdent = HsIdentifier
                  "VarCounter"})},
      foreignImportComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "signed int hs_bindgen_test_iterator_a88cd5c9559b5d52 (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "varCounterNext",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "VarCounter"))
        (HsFun
          (HsPrimType HsPrimCInt)
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_a88cd5c9559b5d52",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "VarCounter",
                  nameHsIdent = HsIdentifier
                    "VarCounter"}),
            TypePrim
              (PrimIntegral PrimInt Signed)],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)},
      foreignImportComment = Nothing},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void hs_bindgen_test_iterator_2d2d26e60eea04a8 (VarCounter arg1) { releaseVarCounter(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "releaseVarCounter",
      foreignImportType = HsFun
        (HsTypRef
          (HsName
            "@NsTypeConstr"
            "VarCounter"))
        (HsIO (HsPrimType HsPrimUnit)),
      foreignImportOrigName =
      "hs_bindgen_test_iterator_2d2d26e60eea04a8",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "VarCounter",
                  nameHsIdent = HsIdentifier
                    "VarCounter"})],
          functionAttrs =
          FunctionAttributes
            ImpureFunction,
          functionRes = TypeVoid},
      foreignImportComment = Nothing}]
