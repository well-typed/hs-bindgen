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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:3:16",
          declId = NamePair {
            nameC = Name "Toggle",
            nameHsIdent = HsIdentifier
              "Toggle"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h"},
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
        []},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "Toggle testmodule_makeToggle (_Bool arg1) { return makeToggle(arg1); }",
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
      "testmodule_makeToggle",
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
                  "Toggle"})}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "_Bool testmodule_toggleNext (Toggle arg1) { return toggleNext(arg1); }",
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
      "testmodule_toggleNext",
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
            PrimBool}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void testmodule_releaseToggle (Toggle arg1) { releaseToggle(arg1); }",
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
      "testmodule_releaseToggle",
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
          functionRes = TypeVoid}},
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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:10:14",
          declId = NamePair {
            nameC = Name "Counter",
            nameHsIdent = HsIdentifier
              "Counter"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h"},
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
        []},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "Counter testmodule_makeCounter (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }",
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
      "testmodule_makeCounter",
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
                  "Counter"})}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "signed int testmodule_counterNext (Counter arg1) { return counterNext(arg1); }",
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
      "testmodule_counterNext",
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
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void testmodule_releaseCounter (Counter arg1) { releaseCounter(arg1); }",
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
      "testmodule_releaseCounter",
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
          functionRes = TypeVoid}},
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
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc = "iterator.h:17:14",
          declId = NamePair {
            nameC = Name "VarCounter",
            nameHsIdent = HsIdentifier
              "VarCounter"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader = "iterator.h"},
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
        []},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "VarCounter testmodule_makeVarCounter (signed int arg1) { return makeVarCounter(arg1); }",
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
      "testmodule_makeVarCounter",
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
                  "VarCounter"})}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "signed int testmodule_varCounterNext (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }",
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
      "testmodule_varCounterNext",
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
            (PrimIntegral PrimInt Signed)}},
  DeclInlineCInclude "iterator.h",
  DeclInlineC
    "void testmodule_releaseVarCounter (VarCounter arg1) { releaseVarCounter(arg1); }",
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
      "testmodule_releaseVarCounter",
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
          functionRes = TypeVoid}}]
