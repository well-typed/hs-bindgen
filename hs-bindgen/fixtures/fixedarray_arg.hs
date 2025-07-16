[
  DeclInlineCInclude
    "fixedarray_arg.h",
  DeclInlineC
    "signed int testmodule_fun_1 (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_1_wrapper",
      foreignImportType = HsFun
        (HsPrimType HsPrimCInt)
        (HsFun
          (HsPtr (HsPrimType HsPrimCInt))
          (HsIO (HsPrimType HsPrimCInt))),
      foreignImportOrigName =
      "testmodule_fun_1",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypePrim
              (PrimIntegral PrimInt Signed),
            TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclSimple,
  DeclNewtype
    Newtype {
      newtypeName = HsName
        "@NsTypeConstr"
        "Triple",
      newtypeConstr = HsName
        "@NsConstr"
        "Triple",
      newtypeField = Field {
        fieldName = HsName
          "@NsVar"
          "un_Triple",
        fieldType = HsConstArray
          3
          (HsPrimType HsPrimCInt),
        fieldOrigin = GeneratedField},
      newtypeOrigin = Decl {
        declInfo = DeclInfo {
          declLoc =
          "fixedarray_arg.h:3:13",
          declId = NamePair {
            nameC = Name "triple",
            nameHsIdent = HsIdentifier
              "Triple"},
          declOrigin = NameOriginInSource,
          declAliases = [],
          declHeader =
          "fixedarray_arg.h"},
        declKind = Typedef
          Typedef {
            typedefNames = NewtypeNames {
              newtypeConstr = HsName
                "@NsConstr"
                "Triple",
              newtypeField = HsName
                "@NsVar"
                "un_Triple"},
            typedefType = TypeConstArray
              3
              (TypePrim
                (PrimIntegral PrimInt Signed))},
        declSpec = DeclSpec
          TypeSpec {
            typeSpecModule = Nothing,
            typeSpecIdentifier = Nothing,
            typeSpecInstances = Map.fromList
              []}},
      newtypeInstances = Set.fromList
        [Eq, Show, Storable]},
  DeclNewtypeInstance
    DeriveNewtype
    Storable
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Eq
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclNewtypeInstance
    DeriveStock
    Show
    (HsName
      "@NsTypeConstr"
      "Triple"),
  DeclInlineCInclude
    "fixedarray_arg.h",
  DeclInlineC
    "signed int testmodule_fun_2 (signed int *arg1) { return fun_2(arg1); }",
  DeclForeignImport
    ForeignImportDecl {
      foreignImportName = HsName
        "@NsVar"
        "fun_2_wrapper",
      foreignImportType = HsFun
        (HsPtr (HsPrimType HsPrimCInt))
        (HsIO (HsPrimType HsPrimCInt)),
      foreignImportOrigName =
      "testmodule_fun_2",
      foreignImportCallConv =
      CallConvUserlandCAPI,
      foreignImportOrigin = Function
        Function {
          functionArgs = [
            TypeTypedef
              (TypedefRegular
                NamePair {
                  nameC = Name "triple",
                  nameHsIdent = HsIdentifier
                    "Triple"})],
          functionRes = TypePrim
            (PrimIntegral PrimInt Signed)}},
  DeclSimple]
